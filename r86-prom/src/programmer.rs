use std::io::{Read, Write};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use serialport::{SerialPort, ClearBuffer};
use tokio::sync::{oneshot, watch};
use tokio::task;

use crate::error::{DeviceError, DeviceResult, DeviceErrorKind};
use crate::message::{DeviceRequest, DeviceResponse};

pub struct ProgrammerRequest {
    pub(in self) port: Arc<Mutex<Box<dyn SerialPort>>>,
    pub(in self) req: DeviceRequest,
    pub(in self) payload: Vec<u8>
}
impl ProgrammerRequest {
    pub fn payload(mut self, payload: Vec<u8>) -> Self {
        self.payload = payload;
        self
    }

    pub fn device_request(&self) -> DeviceRequest {
        self.req
    }

    pub fn send(self) -> ProgrammerResponse {
        let (status_tx, status_rx) = watch::channel(ProgrammerResponseStatus::Initialization);
        let (data_tx, data_rx) = oneshot::channel();

        task::spawn(async move {
            let main_status_tx = Arc::new(Mutex::new(status_tx));
            let task_status_tx = main_status_tx.clone();

            let handle = task::spawn(async move {
                let mut port = self.port.lock().unwrap();
                let mut response_buffer = vec![0];
                let status_tx = task_status_tx.lock().unwrap();

                status_tx.send(ProgrammerResponseStatus::SendingRequest).unwrap();

                port.write(&self.req.decode()[..])
                    .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                port.flush()
                    .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                status_tx.send(ProgrammerResponseStatus::ProcessingResponse).unwrap();
                port.read_exact(&mut response_buffer[..])
                    .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;

                let mut response = DeviceResponse::from_response(response_buffer[0]);
                let mut payload = match self.req {
                    DeviceRequest::ReadByte
                    | DeviceRequest::ReadByteAt(_) => {
                        let mut immediate = vec![0; 1];
                        port.read_exact(&mut immediate[..])
                            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                        immediate
                    },
                    DeviceRequest::QueryAddress => {
                        let mut immediate = vec![0; 2];
                        port.read_exact(&mut immediate[..])
                            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                        immediate
                    },
                    _ => Vec::new()
                };
                let mut page = 0;
                let expected_pages = match self.req {
                    DeviceRequest::ReadPage(_)
                    | DeviceRequest::WritePage(_)
                    | DeviceRequest::ProtectedWritePage(_)
                    | DeviceRequest::UnprotectAndWritePage(_) => 1,
                    DeviceRequest::ReadAll
                    | DeviceRequest::WriteAll
                    | DeviceRequest::ProtectedWriteAll
                    | DeviceRequest::UnprotectAndWriteAll => 512,
                    _ => 0
                };

                while let Ok(DeviceResponse::Ready) = response {
                    status_tx.send(ProgrammerResponseStatus::MultipartIO { num: page as u64, len: expected_pages}).unwrap();

                    port.write(&self.payload[page * 64 .. (page + 1) * 64])
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                    port.flush()
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                    port.read_exact(&mut response_buffer[..])
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;

                    response = DeviceResponse::from_response(response_buffer[0]);
                    page += 1;
                }

                while let Ok(DeviceResponse::Multipart) = response {
                    status_tx.send(ProgrammerResponseStatus::MultipartIO { num: page as u64, len: expected_pages}).unwrap();
                    payload.append(&mut vec![0; 64]);

                    port.read_exact(&mut payload[page * 64 .. (page + 1) * 64])
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                    port.write(&vec![0x82][..])
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                    port.flush()
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
                    port.read_exact(&mut response_buffer[..])
                        .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;

                    response = DeviceResponse::from_response(response_buffer[0]);
                    page += 1;
                }

                status_tx.send(ProgrammerResponseStatus::MultipartIO { num: expected_pages, len: expected_pages}).unwrap();
                Ok((response, payload))
            });

            let outbound = match handle.await.unwrap() {
                Ok((response, payload)) => (response, if payload.len() > 0 { Some(payload) } else { None }),
                Err(e) => (Err(e), None)
            };
            let status_tx = main_status_tx.lock().unwrap();

            data_tx.send(outbound).unwrap();
            status_tx.send(ProgrammerResponseStatus::Ready).unwrap();
        });

        ProgrammerResponse {
            status_listener: status_rx,
            data_listener: data_rx
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ProgrammerResponseStatus {
    Initialization,
    SendingRequest,
    MultipartIO { num: u64, len: u64 },
    ProcessingResponse,
    Ready,
}

pub struct ProgrammerResponse {
    pub(in self) status_listener: watch::Receiver<ProgrammerResponseStatus>,
    pub(in self) data_listener: oneshot::Receiver<(DeviceResult<DeviceResponse>, Option<Vec<u8>>)>
}
impl ProgrammerResponse {
    pub fn query(&mut self) -> ProgrammerResponseStatus {
        *self.status_listener.borrow()
    }

    pub async fn resolve(self) -> (DeviceResult<DeviceResponse>, Option<Vec<u8>>) {
        self.data_listener.await.unwrap()
    }
}

pub struct Programmer {
    port: Arc<Mutex<Box<dyn SerialPort>>>
}

impl Programmer {
    pub fn new() -> DeviceResult<Self> {
        let available = match serialport::available_ports() {
            Ok(available) => available,
            Err(e) => {
                return Err(DeviceError::new(DeviceErrorKind::SerialPortError, e));
            }
        };

        let mut port = None;
        for p in available {
            //eprintln!("Testing {} ...", p.port_name);
            // Get the next available port, if possible.
            let mut p = match serialport::new(p.port_name, 38_400)
                .timeout(Duration::from_millis(500))
                .open() {
                Ok(p) => p,
                Err(_e) => {
                    //eprintln!("  Not available: {}", e);
                    continue;
                }
            };
            // Await for possible device reboot.
            std::thread::sleep(Duration::from_millis(10));
            // Try to clear both input and output buffer.
            if let Err(_e) = p.clear(ClearBuffer::All) {
                //eprintln!("  Cannot clear buffer: {}", e);
                continue;
            }
            // Try to send ping.
            let mut buf = vec![0; 3];
            if let Err(_e) = p.write(&DeviceRequest::Ping.decode()[..]) {
                //eprintln!("  Cannot write: {}", e);
                continue;
            }
            if let Err(_e) = p.flush() {
                //eprintln!("  Cannot flush: {}", e);
                continue;
            }
            if let Err(_e) = p.read_exact(&mut buf[..]) {
                //eprintln!("  Cannot read: {}", e);
                continue;
            }
            // Check if ping is correct.
            if buf[0] != 0x00 || buf[1] != 0xAA || buf[2] != 0x55 {
                //eprintln!("  Unexpected data from ping: ({:02X}) {:02X}{:02X}", buf[0], buf[1], buf[2]);
                continue;
            }
            // After all these checks, the correct port should have been found.
            port = Some(p);
        }

        // If no port has been found, return error.
        let port = if let Some(port) = port {
            Arc::new(Mutex::new(port))
        } else {
            return Err(DeviceError::from(DeviceErrorKind::NotDetected));
        };

        Ok(Programmer {
            port
        })
    }

    pub fn request(&mut self, req: DeviceRequest) -> ProgrammerRequest {
        let port = self.port.clone();
        ProgrammerRequest {
            port,
            req,
            payload: Vec::new()
        }
    }

    /*pub async fn send_message(&mut self, req: DeviceRequest) -> DeviceResult<DeviceResponse> {
        let mut req_buffer = req.decode();
        let mut res_buffer = vec![0; 1];

        self.port.write(&req_buffer[..])
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
        self.port.flush()
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
        self.port.read_exact(&mut res_buffer[..])
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;

        DeviceResponse::from_response(res_buffer[0])
    }

    pub async fn send_data(&mut self, buffer: &[u8]) -> DeviceResult<()> {
        self.port.write(buffer)
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;
        self.port.flush()
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))
    }

    pub async fn receive_data(&mut self, buffer: &mut [u8]) -> DeviceResult<()> {
        self.port.read_exact(buffer)
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))
    }

    pub async fn receive_u8(&mut self) -> DeviceResult<u8> {
        let mut buffer = vec![0; 1];
        self.port.read_exact(&mut buffer[..])
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;

        Ok(buffer[0])
    }

    pub async fn receive_u16(&mut self) -> DeviceResult<u16> {
        let mut buffer = vec![0; 2];
        self.port.read_exact(&mut buffer[..])
            .map_err(|e| DeviceError::new(DeviceErrorKind::Timeout, e))?;

        Ok((buffer[0] as u16) | ((buffer[1] as u16) << 8))
    }

    pub fn pages_count(&self) -> u64 {
        512
    }

    pub async fn chip_enable(&mut self) -> DeviceResult<()> {
        self.send_message(DeviceRequest::ChipEnable).await?;
        Ok(())
    }

    pub async fn chip_disable(&mut self) -> DeviceResult<()> {
        self.send_message(DeviceRequest::ChipDisable).await?;
        Ok(())
    }

    pub async fn addr(&mut self) -> DeviceResult<u16> {
        self.send_message(DeviceRequest::QueryAddress).await?;
        self.receive_u16().await
    }

    pub async fn set_addr(&mut self, addr: u16) -> DeviceResult<()> {
        self.send_message(DeviceRequest::SelectAddress(addr)).await?;
        Ok(())
    }

    pub async fn ping(&mut self) -> DeviceResult<()> {
        self.send_message(DeviceRequest::Ping).await?;
        Ok(())
    }

    pub async fn read_byte(&mut self) -> DeviceResult<u8> {
        self.send_message(DeviceRequest::ReadByte).await?;
        self.receive_u8().await
    }

    pub async fn read_byte_at(&mut self, addr: u16) -> DeviceResult<u8> {
        self.send_message(DeviceRequest::ReadByteAt(addr)).await?;
        self.receive_u8().await
    }

    pub async fn read_page(&mut self, page: u16) -> DeviceResult<Vec<u8>> {
        let mut buffer = vec![0; 64];
        self.send_message(DeviceRequest::ReadPage(page)).await?;
        self.receive_data(&mut buffer).await?;
        Ok(buffer)
    }*/

    /*pub async fn read_all(&mut self, tx: Sender<u64>) -> DeviceResult<Vec<u8>> {
        let mut buffer = vec![0; 32768];
        let mut page = 0;
        self.send_message(DeviceRequest::ReadAll).await?;

        loop {
            let response = self.receive_u8().await?;

            match response {
                0x80 => {
                    self.receive_data(&mut buffer[page * 64 .. (page + 1) * 64]).await?;
                    page += 1;
                    tx.send(page as u64);
                },
                0x81 => break,
                _ => Err(DeviceError::from(DeviceErrorKind::UnexpectedResponse))?
            }
        }

        Ok(buffer)
    }*/

    /*pub fn read_all(&mut self, bar: Option<&ProgressBar>) -> Result<Vec<u8>> {
        let mut response = vec![0; 1];
        let mut buffer = vec![0; 32768];
        let mut size = 0;

        self.port.write(&[0x13]).expect("Device not ready.");
        self.port.flush().unwrap();

        if let Some(bar) = bar {
            bar.set_length(512);
        }

        loop {
            self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");

            match response[0] {
                0x80 => {
                    self.port.read_exact(&mut buffer[size..size + 64]).expect("Device does not respond");
                    self.port.write(&[0x82]).expect("Device not ready.");
                    self.port.flush().unwrap();
                    size += 64;
                    if let Some(bar) = bar {
                        bar.inc(1);
                    }
                },
                0x81 => break,
                _ => DeviceError::from_device_error_code(response[0], 0x13)?
            };
        }

        self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
        DeviceError::from_device_error_code(response[0], 0x13)?;

        Ok(buffer)
    }

    pub fn write_byte(&mut self, value: u8) -> Result<()> {
        let mut buffer = vec![0; 2];

        self.port.write(&[
            0x20,
            value
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut buffer[0..1]).expect("Device does not respond.");

        DeviceError::from_device_error_code(buffer[0], 0x20)
    }

    pub fn write_byte_at(&mut self, addr: u16, value: u8) -> Result<()> {
        let mut buffer = vec![0; 2];

        self.port.write(&[
            0x21,
            (addr & 0xFF) as u8,
            (addr >> 8) as u8,
            value
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut buffer[0..1]).expect("Device does not respond.");

        DeviceError::from_device_error_code(buffer[0], 0x20)
    }

    pub fn write_page(&mut self, page: u16, contents: &[u8]) -> Result<()> {
        let mut response = vec![0; 1];

        if contents.len() != 64 { panic!("Length must be 64 bytes"); }

        self.port.write(&[
            0x22,
            (page & 0xFF) as u8,
            (page >> 8) as u8
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");

        match response[0] {
            0x82 => {
                self.port.write(contents).expect("Device not ready.");
                self.port.flush().unwrap();
                self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
            },
            0x83 => {
                self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
            },
            _ => DeviceError::from_device_error_code(response[0], 0x13)?
        };

        DeviceError::from_device_error_code(response[0], 0x20)
    }

    pub fn protected_write_byte(&mut self, value: u8) -> Result<()> {
        let mut buffer = vec![0; 2];

        self.port.write(&[
            0x24,
            value
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut buffer[0..1]).expect("Device does not respond.");

        DeviceError::from_device_error_code(buffer[0], 0x24)
    }

    pub fn protected_write_byte_at(&mut self, addr: u16, value: u8) -> Result<()> {
        let mut buffer = vec![0; 2];

        self.port.write(&[
            0x25,
            (addr & 0xFF) as u8,
            (addr >> 8) as u8,
            value
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut buffer[0..1]).expect("Device does not respond.");

        DeviceError::from_device_error_code(buffer[0], 0x24)
    }

    pub fn protected_write_page(&mut self, page: u16, contents: &[u8]) -> Result<()> {
        let mut response = vec![0; 1];

        if contents.len() != 64 { panic!("Length must be 64 bytes"); }

        self.port.write(&[
            0x26,
            (page & 0xFF) as u8,
            (page >> 8) as u8
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");

        match response[0] {
            0x82 => {
                self.port.write(contents).expect("Device not ready.");
                self.port.flush().unwrap();
                self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
            },
            0x83 => {
                self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
            },
            _ => DeviceError::from_device_error_code(response[0], 0x13)?
        };

        DeviceError::from_device_error_code(response[0], 0x20)
    }

    pub fn unprotect_and_write_byte(&mut self, value: u8) -> Result<()> {
        let mut buffer = vec![0; 2];

        self.port.write(&[
            0x28,
            value
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut buffer[0..1]).expect("Device does not respond.");

        DeviceError::from_device_error_code(buffer[0], 0x28)
    }

    pub fn unprotect_and_write_byte_at(&mut self, addr: u16, value: u8) -> Result<()> {
        let mut buffer = vec![0; 2];

        self.port.write(&[
            0x29,
            (addr & 0xFF) as u8,
            (addr >> 8) as u8,
            value
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut buffer[0..1]).expect("Device does not respond.");

        DeviceError::from_device_error_code(buffer[0], 0x28)
    }

    pub fn unprotect_and_write_page(&mut self, page: u16, contents: &[u8]) -> Result<()> {
        let mut response = vec![0; 1];

        if contents.len() != 64 { panic!("Length must be 64 bytes"); }

        self.port.write(&[
            0x2A,
            (page & 0xFF) as u8,
            (page >> 8) as u8
        ]).expect("Device not ready.");
        self.port.flush().unwrap();
        self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");

        match response[0] {
            0x82 => {
                self.port.write(contents).expect("Device not ready.");
                self.port.flush().unwrap();
                self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
            },
            0x83 => {
                self.port.read_exact(&mut response[0..1]).expect("Device does not respond.");
            },
            _ => DeviceError::from_device_error_code(response[0], 0x13)?
        };

        DeviceError::from_device_error_code(response[0], 0x20)
    }*/
}