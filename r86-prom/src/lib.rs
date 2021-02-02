pub mod cli;
pub mod error;
pub mod message;
pub mod programmer;
pub mod task;

/*pub fn find_port(options: &cli::Options) -> serialport::Result<Box<dyn serialport::SerialPort>> {
    if let Some(ref port) = options.port {
        if options.verbose > 1 { println!("Probing port '{}'... ", port); }
        let mut port = serialport::new(port, 9_600)
            .timeout(Duration::from_millis(500))
            .open()?;

        if options.verbose > 2 { println!("  Port opened correctly."); }

        port.write(&[0x42])?;

        if options.verbose > 2 { println!("  Written command 'PING', awaiting for response..."); }

        let mut serial_buf: Vec<u8> = vec![0; 2];
        port.read(serial_buf.as_mut_slice())?;
        let response: u16 = (serial_buf[0] as u16) | ((serial_buf[1] as u16) << 8);

        if options.verbose > 2 { println!("  Response received (should be '0x55AA'): '{:#X}'", response); }

        if response != 0x55AA {
            if options.verbose > 1 { println!("Unexpected output from device or wrong port selected."); }
            Err(serialport::Error::new(serialport::ErrorKind::InvalidInput, "Unexpected output from device or wrong port selected."))
        } else {
            if options.verbose > 1 { println!("Success!"); }
            std::thread::sleep(Duration::from_millis(100));
            Ok(port)
        }
    } else {
        if options.verbose > 1 { println!("Searching programmer port..."); }

        let ports = serialport::available_ports()?;
        for p in ports {
            if options.verbose > 2 { println!(" - Testing port '{}'...", p.port_name); }

            let port = serialport::new(&p.port_name, 9_600)
                .timeout(Duration::from_millis(500))
                .open();
            if port.is_err() {
                if options.verbose > 2 { println!("   Cannot open '{}', moving on...", p.port_name); }
                continue;
            }

            if options.verbose > 2 { println!("   Port opened correctly."); }
            let mut port = port.unwrap();

            if port.write(&[0x42]).is_err() {
                if options.verbose > 2 { println!("   Cannot write to '{}', moving on...", p.port_name); }
                continue;
            }
            if options.verbose > 2 { println!("   Written command 'PING', awaiting for response..."); }

            let mut serial_buf: Vec<u8> = vec![0; 2];
            if port.read(serial_buf.as_mut_slice()).is_err() {
                if options.verbose > 2 { println!("   Cannot read from '{}', moving on...", p.port_name); }
                continue;
            }
            let response: u16 = (serial_buf[0] as u16) | ((serial_buf[1] as u16) << 8);

            if options.verbose > 2 { println!("   Response received (should be '0x55AA'): '{:#X}'", response); }

            if response != 0x55AA {
                if options.verbose > 2 { println!("   Unexpected output, moving on..."); }
                continue;
            } else {
                if options.verbose > 1 { println!("Port found: '{}'", p.port_name); }
                std::thread::sleep(Duration::from_millis(100));
                return Ok(port)
            }
        }

        if options.verbose > 1 { println!("Could not find a valid port."); }
        Err(serialport::Error::new(serialport::ErrorKind::NoDevice, "Could not find a valid port."))
    }
}

pub fn read(options: &cli::Options, read_options: &cli::Read, mut port: Box<dyn serialport::SerialPort>) -> serialport::Result<Vec<u8>> {
    let mut package: Vec<u8> = if let Some(a) = read_options.address {
        vec![
            0x14,
            (a & 0xFF) as u8,
            (a >> 8) as u8
        ]
    } else {
        vec![0x10]
    };

    let mut buffer: Vec<u8> = if read_options.byte {
        package[0] |= 0x01;
        vec![0; 1]
    } else {
        package[0] |= 0x02;
        vec![0; 64]
    };

    if options.verbose > 2 { println!("Constructed package: {:#X?}.", package); }

    if let Err(e) = port.write(&package[..]) {
        if options.verbose > 1 { println!("  Cannot write to '{}'.", port.name().unwrap()); }
        return Err(serialport::Error::from(e));
    }
    if let Err(e) = port.read(buffer.as_mut_slice()) {
        if options.verbose > 1 { println!("  Cannot read from '{}'.", port.name().unwrap()); }
        return Err(serialport::Error::from(e));
    }

    if options.verbose > 1 { println!("Received {} byte(s) of data.", buffer.len()); }

    Ok(buffer)
}

pub fn await_for_flush() {
    std::thread::sleep(Duration::from_millis(100));
}*/