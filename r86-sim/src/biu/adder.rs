use std::sync::Arc;

use tokio::sync::{mpsc, Mutex};
use tokio::task;
use tokio::task::JoinHandle;

use crate::biu::Bus;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum AdderCommand {
    Idle,
    LoadFromBus,
    LoadFromMux(i16),
    Output
}

pub struct Adder {
    command: mpsc::Receiver<AdderCommand>,
    bus_b: Bus<u16>,
    bus_c: Bus<u32>,
}
impl Adder {
    pub fn new(command: mpsc::Receiver<AdderCommand>, bus_b: Arc<Mutex<u16>>, bus_c: Arc<Mutex<u32>>) -> Self {
        Adder {
            command,
            bus_b,
            bus_c
        }
    }

    pub fn run(self) -> JoinHandle<()> {
        task::spawn(async move {
            let mut command = self.command;
            let bus_b = self.bus_b;
            let bus_c = self.bus_c;

            let mut tmp_b: u16 = 0;
            let mut tmp_c: u16 = 0;
            let mut tmp_out: u16 = 0;

            while let Some(command) = command.recv().await {
                match command {
                    AdderCommand::Idle => {},
                    AdderCommand::LoadFromBus => {
                        tmp_b = *bus_b.lock().await;
                        tmp_c = (*bus_c.lock().await >> 4) as u16;
                    },
                    AdderCommand::LoadFromMux(val) => {
                        tmp_b = *bus_b.lock().await;
                        tmp_c = val as u16;
                    },
                    AdderCommand::Output => {
                        let mut lock = bus_c.lock().await;
                        *lock = ((tmp_out as u32) << 4) | (*lock & 0xF);
                    }
                }

                tmp_out = tmp_b.overflowing_add(tmp_c).0;
            }
        })
    }
}



#[cfg(test)]
mod test {
    use std::sync::Arc;

    use tokio::sync::{mpsc};
    use tokio::sync::Mutex;
    use tokio::time;

    use crate::biu::adder::{Adder, AdderCommand};

    #[tokio::test]
    async fn compose_address() {
        let bus_b = Arc::new(Mutex::new(0));
        let bus_c = Arc::new(Mutex::new(0));
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.clone(), bus_c.clone());
        let handle = adder.run();

        *bus_b.lock().await = 0xB800;
        *bus_c.lock().await = 0x0004;
        tx.send(AdderCommand::LoadFromBus).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(*bus_c.lock().await, 0xB8004);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn increment_instruction_pointer() {
        let bus_b = Arc::new(Mutex::new(0));
        let bus_c = Arc::new(Mutex::new(0));
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.clone(), bus_c.clone());
        let handle = adder.run();

        *bus_b.lock().await = 0x0100;
        tx.send(AdderCommand::LoadFromMux(2)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(*bus_c.lock().await >> 4, 0x0102);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn decrement_instruction_pointer() {
        let bus_b = Arc::new(Mutex::new(0));
        let bus_c = Arc::new(Mutex::new(0));
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.clone(), bus_c.clone());
        let handle = adder.run();

        *bus_b.lock().await = 0x0100;
        tx.send(AdderCommand::LoadFromMux(-2)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(*bus_c.lock().await >> 4, 0x00FE);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn overflow_increment_instruction_pointer() {
        let bus_b = Arc::new(Mutex::new(0));
        let bus_c = Arc::new(Mutex::new(0));
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.clone(), bus_c.clone());
        let handle = adder.run();

        *bus_b.lock().await = 0xFFFE;
        tx.send(AdderCommand::LoadFromMux(4)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(*bus_c.lock().await >> 4, 0x0002);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn overflow_decrement_instruction_pointer() {
        let bus_b = Arc::new(Mutex::new(0));
        let bus_c = Arc::new(Mutex::new(0));
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.clone(), bus_c.clone());
        let handle = adder.run();

        *bus_b.lock().await = 0x0002;
        tx.send(AdderCommand::LoadFromMux(-4)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(*bus_c.lock().await >> 4, 0xFFFE);

        std::mem::drop(tx);
        handle.await.unwrap();
    }
}