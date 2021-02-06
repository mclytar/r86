use tokio::sync::{mpsc};
use tokio::task;
use tokio::task::JoinHandle;

use crate::bus::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum AdderCommand {
    Idle,
    LoadFromBus,
    LoadFromMux(i16),
    Output
}

pub struct Adder {
    command: mpsc::Receiver<AdderCommand>,
    bus_b: BusSocket<u16, u16>,
    bus_c: BusSocket<u32, u16>,
}
impl Adder {
    pub fn new(command: mpsc::Receiver<AdderCommand>, bus_b: BusSocket<u16, u16>, bus_c: BusSocket<u32, u16>) -> Self {
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
                        tmp_b = bus_b.read().await;
                        tmp_c = bus_c.read().await;
                    },
                    AdderCommand::LoadFromMux(val) => {
                        tmp_b = bus_b.read().await;
                        tmp_c = val as u16;
                    },
                    AdderCommand::Output => {
                        bus_c.write(tmp_out).await;
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

    use crate::processor::biu::adder::{Adder, AdderCommand};
    use crate::bus::{Bus16, Bus20, Bus};

    #[tokio::test]
    async fn compose_address() {
        let bus_b = Bus16::new();
        let bus_c = Bus20::new();
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.socket(), bus_c.socket_high16());
        let handle = adder.run();

        let soc_b = bus_b.socket();
        let soc_c = bus_c.socket();

        soc_b.write(0xB800).await;
        soc_c.write(0x00004).await;
        tx.send(AdderCommand::LoadFromBus).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(soc_c.read().await, 0xB8004);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn increment_instruction_pointer() {
        let bus_b = Bus16::new();
        let bus_c = Bus20::new();
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.socket(), bus_c.socket_high16());
        let handle = adder.run();

        let soc_b = bus_b.socket();
        let soc_c = bus_c.socket_high16();

        soc_b.write(0x0100).await;
        tx.send(AdderCommand::LoadFromMux(2)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(soc_c.read().await, 0x0102);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn decrement_instruction_pointer() {
        let bus_b = Bus16::new();
        let bus_c = Bus20::new();
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.socket(), bus_c.socket_high16());
        let handle = adder.run();

        let soc_b = bus_b.socket();
        let soc_c = bus_c.socket_high16();

        soc_b.write(0x0100).await;
        tx.send(AdderCommand::LoadFromMux(-2)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(soc_c.read().await, 0x00FE);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn overflow_increment_instruction_pointer() {
        let bus_b = Bus16::new();
        let bus_c = Bus20::new();
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.socket(), bus_c.socket_high16());
        let handle = adder.run();

        let soc_b = bus_b.socket();
        let soc_c = bus_c.socket_high16();

        soc_b.write(0xFFFE).await;
        tx.send(AdderCommand::LoadFromMux(4)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(soc_c.read().await, 0x0002);

        std::mem::drop(tx);
        handle.await.unwrap();
    }

    #[tokio::test]
    async fn overflow_decrement_instruction_pointer() {
        let bus_b = Bus16::new();
        let bus_c = Bus20::new();
        let (tx, rx) = mpsc::channel(1);
        let adder = Adder::new(rx, bus_b.socket(), bus_c.socket_high16());
        let handle = adder.run();

        let soc_b = bus_b.socket();
        let soc_c = bus_c.socket_high16();

        soc_b.write(0x0002).await;
        tx.send(AdderCommand::LoadFromMux(-4)).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;

        tx.send(AdderCommand::Output).await.unwrap();
        time::sleep(time::Duration::from_millis(1)).await;
        assert_eq!(soc_c.read().await, 0xFFFE);

        std::mem::drop(tx);
        handle.await.unwrap();
    }
}