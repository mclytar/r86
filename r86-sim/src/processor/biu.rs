// TODO: remove after implementation
#![allow(dead_code)]

pub mod adder;
pub mod queue;

use tokio::sync::{mpsc};

use crate::bus::*;
use self::adder::{Adder, AdderCommand};
use self::queue::InstructionQueue;

struct BIURegisterFile {
    pub cs: u16,
    pub ds: u16,
    pub ss: u16,
    pub es: u16,
    pub ip: u16,
    pub ind: u16,
    pub opr: u16,
    pub queue: InstructionQueue
}
impl BIURegisterFile {
    pub fn new() -> Self {
        BIURegisterFile {
            cs: 0xFFFF,
            ds: 0x0000,
            ss: 0x0000,
            es: 0x0000,
            ip: 0x0000,
            ind: 0x0000,
            opr: 0x0000,
            queue: InstructionQueue::new()
        }
    }
}

pub struct BusInterfaceUnit {
    bus_b: Bus16,
    bus_c: Bus20,
    bus_alu: BusSocket<u16, u16>,
    adder: Adder,
    adder_command: mpsc::Sender<AdderCommand>,
    register_file: BIURegisterFile
}
impl BusInterfaceUnit {
    pub fn new(bus_alu: BusSocket<u16, u16>) -> Self {
        let bus_b = Bus16::new();
        let bus_c = Bus20::new();
        let (adder_command, command) = mpsc::channel(1);
        let adder = Adder::new(command, bus_b.socket(), bus_c.socket_high16());
        let register_file = BIURegisterFile::new();

        BusInterfaceUnit {
            bus_b,
            bus_c,
            bus_alu,
            adder,
            adder_command,
            register_file
        }
    }
}