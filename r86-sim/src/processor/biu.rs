pub mod adder;
pub mod queue;

use std::sync::Arc;

use tokio::sync::{Mutex};

use crate::bus::*;
use self::adder::{Adder, AdderControl};
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

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum AdderBusB {
    ALU,
    RegFile
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum AdderBusC {
    ALU,
    ALUOut,
    RegFile,
    RegFileLo4,
    Out,
    OutHi4
}

pub struct BusInterfaceUnit {
    bus_b: Bus<u16, Socket16, AdderBusB>,
    bus_c: Bus<u16, Socket20, AdderBusC>,
    reg_b: Arc<Mutex<u16>>,
    reg_c: Arc<Mutex<u16>>,
    reg_c_lo4: Arc<Mutex<u16>>,
    //bus_alu: BusSocket<u16, u16>,
    adder: Adder,
    adder_ctrl: Arc<Mutex<AdderControl>>,
    register_file: BIURegisterFile,
    //ctrl: ???
}
impl BusInterfaceUnit {
    pub fn new() -> Self {
        // Create buses.
        let mut bus_b = Bus::new();
        let mut bus_c = Bus::new();
        // Create bus connections.
        let reg_b = Arc::new(Mutex::new(0));
        let reg_c = Arc::new(Mutex::new(0));
        let reg_c_lo4 = Arc::new(Mutex::new(0));
        // Create adder.
        let adder = Adder::new();
        let adder_ctrl = adder.control_line();
        // Plug components into buses.
        bus_b.plug(AdderBusB::ALU, adder.socket_b());
        bus_b.plug(AdderBusB::RegFile, Socket16::new(reg_b.clone()));
        bus_c.plug(AdderBusC::ALU, adder.socket_c());
        bus_c.plug(AdderBusC::RegFile, Socket20::new_high(reg_c.clone()));
        bus_c.plug(AdderBusC::RegFileLo4, Socket20::new_low_nibble(reg_c_lo4.clone()));
        // Create register file.
        let register_file = BIURegisterFile::new();

        BusInterfaceUnit {
            bus_b,
            bus_c,
            reg_b,
            reg_c,
            reg_c_lo4,
            //bus_alu,
            adder,
            adder_ctrl,
            register_file
        }
    }
}