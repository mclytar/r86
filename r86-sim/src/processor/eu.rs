// TODO: remove after implementation
#![allow(dead_code)]

pub mod alu;
pub mod microcode;

use crate::bus::*;

struct EURegisterFile {
    pub ax: u16,
    pub cx: u16,
    pub dx: u16,
    pub bx: u16,
    pub sp: u16,
    pub bp: u16,
    pub si: u16,
    pub di: u16,
    pub flags: u16
}
impl EURegisterFile {
    pub fn new() -> Self {
        EURegisterFile {
            ax: 0x0000,
            cx: 0x0000,
            dx: 0x0000,
            bx: 0x0000,
            sp: 0x0000,
            bp: 0x0000,
            si: 0x0000,
            di: 0x0000,
            flags: 0x0000
        }
    }
}

pub struct ExecutionUnit {
    bus_alu: Bus16,
    register_file: EURegisterFile
}