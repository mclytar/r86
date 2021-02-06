use tokio::sync::{mpsc};

use crate::bus::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum ALUCommand {
    Idle,
    LoadRegA,
    LoadRegB,
    LoadRegC,
    Output
}

pub struct ArithmeticLogicUnit {
    command: mpsc::Receiver<ALUCommand>,
    bus_alu: Bus16
}