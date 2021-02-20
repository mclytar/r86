use crate::bus::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum ALUCommand {
    Idle,
    LoadRegA,
    LoadRegB,
    LoadRegC,
    Output
}