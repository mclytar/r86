pub mod bootloader;
pub mod executable;
pub mod firmware;

use crate::result::prelude::*;
use crate::compiler::binary::BinaryModule;

pub struct Binary {
    log: CompilerLog,
    contents: Vec<u8>
}
impl Binary {
    pub fn from_raw_parts(log: CompilerLog, contents: Vec<u8>) -> Binary {
        Binary { log, contents }
    }

    pub fn warnings(&self) -> &CompilerLog {
        &self.log
    }
}
impl AsRef<[u8]> for Binary {
    fn as_ref(&self) -> &[u8] {
        &self.contents[..]
    }
}

pub trait Linker {
    fn link(&mut self, binary: BinaryModule) -> Result<(), CompilerLog>;
    fn into_binary(self) -> Binary;
}