//! Utilities for the motherboard firmware (BIOS) linker.
//!
//! The firmware linker needs a binary of exactly 65520 bytes and a global symbol `_start`,
//! and produces a binary of exactly 65536 bytes.
//!
//! The last 16 bytes of the final executable are dedicated to a near (possibly short) jump to
//! `_start` and, eventually, a 3-word version number.

use std::path::PathBuf;

use crate::result::prelude::*;
use crate::compiler::binary::BinaryModule;
use super::Linker;
use crate::linker::Binary;

pub struct FirmwareLinker {
    log: Option<CompilerLog>,
    contents: Vec<u8>
}
impl FirmwareLinker {
    pub fn new() -> FirmwareLinker {
        FirmwareLinker { log: None, contents: Vec::new() }
    }
}
impl Linker for FirmwareLinker {
    fn link(&mut self, binary: BinaryModule) -> Result<(), CompilerLog> {
        let BinaryModule { log, sections, global_vars, extern_vars, ..} = binary;
        let mut log = log.into_inner();
        if sections.len() > 1 {
            for sec in &sections[1..] {
                log.err(Notification::error_linker_no_sections_allowed("8086-firmware", &sec));
            }
        }
        for var in &extern_vars {
            log.err(Notification::error_linker_undefined_reference(var.name(), &var));
        }
        for var in &global_vars {
            log.warn(Notification::warning_unnecessary_global("8086-firmware", &var));
        }
        let mut contents = Vec::with_capacity(65536);

        for opcode in sections[0].opcodes() {
            for byte in opcode.payload().as_slice() {
                contents.push(*byte);
            }
        }

        if contents.len() > 65520 {
            log.err(Notification::error_linker_binary_too_big(65520, contents.len()));
        }

        if let Some(start_offset) = sections[0].vars().iter()
            .find(|l| l.name() == "_start") {
            let offset = ((start_offset.offset() - 65520) & 0xFFFF) as u16;
            if offset < 0x82 || offset >= 0xFF82 {
                let offset = (offset.overflowing_sub(2).0 & 0xFF) as u8;
                contents.push(0xEB);
                contents.push(offset);
            } else {
                let offset = offset - 3;
                contents.push(0xE9);
                contents.push((offset & 0xFF) as u8);
                contents.push((offset >> 8) as u8);
            }
        } else {
            log.err(Notification::error_linker_no_start());
        }

        if log.is_err() {
            Err(log)
        } else {
            self.log = Some(log);
            self.contents = contents;
            Ok(())
        }
    }

    fn into_binary(self) -> Binary {
        Binary::from_raw_parts(self.log.unwrap(), self.contents)
    }
}