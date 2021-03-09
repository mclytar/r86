pub mod operand;

use std::collections::HashMap;

use regex::Regex;

use operand::{NumericOption, Operand};

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Instruction {
    label: Option<String>,
    prefix: Option<String>,
    name: String,
    operands: Vec<Operand>
}
impl Instruction {
    pub fn from_str<S>(s: S) -> Instruction where
        S: AsRef<str>
    {
        let re_label = Regex::new(r"^(?P<label>(?:\.|[A-Za-z_])[A-Za-z0-9_]*):").unwrap();

        let prefixes = ["lock", "rep", "repne", "repnz", "repe", "repz"];

        let mut line = if let Some(pos) = s.as_ref().find(';') {
            s.as_ref()[..pos].trim()
        } else {
            s.as_ref().trim()
        };

        let label = if let Some(capture) = re_label.captures(line) {
            if let Some(l) = capture.name("label") {
                line = line[l.range().end + 1..].trim();
                Some(l.as_str().to_owned())
            } else {
                None
            }
        } else {
            None
        };

        let mut name = line.split_whitespace().next().unwrap().to_lowercase();

        let prefix = if let Some(i) = prefixes.iter().position(|s| *s == &name[..]) {
            let prefix = name.clone();
            if let Some(n) = line[prefixes[i].len()..].trim().split_whitespace().next() {
                line = line[prefixes[i].len()..].trim();
                name = n.to_lowercase();
                Some(prefix)
            } else {
                name = prefix;
                None
            }
        } else {
            None
        };

        let operands = line[name.len()..].trim().replace(' ', "").to_owned();
        let operands = if operands.len() > 0 {
            operands.split(',').map(|s| Operand::from_str(s)).collect()
        } else {
            Vec::new()
        };

        Instruction {
            label,
            prefix,
            name,
            operands
        }
    }

    pub fn to_opcode_ignore_labels(&self) -> Vec<u8> {
        let alu_instructions = ["add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"];
        let stack_instructions = ["push", "pop"];

        let prefix = self.prefix.as_ref().map(|s| match &s[..] {
            "lock" => 0xF0,
            "repne" | "repnz" => 0xF2,
            "rep" | "repe" | "repz" => 0xF3,
            _ => unreachable!()
        });

        let mut no_opr_result = match &self.name.to_lowercase()[..] {
            "daa" => vec![0x27],
            "das" => vec![0x2F],
            "aaa" => vec![0x37],
            "aas" => vec![0x3F],
            "nop" => vec![0x90],
            "cbw" => vec![0x98],
            "cwd" => vec![0x99],
            "wait" => vec![0x9B],
            "pushf" => vec![0x9C],
            "popf" => vec![0x9D],
            "sahf" => vec![0x9E],
            "lahf" => vec![0x9F],
            "movsb" => vec![0xA4],
            "movsw" => vec![0xA5],
            "cmpsb" => vec![0xA6],
            "cmpsw" => vec![0xA7],
            "stosb" => vec![0xAA],
            "stosw" => vec![0xAB],
            "lodsb" => vec![0xAC],
            "lodsw" => vec![0xAD],
            "scasb" => vec![0xAE],
            "scasw" => vec![0xAF],
            "into" => vec![0xCE],
            "iret" => vec![0xCF],
            "aam" => vec![0xD4, 0x0A],
            "aad" => vec![0xD5, 0x0A],
            "xlat" => vec![0xD7],
            "lock" => vec![0xF0],
            "repne" | "repnz" => vec![0xF2],
            "rep" | "repe" | "repz" => vec![0xF3],
            "hlt" => vec![0xF4],
            "cmc" => vec![0xF5],
            "clc" => vec![0xF8],
            "stc" => vec![0xF9],
            "cli" => vec![0xFA],
            "sti" => vec![0xFB],
            "cld" => vec![0xFC],
            "std" => vec![0xFD],
            _ => { Vec::new() }
        };

        if no_opr_result.len() > 0 {
            if self.operands.len() > 0 {
                panic!("Instruction '{}' accepts 0 arguments, {} given.", &self.name, self.operands.len());
            } else {
                return if let Some(prefix) = prefix {
                    let mut result = vec![prefix];
                    result.append(&mut no_opr_result);
                    result
                } else {
                    no_opr_result
                };
            }
        }


        if let Some(i) = alu_instructions.iter().position(|s| *s == &self.name[..]) {
            let mut opcode = (i << 3) as u8;
            if self.operands.len() != 2 {
                panic!("Instruction '{}' needs 2 arguments, {} given.", alu_instructions[i], self.operands.len());
            }
            if self.operands[0].read_only() {
                panic!("Invalid destination operand for instruction '{}'.", alu_instructions[i]);
            }
            if self.operands[0].is_segment_register() || self.operands[1].is_segment_register() {
                panic!("Cannot perform arithmetic/logic operations with segment registers.");
            }
            if (self.operands[0].forces_byte() && self.operands[1].forces_word())
                || (self.operands[0].forces_word() && self.operands[1].forces_byte()) {
                panic!("Operand sizes do not match.");
            }
            let word = if self.operands[0].forces_byte() || self.operands[1].forces_byte() {
                false
            } else if self.operands[0].forces_word() || self.operands[1].forces_word() {
                true
            } else {
                panic!("Unknown operands size.");
            };

            let mut result = Vec::new();

            if let Some(prefix) = prefix {
                result.push(prefix);
            }

            if self.operands[0].is_register() && self.operands[0].mode_number() == Some(0) && self.operands[1].is_immediate() {
                opcode |= if word { 0b00000101 } else { 0b00000100 };
                result.push(opcode);
                match self.operands[1] {
                    Operand::Immediate(v) => {
                        result.push((v & 0xFF) as u8);
                        if word { result.push((v >> 8) as u8); }
                    },
                    Operand::ImmediateLabel(_) => {
                        result.push(0);
                        if word { result.push(0); }
                    },
                    _ => unreachable!()
                }
            } else if self.operands[0].is_register() && self.operands[1].is_register() {
                opcode |= if word { 0b00000001 } else { 0b00000000 };

                let mode = 0xC0 | (self.operands[1].mode_number().unwrap() << 3) | self.operands[0].mode_number().unwrap();

                result.push(opcode);
                result.push(mode);
            } else if (self.operands[0].is_register() && self.operands[1].is_address())
                || (self.operands[0].is_address() && self.operands[1].is_register()) {
                opcode |= if word { 0b00000001 } else { 0b00000000 };

                let reg_i = if self.operands[0].is_register() { 0 } else { 1 };
                let addr_i = 1 - reg_i;

                opcode |= if reg_i == 0 { 0b00000010 } else { 0b00000000 };

                match self.operands[addr_i] {
                    Operand::RegisterMemory { segment_override, mode_number, displacement: NumericOption::None, .. } => {
                        let mode = mode_number | (self.operands[reg_i].mode_number().unwrap() << 3);

                        if let Some(o) = segment_override {
                            result.push(0b00100110 | (o << 3))
                        }

                        result.push(opcode);
                        result.push(mode);
                    },
                    Operand::RegisterMemory { segment_override, mode_number, displacement: NumericOption::Number(n), ..} => if n > 255 {
                        let mode = mode_number | (self.operands[reg_i].mode_number().unwrap() << 3) | 0x80;

                        if let Some(o) = segment_override {
                            result.push(0b00100110 | (o << 3))
                        }

                        result.push(opcode);
                        result.push(mode);
                        result.push((n & 0xFF) as u8);
                        result.push((n >> 8) as u8);
                    } else {
                        let mode = mode_number | (self.operands[reg_i].mode_number().unwrap() << 3) | 0x40;

                        if let Some(o) = segment_override {
                            result.push(0b00100110 | (o << 3))
                        }

                        result.push(opcode);
                        result.push(mode);
                        result.push((n & 0xFF) as u8);
                    },
                    Operand::RegisterMemory { segment_override, mode_number, displacement: NumericOption::Label(_), ..} => {
                        let mode = mode_number | (self.operands[reg_i].mode_number().unwrap() << 3) | 0x80;

                        if let Some(o) = segment_override {
                            result.push(0b00100110 | (o << 3))
                        }

                        result.push(opcode);
                        result.push(mode);
                        result.push(0);
                        result.push(0);
                    },
                    _ => unreachable!()
                }
            }

            result
        } else if let Some(i) = stack_instructions.iter().position(|s| *s == &self.name[..]) {
            unimplemented!()
        } else {
            unimplemented!()
        }
    }

    pub fn to_opcode(&self, labels: HashMap<String, u16>) -> Vec<u8> {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use crate::instruction::{Instruction};
    use crate::instruction::operand::{Operand, NumericOption};

    #[test]
    pub fn operands() {
        assert_eq!(Instruction::from_str("nop"), Instruction { label: None, prefix: None, name: "nop".to_owned(), operands: vec![] });
        assert_eq!(Instruction::from_str("inc ax"), Instruction { label: None, prefix: None, name: "inc".to_owned(), operands: vec![Operand::Reg16(0)] });
        assert_eq!(Instruction::from_str("inc al"), Instruction { label: None, prefix: None, name: "inc".to_owned(), operands: vec![Operand::Reg8(0)] });
        assert_eq!(Instruction::from_str("add es:w.[bx + si + 8], ax"), Instruction { label: None, prefix: None, name: "add".to_owned(), operands: vec![
            Operand::RegisterMemory { segment_override: Some(0), word_flag: Some(true), mode_number: 0, displacement: NumericOption::Number(8) },
            Operand::Reg16(0)
        ] });
        assert_eq!(Instruction::from_str("ENDLESS_LOOP: jmp ENDLESS_LOOP ; this instruction halts the execution"),
            Instruction { label: Some("ENDLESS_LOOP".to_owned()), prefix: None, name: "jmp".to_owned(), operands: vec![Operand::ImmediateLabel("ENDLESS_LOOP".to_owned())]});
        assert_eq!(Instruction::from_str(".MUTEX: lock add ds:b.[bp + di + 144], ah"), Instruction { label: Some(".MUTEX".to_owned()), prefix: Some("lock".to_owned()), name: "add".to_owned(), operands: vec![
            Operand::RegisterMemory { segment_override: Some(3), word_flag: Some(false), mode_number: 3, displacement: NumericOption::Number(144) },
            Operand::Reg8(4)
        ] });
    }

    #[test]
    pub fn arithmetic_logic_instruction() {
        assert_eq!(Instruction::from_str("add ax, 0x0123").to_opcode_ignore_labels(), vec![0x05, 0x23, 0x01]);
        assert_eq!(Instruction::from_str("or al, 0x01").to_opcode_ignore_labels(), vec![0x0C, 0x01]);
        assert_eq!(Instruction::from_str("adc bl, al").to_opcode_ignore_labels(), vec![0x10, 0xC3]);
        assert_eq!(Instruction::from_str("sbb cx, dx").to_opcode_ignore_labels(), vec![0x19, 0xD1]);
        assert_eq!(Instruction::from_str("and [bx + si], bl").to_opcode_ignore_labels(), vec![0x20, 0x18]);
        assert_eq!(Instruction::from_str("sub [bx + si], dx").to_opcode_ignore_labels(), vec![0x29, 0x10]);
        assert_eq!(Instruction::from_str("xor al, [bx + si + 40]").to_opcode_ignore_labels(), vec![0x32, 0x40, 40]);
        assert_eq!(Instruction::from_str("xor ax, [bx + si + 40]").to_opcode_ignore_labels(), vec![0x33, 0x40, 40]);
        assert_eq!(Instruction::from_str("cmp ax, [bx + si + 0x1234]").to_opcode_ignore_labels(), vec![0x3B, 0x80, 0x34, 0x12]);
        assert_eq!(Instruction::from_str("add ax, es:[bx + si + LABEL]").to_opcode_ignore_labels(), vec![0x26, 0x03, 0x80, 0, 0]);

        // TODO: add immediate instructions.
    }

    #[test]
    pub fn stack_instructions() {
        assert_eq!(Instruction::from_str("push cs").to_opcode_ignore_labels(), vec![0x00]);
        assert_eq!(Instruction::from_str("pop es").to_opcode_ignore_labels(), vec![0x00]);
        assert_eq!(Instruction::from_str("push ax").to_opcode_ignore_labels(), vec![0x00]);
        assert_eq!(Instruction::from_str("pop dx").to_opcode_ignore_labels(), vec![0x00]);

        // TODO: add immediate instructions.
    }

    #[test]
    pub fn no_operands_instruction() {
        assert_eq!(Instruction::from_str("daa").to_opcode_ignore_labels(), vec![0x27]);
        assert_eq!(Instruction::from_str("das").to_opcode_ignore_labels(), vec![0x2F]);
        assert_eq!(Instruction::from_str("aaa").to_opcode_ignore_labels(), vec![0x37]);
        assert_eq!(Instruction::from_str("aas").to_opcode_ignore_labels(), vec![0x3F]);
        assert_eq!(Instruction::from_str("nop").to_opcode_ignore_labels(), vec![0x90]);
        assert_eq!(Instruction::from_str("cbw").to_opcode_ignore_labels(), vec![0x98]);
        assert_eq!(Instruction::from_str("cwd").to_opcode_ignore_labels(), vec![0x99]);
        assert_eq!(Instruction::from_str("wait").to_opcode_ignore_labels(), vec![0x9B]);
        assert_eq!(Instruction::from_str("pushf").to_opcode_ignore_labels(), vec![0x9C]);
        assert_eq!(Instruction::from_str("popf").to_opcode_ignore_labels(), vec![0x9D]);
        assert_eq!(Instruction::from_str("sahf").to_opcode_ignore_labels(), vec![0x9E]);
        assert_eq!(Instruction::from_str("lahf").to_opcode_ignore_labels(), vec![0x9F]);
        assert_eq!(Instruction::from_str("movsb").to_opcode_ignore_labels(), vec![0xA4]);
        assert_eq!(Instruction::from_str("movsw").to_opcode_ignore_labels(), vec![0xA5]);
        assert_eq!(Instruction::from_str("cmpsb").to_opcode_ignore_labels(), vec![0xA6]);
        assert_eq!(Instruction::from_str("cmpsw").to_opcode_ignore_labels(), vec![0xA7]);
        assert_eq!(Instruction::from_str("stosb").to_opcode_ignore_labels(), vec![0xAA]);
        assert_eq!(Instruction::from_str("stosw").to_opcode_ignore_labels(), vec![0xAB]);
        assert_eq!(Instruction::from_str("lodsb").to_opcode_ignore_labels(), vec![0xAC]);
        assert_eq!(Instruction::from_str("lodsw").to_opcode_ignore_labels(), vec![0xAD]);
        assert_eq!(Instruction::from_str("scasb").to_opcode_ignore_labels(), vec![0xAE]);
        assert_eq!(Instruction::from_str("scasw").to_opcode_ignore_labels(), vec![0xAF]);
        assert_eq!(Instruction::from_str("into").to_opcode_ignore_labels(), vec![0xCE]);
        assert_eq!(Instruction::from_str("iret").to_opcode_ignore_labels(), vec![0xCF]);
        assert_eq!(Instruction::from_str("aam").to_opcode_ignore_labels(), vec![0xD4, 0x0A]);
        assert_eq!(Instruction::from_str("aad").to_opcode_ignore_labels(), vec![0xD5, 0x0A]);
        assert_eq!(Instruction::from_str("xlat").to_opcode_ignore_labels(), vec![0xD7]);
        assert_eq!(Instruction::from_str("lock").to_opcode_ignore_labels(), vec![0xF0]);
        assert_eq!(Instruction::from_str("repne").to_opcode_ignore_labels(), vec![0xF2]);
        assert_eq!(Instruction::from_str("rep").to_opcode_ignore_labels(), vec![0xF3]);
        assert_eq!(Instruction::from_str("hlt").to_opcode_ignore_labels(), vec![0xF4]);
        assert_eq!(Instruction::from_str("cmc").to_opcode_ignore_labels(), vec![0xF5]);
        assert_eq!(Instruction::from_str("clc").to_opcode_ignore_labels(), vec![0xF8]);
        assert_eq!(Instruction::from_str("stc").to_opcode_ignore_labels(), vec![0xF9]);
        assert_eq!(Instruction::from_str("cli").to_opcode_ignore_labels(), vec![0xFA]);
        assert_eq!(Instruction::from_str("sti").to_opcode_ignore_labels(), vec![0xFB]);
        assert_eq!(Instruction::from_str("cld").to_opcode_ignore_labels(), vec![0xFC]);
        assert_eq!(Instruction::from_str("std").to_opcode_ignore_labels(), vec![0xFD]);
    }
}