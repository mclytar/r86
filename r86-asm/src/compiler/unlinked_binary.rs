use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::parser::prelude::*;
use crate::result::prelude::*;

#[derive(Clone, Debug)]
pub struct LinkerLabel {
    pub(self) location: Location,
    pub(self) name: String,
    pub(self) offset: i32
}
impl LinkerLabel {
    pub fn name(&self) -> &str {
        &self.name[..]
    }

    pub fn offset(&self) -> i32 {
        self.offset
    }
}
impl Locate for LinkerLabel {
    fn locate(&self) -> Location {
        self.location
    }
}



#[derive(Clone, Debug)]
pub struct LinkerDeclaration {
    pub(self) location: Location,
    pub(self) name: String
}
impl Locate for LinkerDeclaration {
    fn locate(&self) -> Location {
        self.location
    }
}
impl AsRef<str> for LinkerDeclaration {
    fn as_ref(&self) -> &str {
        &self.name
    }
}



#[derive(Clone, Debug)]
pub struct UnsolvedOperand {
    reference: usize,
    size: usize,
    operand: Operand
}
impl UnsolvedOperand {
    pub fn new_u8(payload: &mut Vec<u8>, operand: Operand) -> UnsolvedOperand {
        let reference = payload.len();
        payload.push(0);
        let size = 1;
        UnsolvedOperand { reference, size, operand }
    }

    pub fn new_u16(payload: &mut Vec<u8>, operand: Operand) -> UnsolvedOperand {
        let reference = payload.len();
        payload.push(0);
        payload.push(0);
        let size = 2;
        UnsolvedOperand { reference, size, operand }
    }

    pub fn new_u32(payload: &mut Vec<u8>, operand: Operand) -> UnsolvedOperand {
        let reference = payload.len();
        payload.push(0);
        payload.push(0);
        payload.push(0);
        payload.push(0);
        let size = 4;
        UnsolvedOperand { reference, size, operand }
    }
}


#[derive(Clone, Debug)]
pub struct OpCode {
    location: Location,
    repetitions: i32,
    payload: Vec<u8>,
    origin: u16,
    unsolved_ops: Vec<UnsolvedOperand>
}
impl OpCode {
    pub fn from_instr(binary: &mut UnlinkedBinary, instr: Instruction) -> CompilerResult<Self> {
        let location = instr.locate();

        let repetitions = if let Some(quantifier) = instr.quantifier() {
            match quantifier.try_eval(binary)? {
                Some(val) => val,
                None => { binary.err(Notification::error_critical_expression_evaluation(quantifier)); 1 }
            }
        } else { 1 };

        let mut payload = Vec::new();

        // Store location for warning purposes (i.e. "this prefix is useless in this context").
        let _prefix_location = if let Some((prefix_location, prefix)) = instr.prefix() {
            match &prefix[..] {
                "lock" => payload.push(0xF0),
                "repne" | "repnz" => payload.push(0xF2),
                "rep" | "repe" | "repz" => payload.push(0xF3),
                _ => unreachable!()
            }
            Some(*prefix_location)
        } else { None };

        let origin = binary.current_offset();
        let mut unsolved_ops = Vec::new();

        // Decoding instruction.
        let name = instr.name().to_owned();
        let operands = instr.into_operands();
        match &name[..] {
            // --------------------------------
            // Data transfer.
            // --------------------------------
            "mov" => {
                expect_operands!(location; operands, 2);
                match_operands!(binary, payload, location, operands;
                    ( [word] Acc <dir> Mem(_segment, address) ) => {
                        payload.push(byte!(0b1010_0000, w = word, d = dir));

                        if let (_, Some(offset)) = address.try_eval_offset(binary)? {
                            push_word!(payload, offset);
                        } else {
                            let operand = if dir { operands[1].to_owned() } else { operands[0].to_owned() };
                            unsolved_ops.push(UnsolvedOperand::new_u16(&mut payload, operand));
                        }
                    },
                    ( [word] Reg(reg) <- Imm(value) ) => {
                        payload.push(byte!(0b1011_0000, wh = word) | *reg);
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Reg(reg1) <- Reg(reg2) ) => {
                        payload.push(byte!(0b1000_1000, w = word));
                        payload.push(byte!(0b11_000_000, regh = *reg2, reg = *reg1));
                    },
                    default => {
                        return Err(Notification::error_unimplemented(location));
                    }
                );
            },
            "push" => {
                expect_operands!(location; operands, 1);
                return Err(Notification::error_unimplemented(location));
            },
            "pop" => {
                expect_operands!(location; operands, 1);
                return Err(Notification::error_unimplemented(location));
            },
            "xchg" => {
                expect_operands!(location; operands, 2);
                return Err(Notification::error_unimplemented(location));
            },
            "in" => {
                return Err(Notification::error_unimplemented(location));
            },
            "out" => {
                return Err(Notification::error_unimplemented(location));
            },
            "xlat" => {
                return Err(Notification::error_unimplemented(location));
            },
            "lea" => {
                return Err(Notification::error_unimplemented(location));
            },
            "lds" => {
                return Err(Notification::error_unimplemented(location));
            },
            "les" => {
                return Err(Notification::error_unimplemented(location));
            },
            "lahf" => {
                return Err(Notification::error_unimplemented(location));
            },
            "sahf" => {
                return Err(Notification::error_unimplemented(location));
            }
            "pushf" => {
                return Err(Notification::error_unimplemented(location));
            },
            "popf" => {
                return Err(Notification::error_unimplemented(location));
            },
            // --------------------------------
            // 2-operands arithmetic and logic.
            // --------------------------------
            "add" | "adc" | "sub" | "sbb" | "and" | "or" | "xor" | "cmp" => {
                expect_operands!(location; operands, 2);
                let alu_opcode = match &name[..] {
                    "add" => 0b000,
                    "adc" => 0b010,
                    "sub" => 0b101,
                    "sbb" => 0b011,
                    "and" => 0b100,
                    "or"  => 0b001,
                    "xor" => 0b110,
                    "cmp" => 0b111,
                    _ => unreachable!()
                } << 3;
                match_operands!(binary, payload, location, operands;
                    ( [word] Acc <- Imm(expr) ) => {
                        payload.push(byte!(0b0000_0100, w = word) | alu_opcode);
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Reg(reg) <- Imm(expr) ) => {
                        payload.push(byte!(0b1000_0000, w = word, s = true));
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Mem(segment, address) <- Imm(expr) ) => {
                        //payload.push(byte!(0b1000_0000, w = word, s = true));
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Reg(reg1) <- Reg(reg2) ) => {
                        payload.push(byte!(0b0000_0000, w = word) | alu_opcode);
                        payload.push(0b11_000_000 | (*reg2 << 3) | *reg1);
                    },
                    ( [word] Reg(reg) <dir> Mem(segment, address) ) => {
                        payload.push(byte!(0b0000_0000, w = word) | alu_opcode);
                        return Err(Notification::error_unimplemented(location));
                    },
                    default => {
                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                    }
                );
            },
            "shl" | "sal" | "shr" | "sar" | "rol" | "ror" | "rcl" | "rcr" => {
                expect_operands!(location; operands, 2);
                let alu_opcode = match &name[..] {
                    "shl" => 0b100,
                    "sal" => 0b100,
                    "shr" => 0b101,
                    "sar" => 0b111,
                    "rol" => 0b000,
                    "ror" => 0b001,
                    "rcl" => 0b010,
                    "rcr" => 0b011,
                    _ => unreachable!()
                } << 3;
                match_operands!(binary, payload, location, operands;
                    ( [word] Reg(reg) <- Imm(expr) ) => {
                        if let Some(value) = expr.try_eval(binary)? {
                            for _ in 0..value {
                                payload.push(byte!(0b1101_0000, v = false, w = word));
                                payload.push(0b11_000_000 | alu_opcode | *reg);
                            }
                        } else {
                            return Err(Notification::error_critical_expression_evaluation(expr));
                        }
                    },
                    ( [word] Mem(segment, address) <- Imm(expr) ) => {
                        if let Some(value) = expr.try_eval(binary)? {
                            return Err(Notification::error_unimplemented(location));
                            for _ in 0..value {
                                //payload.push(byte!(0b1101_0000, v = false, w = word));
                                //payload.push(0b11_000_000 | alu_opcode | *reg);
                            }
                        } else {
                            return Err(Notification::error_critical_expression_evaluation(expr));
                        }
                    },
                    // TODO
                    /*( [word] Reg(reg1) <- Ctr ) => {
                        payload.push(byte!(0b1101_0000, v = true, w = word));
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Mem(segment, address) <- Ctr ) => {
                        payload.push(byte!(0b1101_0000, v = true, w = word));
                        return Err(Notification::error_unimplemented(location));
                    },*/
                    default => {
                        expect_operands!(location; operands, 2);
                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                    }
                );
            },
            // --------------------------------
            // 1-operand arithmetic and logic.
            // --------------------------------
            "inc" | "dec" => {
                expect_operands!(location; operands, 1);
                let alu_opcode = match &name[..] {
                    "inc" => 0,
                    "dec" => 1,
                    _ => unreachable!()
                } << 3;
                match_operands!(binary, payload, location, operands;
                    ( [word] Reg(reg) ) => {
                        if word {
                            payload.push(0b01_000_000 | alu_opcode | *reg);
                        } else {
                            payload.push(0b1111_1111);
                            payload.push(0b11_000_000 | alu_opcode | *reg);
                        }
                    },
                    ( [word] Mem(segment, address) ) => {
                        return Err(Notification::error_unimplemented(location));
                        //payload.push(byte!(0b1101_0000, v = false, w = word));
                        //payload.push(0b11_000_000 | alu_opcode | *reg);
                    },
                    default => {
                        if operands.len() != 2 {
                            return Err(Notification::error_operands_amount(location, 2, operands.len(), &operands));
                        } else {
                            return Err(Notification::error_invalid_set_of_operands(location, &operands));
                        }
                    }
                );
            },
            // --------------------------------
            // String manipulation.
            // --------------------------------
            "rep" | "repe" | "repz" => {
                expect_operands!(location; operands, 0);
                payload.push(0b11110011);
            },
            "repne" | "repnz" => {
                expect_operands!(location; operands, 0);
                payload.push(0b11110010);
            },
            "movsb" | "movsw" | "cmpsb" | "cmpsw" | "scasb" | "scasw" | "lodsb" | "lodsw" | "stosb" | "stosw" => {
                expect_operands!(location; operands, 0);
                let opcode = match &name[..name.len() - 1] {
                    "movs" => 0b010,
                    "cmps" => 0b011,
                    "scas" => 0b111,
                    "lods" => 0b110,
                    "stos" => 0b101,
                    _ => unreachable!()
                } << 1;
                let opcode = byte!(0b1010_0000, w = name.ends_with('w')) | opcode;
                payload.push(opcode);
            },
            // --------------------------------
            // Control transfer.
            // --------------------------------
            "jmp" => {
                expect_operands!(location; operands, 1);
                match_operands!(binary, payload, location, operands;
                    ( [word] Imm(expr) ) => {
                        if let (Some(section), Some(value)) = expr.try_eval_offset(binary)? {
                            if section == binary.current_section_name() {
                                let (word, offset) = resize!(2 + value - (binary.current_offset() as i32 + payload.len() as i32));
                                if word {
                                    payload.push(0b1110_1001);
                                    push_word!(payload, offset + 1);
                                } else {
                                    payload.push(0b1110_1011);
                                    payload.push(offset as u8);
                                }
                            } else {
                                payload.push(0b1110_1010);
                                unsolved_ops.push(UnsolvedOperand::new_u32(&mut payload, operands[0].to_owned()));
                            }
                        } else {
                            payload.push(0b1110_1001);
                            unsolved_ops.push(UnsolvedOperand::new_u16(&mut payload, operands[0].to_owned()));
                        }
                    },
                    ( [word] Mem(segment, address) ) => {
                        return Err(Notification::error_unimplemented(location));
                        //payload.push(byte!(0b1101_0000, v = false, w = word));
                        //payload.push(0b11_000_000 | alu_opcode | *reg);
                    },
                    default => {
                        return Err(Notification::error_unimplemented(location));
                        if operands.len() != 2 {
                            return Err(Notification::error_operands_amount(location, 2, operands.len(), &operands));
                        } else {
                            return Err(Notification::error_invalid_set_of_operands(location, &operands));
                        }
                    }
                );
            },
            "loop" => {
                expect_operands!(location; operands, 1);
                match_operands!(binary, payload, location, operands;
                    ( [word] Imm(expr) ) => {
                        if let (Some(section), Some(value)) = expr.try_eval_offset(binary)? {
                            if section == binary.current_section_name() {
                                let (word, offset) = resize!(2 + value - (binary.current_offset() as i32 + payload.len() as i32));
                                if word {
                                    return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                } else {
                                    payload.push(0b1110_0010);
                                    payload.push(offset as u8);
                                }
                            } else {
                                return Err(Notification::error_invalid_set_of_operands(location, &operands));
                            }
                        } else {
                            payload.push(0b1110_1001);
                            unsolved_ops.push(UnsolvedOperand::new_u8(&mut payload, operands[0].to_owned()));
                        }
                    },
                    ( [word] Mem(segment, address) ) => {
                        return Err(Notification::error_unimplemented(location));
                        //payload.push(byte!(0b1101_0000, v = false, w = word));
                        //payload.push(0b11_000_000 | alu_opcode | *reg);
                    },
                    default => {
                        return Err(Notification::error_unimplemented(location));
                        if operands.len() != 2 {
                            return Err(Notification::error_operands_amount(location, 2, operands.len(), &operands));
                        } else {
                            return Err(Notification::error_invalid_set_of_operands(location, &operands));
                        }
                    }
                );
            }
            // --------------------------------
            // Processor transfer.
            // --------------------------------
            "nop" => {
                expect_operands!(location; operands, 0);
                payload.push(0x90);
            },
            // --------------------------------
            // Pseudo-instructions.
            // --------------------------------
            "db" => {
                for i in 0..operands.len() {
                    match_operands!(binary, payload, location, &operands[i..i+1];
                        ( [word] Imm(expr)) => {
                            match expr.try_eval_offset(binary)? {
                                (Some(section), Some(value)) => {
                                    if section != binary.current_section_name() {
                                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                    }
                                    if value >= 256 {
                                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                    }
                                    payload.push(value as u8);
                                },
                                (None, Some(value)) => {
                                    if value >= 256 {
                                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                    }
                                    payload.push(value as u8);
                                },
                                (_, None) => {
                                    unsolved_ops.push(UnsolvedOperand::new_u8(&mut payload, operands[0].to_owned()));
                                }
                            }
                        },
                        default => {
                            return Err(Notification::error_invalid_set_of_operands(location, &operands));
                        }
                    );
                }
            },
            "dw" => {
                for i in 0..operands.len() {
                    match_operands!(binary, payload, location, &operands[i..i+1];
                        ( [word] Imm(expr)) => {
                            match expr.try_eval_offset(binary)? {
                                (Some(section), Some(value)) => {
                                    if section != binary.current_section_name() {
                                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                    }
                                    if value >= 65536 {
                                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                    }
                                    push_word!(payload, value);
                                },
                                (None, Some(value)) => {
                                    if value >= 65536 {
                                        return Err(Notification::error_invalid_set_of_operands(location, &operands));
                                    }
                                    push_word!(payload, value);
                                },
                                (_, None) => {
                                    unsolved_ops.push(UnsolvedOperand::new_u8(&mut payload, operands[0].to_owned()));
                                }
                            }
                        },
                        default => {
                            return Err(Notification::error_invalid_set_of_operands(location, &operands));
                        }
                    );
                }
            },
            _ => return Err(Notification::error_unimplemented(location))
        }
        binary.advance(payload.len() as u16 * repetitions as u16);
        Ok(OpCode {
            location,
            repetitions,
            payload,
            origin,
            unsolved_ops
        })
    }

    pub fn size(&self) -> usize {
        self.payload.len() * self.repetitions as usize
    }
}
impl Locate for OpCode {
    fn locate(&self) -> Location {
        self.location
    }
}

#[derive(Clone, Debug)]
pub struct Section {
    pub(self) location: Location,
    pub(self) name: Option<String>,
    pub(self) local_vars: Vec<LinkerLabel>,
    pub(self) opcodes: Vec<OpCode>
}
impl Section {
    pub fn name(&self) -> Option<String> {
        self.name.clone()
    }

    pub fn vars(&self) -> &[LinkerLabel] {
        &self.local_vars[..]
    }

    pub fn opcodes(&self) -> &[OpCode] {
        &self.opcodes[..]
    }
}
impl Locate for Section {
    fn locate(&self) -> Location {
        self.location
    }
}

#[derive(Debug)]
pub struct UnlinkedBinary {
    filename: PathBuf,
    contents: Rc<String>,
    log: CompilerLog,
    pub(self) sections: Vec<Rc<RefCell<Section>>>,
    pub(self) global_vars: Vec<LinkerDeclaration>,
    pub(self) extern_vars: Vec<LinkerDeclaration>,
    pub(self) origin: u16,
    pub(self) offset: u16,
    pub(self) last_label: Option<String>,
    pub(self) current_section: Rc<RefCell<Section>>
}
impl UnlinkedBinary {
    pub fn new(filename: PathBuf, contents: Rc<String>, log: CompilerLog) -> Self {
        let current_section = Section { location: Location::new(0, 0, 0, 0), name: None, local_vars: Vec::new(), opcodes: Vec::new() };
        let current_section = Rc::new(RefCell::new(current_section));
        UnlinkedBinary {
            filename,
            contents,
            log,
            sections: vec![current_section.clone()],
            current_section,
            origin: 0,
            offset: 0,
            last_label: None,
            global_vars: Vec::new(),
            extern_vars: Vec::new()
        }
    }

    pub fn filename(&self) -> &Path {
        &self.filename
    }

    pub fn contents(&self) -> Rc<String> {
        self.contents.clone()
    }

    pub fn warn(&mut self, warning: Notification) {
        self.log.warn(warning);
    }

    pub fn err(&mut self, error: Notification) {
        self.log.err(error);
    }

    pub fn as_str(&self) -> &str {
        self.contents.as_str()
    }

    pub fn declare_global<S>(&mut self, var: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        if let Some(dup) = self.global_vars.iter().find(|s| s.as_ref() == var.as_ref()) {
            return Err(Notification::error_duplicate_definition(var.as_ref(), dup, &var));
        }
        let declaration = LinkerDeclaration { location: var.locate(), name: var.as_ref().to_owned() };
        self.global_vars.push(declaration);
        Ok(())
    }

    pub fn declare_extern<S>(&mut self, var: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        if let Some(dup) = self.extern_vars.iter().find(|s| s.as_ref() == var.as_ref()) {
            return Err(Notification::error_duplicate_definition(var.as_ref(), dup, &var));
        }
        let declaration = LinkerDeclaration { location: var.locate(), name: var.as_ref().to_owned() };
        self.extern_vars.push(declaration);
        Ok(())
    }

    pub fn declare_section<S>(&mut self, name: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        let result = if let Some(dup) = self.sections.iter().find(|s| if let Some(ref dup) = s.as_ref().borrow().name { dup == name.as_ref() } else { false }) {
            Err(Notification::error_duplicate_definition(name.as_ref(), dup.as_ref().borrow().locate(), &name))
        } else {
            Ok(())
        };
        let section = Section { location: name.locate(), name: Some(name.as_ref().to_owned()), local_vars: Vec::new(), opcodes: Vec::new() };
        let section = Rc::new(RefCell::new(section));
        self.sections.push(section.clone());
        self.origin = 0;
        self.offset = 0;
        self.last_label = None;
        self.current_section = section;
        result
    }

    pub fn declare_local_label<S>(&mut self, name: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        let location = name.locate();
        let name = name.as_ref();
        let name = if name.starts_with('.') {
            if let Some(label) = self.last_label.as_ref() {
                label.to_owned() + name
            } else {
                name.to_owned()
            }
        } else {
            name.to_owned()
        };
        for sec in self.sections.iter() {
            for dup in sec.as_ref().borrow().local_vars.iter() {
                if &dup.name == &name {
                    return Err(Notification::error_duplicate_definition(&name, dup, location));
                }
            }
        }
        let label = LinkerLabel { location, name, offset: self.offset as i32 };
        self.current_section.borrow_mut()
            .local_vars.push(label);
        Ok(())
    }

    pub fn set_origin(&mut self, origin: i32) {
        self.origin = origin as u16;
    }

    pub fn advance(&mut self, amount: u16) {
        self.offset = self.offset.overflowing_add(amount).0;
    }

    pub fn sections(&self) -> &[Rc<RefCell<Section>>] {
        &self.sections[..]
    }

    pub fn current_section_name(&self) -> Option<String> {
        self.current_section.as_ref()
            .borrow()
            .name()
    }

    pub fn current_offset(&self) -> u16 {
        self.offset
    }

    pub fn process_statement(&mut self, statement: Statement) -> CompilerResult<()> {
        match statement.kind() {
            StatementKind::Global(label) => self.declare_global(label)?,
            StatementKind::Extern(label) => self.declare_extern(label)?,
            StatementKind::Section(label) => self.declare_section(label)?,
            StatementKind::Origin(num) => self.set_origin(num.value()),
            StatementKind::Label(label) => self.declare_local_label(label)?,
            StatementKind::Instruction(instr) => {
                if let Some(label) = instr.label() {
                    if let Err(error) = self.declare_local_label(label) {
                        self.err(error);
                    }
                }
                let opcode = OpCode::from_instr(self, instr.to_owned())?;
                self.current_section.borrow_mut().opcodes.push(opcode);
            }
        }
        Ok(())
    }

    pub fn from_listing(listing: Listing) -> Result<Self, CompilerLog> {
        let Listing { filename, contents, log, statements, .. } = listing;
        let mut binary = Self::new(filename, contents, log);
        for statement in statements {
            if let Err(error) = binary.process_statement(statement) {
                binary.err(error);
            }
        }
        let mut iter_of_ops = Vec::new();
        for section in binary.sections.iter() {
            let section = (section as &RefCell<Section>).borrow();
            for opcode in section.opcodes() {
                for _ in 0..opcode.repetitions {
                    for byte in opcode.payload.iter() {
                        iter_of_ops.push(*byte);
                    }
                }
            }
        }
        println!("Generated binary:");
        let mut display = true;
        for i in 0..iter_of_ops.len() {
            if i > 0 && i % 16 == 0 {
                if i < iter_of_ops.len() - 16 && &iter_of_ops[i - 16..i] == &iter_of_ops[i..i + 16] {
                    if display { println!(); print!("..."); }
                    display = false;
                } else {
                    println!();
                    display = true;
                }
            }
            if i % 16 == 0 && display {
                print!("{:04x}: ", i);
            }
            if display {
                print!("{:02x} ", iter_of_ops[i]);
            }
        }
        println!();
        if binary.log.is_err() {
            Err(binary.log)
        } else {
            Ok(binary)
        }
    }
}