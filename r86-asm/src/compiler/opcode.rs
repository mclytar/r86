use std::cell::RefCell;

use crate::parser::prelude::*;
use crate::result::prelude::*;
use super::operand::UnsolvedOperand;
use super::binary::UnsolvedBinary;

#[derive(Clone, Debug)]
pub struct OpCode {
    location: Location,
    repetitions: i32,
    payload: RefCell<Vec<u8>>,
    origin: u16,
    unsolved_ops: RefCell<Vec<UnsolvedOperand>>
}
impl OpCode {
    pub fn from_instr(binary: &mut UnsolvedBinary, instr: Instruction) -> CompilerResult<Self> {
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

        let origin = binary.offset() as u16;
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
                        push_operand!(binary, operands[1], unsolved_ops, payload; [true] address);
                    },
                    ( [word] Reg(reg) <- Imm(value) ) => {
                        payload.push(byte!(0b1011_0000, wh = word) | *reg);
                        push_operand!(binary, operands[1], unsolved_ops, payload; [word] value);
                    },
                    ( [word] Reg(reg1) <- Reg(reg2) ) => {
                        payload.push(byte!(0b1000_1000, w = word));
                        payload.push(byte!(0b11_000_000, regh = *reg2, reg = *reg1));
                    },
                    ( [word] Reg(reg) <dir> Mem(segment, address) ) => {
                        payload.push(byte!(0b1000_1000, w = word, d = dir));
                        push_modrm!(binary, operands, unsolved_ops, payload; [word] reg <dir> address);
                    },
                    ( [word] Mem(segment, address) <- Imm(value) ) => {
                        payload.push(byte!(0b1100_0110, w = word));
                        let reg = 0; let dir = true;
                        push_modrm!(binary, operands, unsolved_ops, payload; [word] reg <dir> address);
                        push_operand!(binary, operands[1], unsolved_ops, payload; [word] value);
                    },
                    ( SegReg(seg_reg) <dir> Reg(reg) ) => {
                        payload.push(byte!(0b1000_1100, d = dir));
                        payload.push(byte!(0b11_000_000, regh = *seg_reg, reg = *reg));
                    },
                    ( SegReg(seg_reg) <dir> Mem(segment, address) ) => {
                        payload.push(byte!(0b1000_1100, d = dir));
                        push_modrm!(binary, operands, unsolved_ops, payload; [word] seg_reg <dir> address);
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
                };
                match_operands!(binary, payload, location, operands;
                    ( [word] Acc <- Imm(expr) ) => {
                        payload.push(byte!(0b0000_0100, w = word) | (alu_opcode << 3));
                        push_operand!(binary, operands[1], unsolved_ops, payload; [word] expr);
                    },
                    ( [_word] Reg(_reg) <- Imm(expr) ) => {
                        /*payload.push(byte!(0b1000_0000, w = word, s = true));
                        push_modrm!(binary, operands, unsolved_ops, payload; [word] reg <dir> address);
                        push_operand!(binary, operands[1], unsolved_ops, payload; [word] expr);*/
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [_word] Mem(segment, _address) <- Imm(expr) ) => {
                        /*payload.push(byte!(0b1000_0000, w = word, s = true));
                        push_modrm!(binary, operands, unsolved_ops, payload; [word] alu_opcode <dir> address);
                        push_operand!(binary, operands[1], unsolved_ops, payload; [word] expr);*/
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Reg(reg1) <- Reg(reg2) ) => {
                        payload.push(byte!(0b0000_0000, w = word) | (alu_opcode << 3));
                        payload.push(0b11_000_000 | (*reg2 << 3) | *reg1);
                    },
                    ( [word] Reg(reg) <dir> Mem(segment, address) ) => {
                        payload.push(byte!(0b0000_0000, w = word) | (alu_opcode << 3));
                        push_modrm!(binary, operands, unsolved_ops, payload; [word] reg <dir> address);
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
                };
                match_operands!(binary, payload, location, operands;
                    ( [word] Reg(reg) <- Imm(expr) ) => {
                        if let Some(value) = expr.try_eval(binary)? {
                            for _ in 0..value {
                                payload.push(byte!(0b1101_0000, v = false, w = word));
                                payload.push(0b11_000_000 | (alu_opcode << 3) | *reg);
                            }
                        } else {
                            return Err(Notification::error_critical_expression_evaluation(expr));
                        }
                    },
                    ( [word] Mem(segment, address) <- Imm(expr) ) => {
                        if let Some(value) = expr.try_eval(binary)? {
                            for _ in 0..value {
                                payload.push(byte!(0b1101_0000, v = false, w = word));
                                let dir = false;
                                push_modrm!(binary, operands, unsolved_ops, payload; [word] alu_opcode <dir> address);
                            }
                        } else {
                            return Err(Notification::error_critical_expression_evaluation(expr));
                        }
                    }
                    // TODO
                    /*( [word] Reg(reg1) <- Ctr ) => {
                        payload.push(byte!(0b1101_0000, v = true, w = word));
                        return Err(Notification::error_unimplemented(location));
                    },
                    ( [word] Mem(segment, address) <- Ctr ) => {
                        payload.push(byte!(0b1101_0000, v = true, w = word));
                        return Err(Notification::error_unimplemented(location));
                    },*/
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
                    ( [_word] Mem(segment, _address) ) => {
                        return Err(Notification::error_unimplemented(location));
                        //payload.push(byte!(0b1101_0000, v = false, w = word));
                        //payload.push(0b11_000_000 | alu_opcode | *reg);
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
                    ( Imm(expr) ) => {
                        if let (Some(section), Some(value)) = expr.try_eval_offset(binary)? {
                            if section == binary.current_section().name() {
                                let (word, offset) = resize!(2 + value - (binary.offset() as i32 + payload.len() as i32));
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
                    ( [_word] Mem(segment, _address) ) => {
                        return Err(Notification::error_unimplemented(location));
                        //payload.push(byte!(0b1101_0000, v = false, w = word));
                        //payload.push(0b11_000_000 | alu_opcode | *reg);
                    }
                );
            },
            "loop" => {
                expect_operands!(location; operands, 1);
                match_operands!(binary, payload, location, operands;
                    ( Imm(expr) ) => {
                        if let (Some(section), Some(value)) = expr.try_eval_offset(binary)? {
                            if section == binary.current_section().name() {
                                let (word, offset) = resize!(2 + value - (binary.offset() as i32 + payload.len() as i32));
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
                        ( Imm(expr)) => {
                            match expr.try_eval_offset(binary)? {
                                (Some(section), Some(value)) => {
                                    if section != binary.current_section().name() {
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
                        }
                    );
                }
            },
            "dw" => {
                for i in 0..operands.len() {
                    match_operands!(binary, payload, location, &operands[i..i+1];
                        ( Imm(expr)) => {
                            match expr.try_eval_offset(binary)? {
                                (Some(section), Some(value)) => {
                                    if section != binary.current_section().name() {
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
                        }
                    );
                }
            },
            _ => return Err(Notification::error_unimplemented(location))
        }
        binary.advance(payload.len() as i32 * repetitions);
        Ok(OpCode {
            location,
            repetitions,
            payload: RefCell::new(payload),
            origin,
            unsolved_ops: RefCell::new(unsolved_ops)
        })
    }

    pub fn size(&self) -> usize {
        self.payload.borrow().len() * self.repetitions as usize
    }

    pub fn payload(&self) -> std::cell::Ref<Vec<u8>> {
        self.payload.borrow()
    }

    pub fn check_references(&self, binary: &UnsolvedBinary) {
        for op in self.unsolved_ops.borrow().iter() {
            op.check_references(binary);
        }
    }

    pub fn solve_references(&self, binary: &UnsolvedBinary, section: Option<&String>) {
        let mut solved = Vec::new();
        for (id, op) in self.unsolved_ops.borrow_mut().iter_mut().enumerate() {
            if let Some(mut solution) = op.try_solve(binary, section) {
                solved.push(id);
                let mut size = op.size();
                let mut reference = op.reference();
                while size > 0 {
                    self.payload.borrow_mut()[reference] = (solution & 0xFF) as u8;
                    solution >>= 8;
                    size -= 1;
                    reference += 1;
                }
            }
        }
        while let Some(id) = solved.pop() {
            self.unsolved_ops.borrow_mut().remove(id);
        }
    }
}
impl Locate for OpCode {
    fn locate(&self) -> Location {
        self.location
    }
}