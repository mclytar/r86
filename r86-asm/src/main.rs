use std::str::FromStr;

fn get_file_contents() -> String {
    /*let mut asm_file = File::open("source.asm").unwrap();
    let mut contents = String::new();
    asm_file.read_to_string(&mut contents).unwrap();*/

    let contents = r##"
    main:
            nop
            add     ax, cx
            sub     al, ah
            ;inc     ch
            ;inc     dx
    .L0:    nop
    "##.to_owned();

    contents
}

#[derive(Copy, Clone, Debug, Hash)]
pub enum OperationKind {
    ArithmeticTwoOperands,
    SpecialTwoOperands,
    ArithmeticOneOperand,
    Stack,
    NoParams,
    Jump
}
impl OperationKind {
    pub fn opcode(&self, op: &str) -> Result<Vec<u8>, ()> {
        match op {
            "aaa"   => Ok(vec![0b00110111]),
            "aas"   => Ok(vec![0b00111111]),
            "daa"   => Ok(vec![0b00100111]),
            "das"   => Ok(vec![0b00101111]),
            "nop"   => Ok(vec![0b10010000]),
            "cbw"   => Ok(vec![0b10011000]),
            "cwd"   => Ok(vec![0b10011001]),
            "wait"  => Ok(vec![0b10011011]),
            "pushf" => Ok(vec![0b10011100]),
            "popf"  => Ok(vec![0b10011101]),
            "sahf"  => Ok(vec![0b10011110]),
            "lahf"  => Ok(vec![0b10011111]),
            "movsb" => Ok(vec![0b10100100]),
            "movsw" => Ok(vec![0b10100101]),
            "cmpsb" => Ok(vec![0b10100110]),
            "cmpsw" => Ok(vec![0b10100111]),
            "stosb" => Ok(vec![0b10101010]),
            "stosw" => Ok(vec![0b10101011]),
            "lodsb" => Ok(vec![0b10101100]),
            "lodsw" => Ok(vec![0b10101101]),
            "scasb" => Ok(vec![0b10101110]),
            "scasw" => Ok(vec![0b10101111]),
            "into"  => Ok(vec![0b11001110]),
            "iret"  => Ok(vec![0b11001111]),
            "aam"   => Ok(vec![0b11010100, 0b00001010]),
            "aad"   => Ok(vec![0b11010101, 0b00001010]),
            "xlat"  => Ok(vec![0b11010111]),
            "hlt"   => Ok(vec![0b11110100]),
            "cmc"   => Ok(vec![0b11110101]),
            "clc"   => Ok(vec![0b11111000]),
            "stc"   => Ok(vec![0b11111001]),
            "cli"   => Ok(vec![0b11111010]),
            "sti"   => Ok(vec![0b11111011]),
            "cld"   => Ok(vec![0b11111100]),
            "std"   => Ok(vec![0b11111101]),
            _ => Err(())
        }
    }

    pub fn decode_operand(&self, opr: &str) -> Result<u8, ()> {
        let mut operand = 0;
        let r16 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
        let r8 = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
        let seg = ["cs", "ss", "ds", "es"];

        if let Some(opr) = r8.iter().position(|s| *s == opr) {
            operand = opr as u8;
            Ok(operand)
        } else if let Some(opr) = r16.iter().position(|s| *s == opr) {
            operand = opr as u8;
            operand |= 0x08;
            Ok(operand)
        } else if let Some(opr) = seg.iter().position(|s| *s == opr) {
            operand = opr as u8;
            operand |= 0x10;
            Ok(operand)
        } else {
            unimplemented!()
        }
    }

    pub fn opcode_two_operands(&self, op: &str, opr1: &str, opr2: &str) -> Result<Vec<u8>, ()> {
        match *self {
            OperationKind::ArithmeticTwoOperands => {
                let opr1 = self.decode_operand(opr1)?;
                let opr2 = self.decode_operand(opr2)?;
                if (opr1 ^ opr2) & 0x08 != 0 {
                    return Err(())
                }
                if (opr1 | opr2) & 0x10 != 0 {
                    return Err(())
                }
                if (opr1 | opr2) & 0x20 == 0 {
                    let mut result = match op {
                        "add"   => 0b00000000,
                        "or"    => 0b00001000,
                        "adc"   => 0b00010000,
                        "sbb"   => 0b00011000,
                        "and"   => 0b00100000,
                        "sub"   => 0b00101000,
                        "xor"   => 0b00110000,
                        "cmp"   => 0b00111000,
                        _ => return Err(())
                    };
                    if opr1 & 0x08 != 0 {
                        result |= 0x01;
                    }
                    let result = vec![result, 0b11000000 | (opr2 << 3) | opr1];
                    Ok(result)
                } else {
                    unimplemented!()
                }
            },
            OperationKind::SpecialTwoOperands => {
                unimplemented!()
            },
            _ => Err(())
        }
    }
}
impl FromStr for OperationKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "add"
            | "or"
            | "adc"
            | "sbb"
            | "and"
            | "sub"
            | "xor"
            | "cmp" => Ok(OperationKind::ArithmeticTwoOperands),
            "test"
            | "xchg"
            | "mov"
            | "lea" => Ok(OperationKind::SpecialTwoOperands),
            "inc"
            | "dec"
            | "mul"
            | "div"
            | "imul"
            | "idiv" => Ok(OperationKind::ArithmeticOneOperand),
            "push"
            | "pop" => Ok(OperationKind::Stack),
            "aaa"
            | "aas"
            | "daa"
            | "das"
            | "nop"
            | "cbw"
            | "cwd"
            | "wait"
            | "pushf"
            | "popf"
            | "sahf"
            | "lahf"
            | "movsb"
            | "movsw"
            | "cmpsb"
            | "cmpsw"
            | "stosb"
            | "stosw"
            | "lodsb"
            | "lodsw"
            | "scasb"
            | "scasw"
            | "into"
            | "iret"
            | "aam"
            | "aad"
            | "xlat"
            | "hlt"
            | "cmc"
            | "clc"
            | "stc"
            | "cli"
            | "sti"
            | "cld"
            | "std"
            => Ok(OperationKind::NoParams),
            "jo"
            | "jno"
            | "jb"
            | "jnae"
            | "jc"
            | "jnb"
            | "jae"
            | "jnc"
            | "je"
            | "jz"
            | "jne"
            | "jnz"
            | "jbe"
            | "jna"
            | "jnbe"
            | "ja"
            | "js"
            | "jns"
            | "jp"
            | "jpe"
            | "jnp"
            | "jpo"
            | "jl"
            | "jnge"
            | "jnl"
            | "jge"
            | "jle"
            | "jng"
            | "jnle"
            | "jg" => Ok(OperationKind::Jump),
            _ => Err(())
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instruction {
    opcode: Vec<u8>,
    label: Option<String>,
    reference_name: Option<String>,
    reference_location: usize
}
impl Instruction {
    pub fn new<S>(line: S) -> Instruction where
        S: AsRef<str>
    {
        let comment = line.as_ref().find(';');
        let mut line = if let Some(comment) = comment {
            (line.as_ref()[..comment]).trim().to_owned()
        } else {
            line.as_ref().trim().to_owned()
        };

        let label = if let Some(index) = line.find(':') {
            if line[..index].trim().contains(' ') {
                None
            } else {
                let label = line[..index].trim().to_owned();
                line = line[index+1..].trim().to_owned();
                Some(label)
            }
        } else {
            None
        };

        if line.is_empty() {
            return Instruction {
                opcode: vec![0; 6],
                label,
                reference_name: None,
                reference_location: 0
            };
        }

        let instruction = line.split_whitespace().next().unwrap().to_lowercase();
        line = line[instruction.len()..].trim().to_owned();

        let line: Vec<_> = if line.is_empty() {
            Vec::new()
        } else {
            line.replace(' ', "").split(',').map(|s| s.to_owned()).collect()
        };

        let operation_kind = OperationKind::from_str(&instruction[..]).unwrap();

        println!("Processing '{}' (kind: {:?})", instruction, operation_kind);

        let opcode = match operation_kind {
            OperationKind::NoParams => operation_kind.opcode(&instruction[..]).unwrap(),
            OperationKind::ArithmeticTwoOperands => operation_kind.opcode_two_operands(&instruction[..], &line[0][..], &line[1][..]).unwrap(),
            _ => unimplemented!()
        };

        Instruction {
            opcode,
            label,
            reference_name: None,
            reference_location: 0
        }
    }
}

fn main() {
    let contents = get_file_contents();

    let mut pre_assembly = Vec::new();

    for line in contents.lines() {
        let instruction = Instruction::new(line);

        pre_assembly.push(instruction);
    }

    println!("{:#x?}", pre_assembly);
}