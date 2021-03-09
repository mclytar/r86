use std::str::FromStr;

use regex::Regex;

trait FormattedToNumber {
    fn is_formatted_number(&self) -> bool;
    fn formatted_to_number(&self) -> Result<u16, std::num::ParseIntError>;
}
impl FormattedToNumber for &str {
    fn is_formatted_number(&self) -> bool {
        let value = self.trim().to_lowercase();

        if let Some(first_digit) = value.chars().next() {
            if !first_digit.is_numeric() {
                return false;
            }
        } else {
            return false;
        };

        if value.starts_with("0x") {
            let value = &value[2..];
            let number = u16::from_str_radix(value, 16);
            number.is_ok()
        } else if value.starts_with("0b") {
            let value = &value[2..];
            let number = u16::from_str_radix(value, 2);
            number.is_ok()
        } else if value.ends_with("h") {
            let value = &value[.. value.len() - 1];
            let number = u16::from_str_radix(value, 16);
            number.is_ok()
        } else {
            let value = &value[..];
            let number = u16::from_str(value);
            number.is_ok()
        }
    }

    fn formatted_to_number(&self) -> Result<u16, std::num::ParseIntError> {
        let value = self.trim().to_lowercase();

        if value.len() > 0 {
            u16::from_str(&value[0..1])?;
        } else {
            u16::from_str("")?;
        }

        if value.starts_with("0x") {
            let value = &value[2..];
            let number = u16::from_str_radix(value, 16);
            number
        } else if value.starts_with("0b") {
            let value = &value[2..];
            let number = u16::from_str_radix(value, 2);
            number
        } else if value.ends_with("h") {
            let value = &value[.. value.len() - 1];
            let number = u16::from_str_radix(value, 16);
            number
        } else {
            let value = &value[..];
            let number = u16::from_str(value);
            number
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum NumericOption {
    None,
    Number(u16),
    Label(String)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Operand {
    Reg8(u8),
    Reg16(u8),
    RegSegment(u8),
    Immediate(u16),
    ImmediateLabel(String),
    RegisterMemory {
        segment_override: Option<u8>,
        word_flag: Option<bool>,
        mode_number: u8,
        displacement: NumericOption
    }
}
impl Operand {
    pub fn from_str<S>(s: S) -> Operand where
        S: AsRef<str>
    {
        let re_label = Regex::new(r"^(\.|[A-Za-z_])[A-Za-z0-9_]*$").unwrap();
        let r16 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
        let r8 = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
        let seg = ["es", "cs", "ss", "ds"];
        let source = s.as_ref().replace(' ', "");
        let mut opr = &source[..];

        if let Some(opr) = r8.iter().position(|s| *s == opr) {
            Operand::Reg8(opr as u8)
        } else if let Some(opr) = r16.iter().position(|s| *s == opr) {
            Operand::Reg16(opr as u8)
        } else if let Some(opr) = seg.iter().position(|s| *s == opr) {
            Operand::RegSegment(opr as u8)
        } else if opr.is_formatted_number() {
            Operand::Immediate(opr.formatted_to_number().unwrap())
        } else if re_label.is_match(opr) {
            Operand::ImmediateLabel(source)
        } else {
            let segment_override = if let Some(i) = opr.find(':') {
                if let Some(seg) = seg.iter().position(|s| *s == &opr[..i]) {
                    opr = &opr[i+1..];
                    Some(seg as u8)
                } else {
                    panic!("Expected segment register, found '{}'.", &opr[..i]);
                }
            } else {
                None
            };
            let word_flag = if opr.starts_with("b.") {
                opr = &opr[2..];
                Some(false)
            } else if opr.starts_with("w.") {
                opr = &opr[2..];
                Some(true)
            } else if !opr.starts_with("[") || !opr.ends_with(']') {
                panic!("Expected address, found '{}'.", &opr[..]);
            } else {
                None
            };

            let parts: Vec<_> = opr[1..opr.len()-1].split('+').collect();
            let mut displacement = NumericOption::None;

            let mode_number = match parts.len() {
                0 => panic!("Expected address, found nothing."),
                1 => match parts[0] {
                    "si" => 4,
                    "di" => 5,
                    "bp" => 6,
                    "bx" => 7,
                    _ => if parts[0].is_formatted_number() {
                        displacement = NumericOption::Number(parts[0].formatted_to_number().unwrap());
                        6
                    } else if re_label.is_match(parts[0]) {
                        displacement = NumericOption::Label(parts[0].to_owned());
                        6
                    } else {
                        panic!("Expected register, address, label or sum of the previous, got '{}'", opr);
                    }
                },
                2 => if parts.iter().find(|s| **s == "bx").is_some() && parts.iter().find(|s| **s == "si").is_some() {
                    0
                } else if parts.iter().find(|s| **s == "bx").is_some() && parts.iter().find(|s| **s == "di").is_some() {
                    1
                } else if parts.iter().find(|s| **s == "bp").is_some() && parts.iter().find(|s| **s == "si").is_some() {
                    2
                } else if parts.iter().find(|s| **s == "bp").is_some() && parts.iter().find(|s| **s == "di").is_some() {
                    3
                } else if let Some((reg, i)) = parts.iter().zip(0..).find(|(s, _)| **s == "si" || **s == "di" || **s == "bp" || **s == "bx") {
                    if parts[1 - i].is_formatted_number() {
                        displacement = NumericOption::Number(parts[1 - i].formatted_to_number().unwrap());
                    } else if re_label.is_match(parts[1 - i]) {
                        displacement = NumericOption::Label(parts[1 - i].to_owned());
                    } else {
                        panic!("Expected register, address, label or sum of the previous, got '{}'", opr);
                    }
                    match *reg {
                        "si" => 4,
                        "di" => 5,
                        "bp" => 6,
                        "bx" => 7,
                        _ => unreachable!()
                    }
                } else {
                    panic!("Expected register, address, label or sum of the previous, got '{}'", opr);
                },
                3 => {
                    let (reg1, i1) = if let Some((reg, i)) = parts.iter().zip(0..).find(|(s, _)| **s == "bx" || **s == "bp") {
                        (*reg, i)
                    } else {
                        panic!("Expected register, address, label or sum of the previous, got '{}'", opr);
                    };
                    let (reg2, i2) = if let Some((reg, i)) = parts.iter().zip(0..).find(|(s, _)| **s == "si" || **s == "di") {
                        (*reg, i)
                    } else {
                        panic!("Expected register, address, label or sum of the previous, got '{}'", opr);
                    };
                    if parts[3 - (i1 + i2)].is_formatted_number() {
                        displacement = NumericOption::Number(parts[3 - (i1 + i2)].formatted_to_number().unwrap());
                    } else if re_label.is_match(parts[3 - (i1 + i2)]) {
                        displacement = NumericOption::Label(parts[3 - (i1 + i2)].to_owned());
                    } else {
                        panic!("Expected register, address, label or sum of the previous, got '{}'", opr);
                    }
                    match (reg1, reg2) {
                        ("bx", "si") => 0,
                        ("bx", "di") => 1,
                        ("bp", "si") => 2,
                        ("bp", "di") => 3,
                        _ => unreachable!()
                    }
                },
                _ => panic!("'{}' is not a valid address.", &opr[..])
            };

            Operand::RegisterMemory {
                segment_override,
                word_flag,
                mode_number,
                displacement
            }
        }
    }

    pub fn segment_override(&self) -> Option<u8> {
        if let Operand::RegisterMemory { ref segment_override, .. } = *self {
            segment_override.to_owned()
        } else {
            None
        }
    }

    pub fn forces_byte(&self) -> bool {
        match *self {
            Operand::Reg8(_) => true,
            Operand::RegisterMemory { word_flag: Some(false), .. } => true,
            _ => false,
        }
    }

    pub fn forces_word(&self) -> bool {
        match *self {
            Operand::Reg16(_) | Operand::RegSegment(_) => true,
            Operand::RegisterMemory { word_flag: Some(true), .. } => true,
            Operand::Immediate(v) => v > 255,
            _ => false,
        }
    }

    pub fn read_only(&self) -> bool {
        match *self {
            Operand::ImmediateLabel(_) | Operand::Immediate(_) => true,
            _ => false
        }
    }

    pub fn is_address(&self) -> bool {
        match *self {
            Operand::RegisterMemory {..} => true,
            _ => false
        }
    }

    pub fn is_register(&self) -> bool {
        match *self {
            Operand::Reg8(_) | Operand::Reg16(_) => true,
            _ => false
        }
    }

    pub fn is_segment_register(&self) -> bool {
        match *self {
            Operand::RegSegment(_) => true,
            _ => false
        }
    }

    pub fn is_immediate(&self) -> bool {
        match *self {
            Operand::Immediate(_) | Operand::ImmediateLabel(_) => true,
            _ => false
        }
    }

    pub fn mode_number(&self) -> Option<u8> {
        match *self {
            Operand::Reg8(n) | Operand::Reg16(n) | Operand::RegSegment(n) => Some(n),
            Operand::RegisterMemory { mode_number, .. } => Some(mode_number),
            Operand::Immediate(_) | Operand::ImmediateLabel(_) => None
        }
    }
}

#[cfg(test)]
mod test {
    use crate::instruction::operand::{Operand, NumericOption};

    #[test]
    pub fn operand() {
        assert_eq!(Operand::from_str("ax"), Operand::Reg16(0));
        assert_eq!(Operand::from_str("ah"), Operand::Reg8(4));
        assert_eq!(Operand::from_str("es"), Operand::RegSegment(0));
        assert_eq!(Operand::from_str("100"), Operand::Immediate(100));
        assert_eq!(Operand::from_str("0x7C00"), Operand::Immediate(0x7C00));
        assert_eq!(Operand::from_str("7C00h"), Operand::Immediate(0x7C00));
        assert_eq!(Operand::from_str("0b10010000"), Operand::Immediate(0x90));
        assert_eq!(Operand::from_str("MY_VALUE"), Operand::ImmediateLabel("MY_VALUE".to_owned()));
        assert_eq!(Operand::from_str("[di]"), Operand::RegisterMemory { segment_override: None, word_flag: None, displacement: NumericOption::None, mode_number: 5});
        assert_eq!(Operand::from_str("[bx + 80]"), Operand::RegisterMemory { segment_override: None, word_flag: None, displacement: NumericOption::Number(80), mode_number: 7});
        assert_eq!(Operand::from_str("[si + MY_ADDRESS]"), Operand::RegisterMemory { segment_override: None, word_flag: None, displacement: NumericOption::Label("MY_ADDRESS".to_owned()), mode_number: 4});
        assert_eq!(Operand::from_str("es:w.[bp + 42]"), Operand::RegisterMemory { segment_override: Some(0), word_flag: Some(true), displacement: NumericOption::Number(42), mode_number: 6});
        assert_eq!(Operand::from_str("cs:b.[bx + si + 4]"), Operand::RegisterMemory { segment_override: Some(1), word_flag: Some(false), displacement: NumericOption::Number(4), mode_number: 0});
        assert_eq!(Operand::from_str("[144 + di + bp]"), Operand::RegisterMemory { segment_override: None, word_flag: None, displacement: NumericOption::Number(144), mode_number: 3});
    }
}