#[macro_export]
macro_rules! token_rule {
    [ $item:ident ] => {
        $crate::parser::syntax_rule::TokenRule::new(vec![ $crate::lexer::token::TokenClass::$item ])
    };
    [ ( $( $item:ident )|+ ) ] => {
        $crate::parser::syntax_rule::TokenRule::new(vec![ $( $crate::lexer::token::TokenClass::$item ),+ ])
    };
    [ [ $( $item:ident )|+ ] ] => {
        $crate::parser::syntax_rule::TokenRule::new_optional(vec![ $( $crate::lexer::token::TokenClass::$item ),+ ])
    };
    [ { $( $item:ident )|+ } ] => {
        $crate::parser::syntax_rule::TokenRule::new_capture(vec![ $( $crate::lexer::token::TokenClass::$item ),+ ])
    };
    [ { [ $( $item:ident )|+ ] } ] => {
        $crate::parser::syntax_rule::TokenRule::new_capture_optional(vec![ $( $crate::lexer::token::TokenClass::$item ),+ ])
    };
}

#[macro_export]
macro_rules! syntax_rule {
    [ $( $token:tt )+ ] => {
        $crate::parser::syntax_rule::SyntaxRule::new(vec![ $( token_rule![ $token ] ),+ ])
    };
}

#[macro_export]
macro_rules! token {
    ( $str:literal ) => {{
        let mut context = $crate::context::Context::new("<test>", $str);
        let token_stream = $crate::lexer::token::TokenStream::tokenize(&mut context);
        let token: $crate::lexer::token::Token = token_stream.into_iter().next().unwrap();
        token
    }}
}
#[macro_export]
macro_rules! try_token {
    ( $str:literal ) => {{
        let mut context = $crate::context::Context::new("<test>", $str);
        let token_stream = $crate::lexer::token::TokenStream::tokenize(&mut context);
        let token: $crate::lexer::token::Token = token_stream.into_iter().next().unwrap();
        if context.is_err() {
            Err(())
        } else {
            Ok(token)
        }
    }}
}

//pub mod assembly;
pub mod compiler;
pub mod context;
pub mod lexer;
pub mod linker;
pub mod parser;
pub mod result;
//pub mod text;

const REG8: [&str; 8] = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
const REG16: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
const REG_SEGMENT: [&str; 4] = ["es", "cs", "ss", "ds"];
const INSTRUCTION: [&str; 110] = [
    // Data transfer
    "mov",
    "push",
    "pop",
    "xchg",
    "in",
    "out",
    "xlat",
    "lea",
    "lds",
    "les",
    "lahf",
    "sahf",
    "pushf",
    "popf",
    // Arithmetic
    "add",
    "adc",
    "inc",
    "aaa",
    "daa",
    "sub",
    "sbb",
    "dec",
    "neg",
    "cmp",
    "aas",
    "das",
    "mul",
    "imul",
    "aam",
    "div",
    "idiv",
    "aad",
    "cbw",
    "cwd",
    // Logic
    "not",
    "shl",
    "sal",
    "shr",
    "sar",
    "rol",
    "ror",
    "rcl",
    "rcr",
    "and",
    "test",
    "or",
    "xor",
    // String manipulation
    "rep",
    "movsb",
    "movsw",
    "cmpsb",
    "cmpsw",
    "scasb",
    "scasw",
    "lodsb",
    "lodsw",
    "stosb",
    "stosw",
    // Control Transfer
    "call",
    "jmp",
    "ret",
    "je",
    "jz",
    "jl",
    "jnge",
    "jle",
    "jng",
    "jb",
    "jnae",
    "jbe",
    "jna",
    "jp",
    "jpe",
    "jo",
    "js",
    "jne",
    "jnz",
    "jnl",
    "jge",
    "jnle",
    "jg",
    "jnb",
    "jae",
    "jnbe",
    "ja",
    "jnp",
    "jpo",
    "jno",
    "jns",
    "loop",
    "loopz",
    "loope",
    "loopnz",
    "loopne",
    "jcxz",
    "int",
    "into",
    "iret",
    // Processor control
    "clc",
    "cmc",
    "stc",
    "cld",
    "std",
    "cli",
    "sti",
    "hlt",
    "wait",
    "esc",
    "segment",
    // No operation
    "nop"
];
const PREFIX_INSTRUCTION: [&str; 6] = ["lock", "repne", "repnz", "rep", "repe", "repz"];
const PSEUDO_INSTRUCTION: [&str; 9] = ["db", "dw", "dd", "dq", "resb", "resw", "resd", "resq", "equ"];

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum Reg8 {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH
}
impl Reg8 {
    pub fn parse(reg: &str) -> Self {
        let reg_lc = reg.to_lowercase();
        match &reg_lc[..] {
            "al" => Reg8::AL,
            "cl" => Reg8::CL,
            "dl" => Reg8::DL,
            "bl" => Reg8::BL,
            "ah" => Reg8::AH,
            "ch" => Reg8::CH,
            "dh" => Reg8::DH,
            "bh" => Reg8::BH,
            _ => panic!("not an 8-bit register: `{}`", reg)
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum Reg16 {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI
}
impl Reg16 {
    pub fn parse(reg: &str) -> Self {
        let reg_lc = reg.to_lowercase();
        match &reg_lc[..] {
            "ax" => Reg16::AX,
            "cx" => Reg16::CX,
            "dx" => Reg16::DX,
            "bx" => Reg16::BX,
            "sp" => Reg16::SP,
            "bp" => Reg16::BP,
            "si" => Reg16::SI,
            "di" => Reg16::DI,
            _ => panic!("not a 16-bit register: `{}`", reg)
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum RegSegment {
    ES,
    CS,
    SS,
    DS
}
impl RegSegment {
    pub fn parse(reg: &str) -> Self {
        let reg_lc = reg.to_lowercase();
        match &reg_lc[..] {
            "es" => RegSegment::ES,
            "cs" => RegSegment::CS,
            "ss" => RegSegment::SS,
            "ds" => RegSegment::DS,
            _ => panic!("not a segment register: `{}`", reg)
        }
    }
}