#[macro_export]
macro_rules! syntax_error {
    ($stream:ident[$n:literal], $lt:literal, $($expr1:expr,)* ref $(,$expr2:expr)*) => {
        error::syntax_error(
            format!($lt, $($expr1,)* $stream[$n].as_ref() $(,$expr2)*),
            $stream[$n].line(),
            $stream[$n].column(),
            $stream[$n].len()
        )
    };
    ($stream:ident[$n:literal], $lt:literal $(,$expr:expr)*) => {
        error::syntax_error(
            format!($lt $(,$expr)*),
            $stream[$n].line(),
            $stream[$n].column(),
            $stream[$n].len()
        )
    };
    ($stream:ident[$n:literal] was $old_stream:ident, $lt:literal, $($expr1:expr,)* ref $(,$expr2:expr)*) => {{
        let err = error::syntax_error(
            format!($lt, $($expr1,)* $stream[$n].as_ref() $(,$expr2)*),
            $stream[$n].line(),
            $stream[$n].column(),
            $stream[$n].len()
        );
        *$stream = $old_stream;
        err
    }};
    ($stream:ident[$n:literal] was $old_stream:ident, $lt:literal $(,$expr:expr)*) => {{
        let err = error::syntax_error(
            format!($lt $(,$expr)*),
            $stream[$n].line(),
            $stream[$n].column(),
            $stream[$n].len()
        );
        *$stream = $old_stream;
        err
    }};
}

pub mod expression;

use std::str::FromStr;

use crate::error::{error, CompilerResult, CompilerError};
use crate::token::{Token, TokenClass};
use crate::{INSTRUCTION, PREFIX_INSTRUCTION, PSEUDO_INSTRUCTION};
use expression::Expression;

macro_rules! is_instruction {
    ($token:expr) => {
        INSTRUCTION.iter().any(|s| *s == $token.as_ref()) || PREFIX_INSTRUCTION.iter().any(|s| *s == $token.as_ref()) || PSEUDO_INSTRUCTION.iter().any(|s| *s == $token.as_ref())
    };
    ($token:expr, - PREFIX) => {
        INSTRUCTION.iter().any(|s| *s == $token.as_ref()) || PSEUDO_INSTRUCTION.iter().any(|s| *s == $token.as_ref())
    }
}

#[derive(Clone, Debug)]
pub struct NumericLiteral<'a> {
    value: u16,
    token: Token<'a>
}
impl<'a> NumericLiteral<'a> {
    pub fn try_parse(token: &Token<'a>) -> CompilerResult<Option<Self>> {
        if token.class() != TokenClass::NumericLiteral {
            return Ok(None);
        }

        let number = token.as_ref()
            .replace('_', "");

        let result = if number.starts_with("0x") {
            u16::from_str_radix(&number[2..], 16)
        } else if number.ends_with(|c: char| c == 'h' || c == 'H') {
            u16::from_str_radix(&number[..number.len() - 1], 16)
        } else if number.starts_with("0b") {
            u16::from_str_radix(&number[2..], 2)
        } else {
            u16::from_str(&number[..])
        };

        match result {
            Ok(value) => {
                let token= token.to_owned();
                Ok(Some(NumericLiteral { value, token }))
            },
            Err(e) => {
                let err = error::parse_int_error(e, token.line(), token.column(), token.len());
                Err(err)
            }
        }
    }

    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Self>> {
        if stream.len() == 0 {
            return Ok(None);
        }

        let result = Self::try_parse(&stream[0])?;
        *stream = &stream[1..];
        Ok(result)
    }
}
impl<'a> Into<u16> for NumericLiteral<'a> {
    fn into(self) -> u16 {
        self.value
    }
}



/// Represents a statement of type `ORG <number>`.
#[derive(Clone, Debug)]
pub struct OriginStatement<'a> {
    value: u16,
    token: Token<'a>
}
impl<'a> OriginStatement<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Self>> {
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();
        match &classes[..] {
            [] => Ok(None),
            [TokenClass::Origin, TokenClass::NumericLiteral, TokenClass::EndOfLine] => {
                let value = NumericLiteral::try_parse(&stream[1])?.unwrap().into();
                let token = stream[1].clone();
                *stream = &stream[3..];
                Ok(Some(OriginStatement{ value, token }))
            },
            [TokenClass::Origin, TokenClass::NumericLiteral, ..]
            => Err(syntax_error!(stream[2], "unexpected `{}` at end of statement", ref)),
            [TokenClass::Origin, TokenClass::EndOfLine, ..]
            => Err(syntax_error!(stream[1], "expected number, found end of line")),
            [TokenClass::Origin, ..]
            => Err(syntax_error!(stream[1], "expected number, found `{}`", ref)),
            _ => Ok(None)
        }
    }
}



/// Type of the declaration.
///
/// Use `EXTERN` to import a symbol from another file and `GLOBAL` to export a symbol for use in other files.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum DeclarationKind {
    Global,
    Extern
}

/// Represents a statement of type `GLOBAL <ident>` or `EXTERN <ident>`.
#[derive(Clone, Debug)]
pub struct DeclarationStatement<'a> {
    kind: DeclarationKind,
    token: Token<'a>
}
impl<'a> DeclarationStatement<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Self>> {
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();
        match &classes[..] {
            [] => Ok(None),
            [TokenClass::Global, TokenClass::Ident, TokenClass::EndOfLine]
            | [TokenClass::Extern, TokenClass::Ident, TokenClass::EndOfLine] => {
                let kind = if stream[0].class() == TokenClass::Global {
                    DeclarationKind::Global
                } else {
                    DeclarationKind::Extern
                };
                let token = stream[1].clone();
                *stream = &stream[3..];
                Ok(Some(DeclarationStatement{ kind, token }))
            },
            [TokenClass::Global, TokenClass::Ident, ..]
            | [TokenClass::Extern, TokenClass::Ident, ..]
            => Err(syntax_error!(stream[2], "unexpected `{}` at end of statement", ref)),
            [TokenClass::Global, TokenClass::EndOfLine, ..]
            | [TokenClass::Extern, TokenClass::EndOfLine, ..]
            => Err(syntax_error!(stream[1], "expected identifier, found end of line")),
            [TokenClass::Global, ..]
            | [TokenClass::Extern, ..]
            => Err(syntax_error!(stream[1], "expected identifier, found `{}`", ref)),
            _ => Ok(None)
        }
    }

    pub fn kind(&self) -> DeclarationKind {
        self.kind
    }
}



/// Represents a statement of type `SECTION <ident>`.
#[derive(Clone, Debug)]
pub struct SectionStatement<'a> {
    token: Token<'a>
}
impl<'a> SectionStatement<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Self>> {
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();
        match &classes[..] {
            [] => Ok(None),
            [TokenClass::Section, TokenClass::Ident, TokenClass::EndOfLine] => {
                let token = stream[1].clone();
                *stream = &stream[3..];
                Ok(Some(SectionStatement{ token }))
            },
            [TokenClass::Section, TokenClass::Ident, ..]
            => Err(syntax_error!(stream[2], "unexpected `{}` at end of statement", ref)),
            [TokenClass::Section, TokenClass::EndOfLine, ..]
            => Err(syntax_error!(stream[1], "expected identifier, found end of line")),
            [TokenClass::Section, ..]
            => Err(syntax_error!(stream[1], "expected identifier, found `{}`", ref)),
            _ => Ok(None)
        }
    }
}



/// Represents a label definition.
#[derive(Clone, Debug)]
pub struct LabelDefinition<'a> {
    token: Token<'a>
}
impl<'a> LabelDefinition<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Self>> {
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(4).collect();
        match &classes[..] {
            [] => Ok(None),
            [TokenClass::Ident, TokenClass::Colon, TokenClass::EndOfLine, ..]
            => {
                let token = stream[0].clone();
                *stream = &stream[2..];
                Ok(Some(LabelDefinition { token }))
            },
            [TokenClass::Ident, TokenClass::EndOfLine, ..]
            => {
                if is_instruction!(stream[0]) {
                    return Ok(None);
                }
                let token = stream[0].clone();
                *stream = &stream[1..];
                Ok(Some(LabelDefinition { token }))
            },
            [TokenClass::Ident, TokenClass::Times, ..]
            => {
                let token = stream[0].clone();
                *stream = &stream[1..];
                Ok(Some(LabelDefinition { token }))
            },
            [TokenClass::Ident, TokenClass::Colon, TokenClass::Times, ..]
            => {
                let token = stream[0].clone();
                *stream = &stream[2..];
                Ok(Some(LabelDefinition { token }))
            },
            [TokenClass::Ident, TokenClass::Ident, ..]
            => {
                if is_instruction!(stream[0]) {
                    return Ok(None);
                }
                let token = stream[0].clone();
                *stream = &stream[1..];
                Ok(Some(LabelDefinition { token }))
            },
            [TokenClass::Ident, TokenClass::Colon, TokenClass::Ident, ..]
            => {
                let token = stream[0].clone();
                *stream = &stream[2..];
                Ok(Some(LabelDefinition { token }))
            },
            [TokenClass::Ident, ..] => if is_instruction!(stream[0]) {
                return Ok(None);
            } else {
                Err(syntax_error!(stream[1], "expected instruction, found `{}`", ref))
            }
            _ => Ok(None)
        }
    }
}



#[derive(Clone, Debug)]
pub struct Quantifier<'a> {
    expression: Expression<'a>,
    token: Token<'a>
}
impl<'a> Quantifier<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Quantifier<'a>>> {
        let stream_slice_preserve = &stream[..];
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();
        match &classes[..] {
            [] => Ok(None),
            [TokenClass::Times, ..]
            => {
                let token = stream[0].clone();
                *stream = &stream[1..];
                let expression = match Expression::try_accept_critical(stream) {
                    Ok(Some(expr)) => expr,
                    Ok(None) => return Err(syntax_error!(stream[0] was stream_slice_preserve, "expected expression, found `{}`", ref)),
                    Err(e) => {
                        *stream = stream_slice_preserve;
                        return Err(e);
                    }
                };
                let quantifier = Quantifier { expression, token };
                Ok(Some(quantifier))
            },
            _ => Ok(None)
        }
    }
}



#[derive(Clone, Debug)]
pub struct PrefixInstruction<'a> {
    token: Token<'a>
}
impl<'a> PrefixInstruction<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<PrefixInstruction<'a>>> {
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();
        match &classes[..] {
            [] => Ok(None),
            [TokenClass::EndOfLine, ..]
            => {
                Ok(None)
            },
            [TokenClass::Ident, TokenClass::EndOfLine, ..]
            | [TokenClass::Ident, TokenClass::Ident, ..]
            => {
                if PREFIX_INSTRUCTION.iter().any(|i| *i == stream[0].as_ref()) {
                    let token = stream[0].clone();
                    let instruction = PrefixInstruction { token };
                    *stream = &stream[1..];
                    Ok(Some(instruction))
                } else {
                    Ok(None)
                }
            },
            [TokenClass::Ident, ..]
            => {
                if is_instruction!(stream[0], - PREFIX) {
                    return Ok(None);
                }
                Err(syntax_error!(stream[0], "expected instruction, found `{}`", ref))
            }
            _ => Err(syntax_error!(stream[0], "expected instruction, found `{}`", ref))
        }
    }
}



/// Represents an operand.
#[derive(Clone, Debug)]
pub struct Operand<'a> {
    size: Option<Token<'a>>,
    address: bool,
    segment_override: Option<Token<'a>>,
    expression: Expression<'a>
}
impl<'a> Operand<'a> {
    pub fn try_accept<'b>(stream: &mut &'b [Token<'a>]) -> CompilerResult<Option<Operand<'a>>> {
        let stream_slice_preserve = &stream[..];

        if stream.len() == 0 {
            return Ok(None);
        }

        let size = match stream[0].class() {
            TokenClass::Byte | TokenClass::Word => {
                let size = stream[0].clone();
                *stream = &stream[1..];
                Some(size)
            },
            _ => None
        };
        let expect_expr = |s: &mut &'b [Token<'a>]| {
            match Expression::try_accept(s) {
                Ok(Some(expr)) => Ok(expr),
                Ok(None) => unimplemented!(),
                Err(err) => {
                    *s = stream_slice_preserve;
                    Err(err)
                }
            }
        };

        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();

        match &classes[..] {
            [] => Ok(None),
            [TokenClass::EndOfLine, ..] => Ok(None),
            [TokenClass::LeftSquareParen, ..] => {
                let segment_override = if &classes[1..] == [TokenClass::RegSegment, TokenClass::Colon] {
                    let token = stream[1].clone();
                    *stream = &stream[3..];
                    Some(token)
                } else {
                    *stream = &stream[1..];
                    None
                };
                let expression = expect_expr(stream)?;
                let classes: Vec<_> = stream.iter().map(|t| t.class()).take(2).collect();
                match &classes[..] {
                    [TokenClass::RightSquareParen, TokenClass::EndOfLine] => *stream = &stream[1..],
                    [TokenClass::RightSquareParen, TokenClass::Comma] => *stream = &stream[2..],
                    [] => {
                        *stream = stream_slice_preserve;
                        let err = error::syntax_error(
                            format!("expected `]`, found end of file"),
                            stream[stream.len() - 1].line(),
                            stream[stream.len() - 1].column() + stream[stream.len() - 1].len(),
                            1
                        );
                        return Err(err);
                    },
                    _ => return Err(syntax_error!(stream[0] was stream_slice_preserve, "expected `]`, found `{}`", ref))
                }
                if stream.len() > 0 && stream[0].class() == TokenClass::Comma {
                    *stream = &stream[1..];
                }
                let operand = Operand { size, address: true, segment_override, expression };
                Ok(Some(operand))
            },
            _ => {
                let segment_override = None;
                let expression = expect_expr(stream)?;
                let classes: Vec<_> = stream.iter().map(|t| t.class()).take(1).collect();
                match &classes[..] {
                    [TokenClass::EndOfLine] => {},
                    [TokenClass::Comma] => *stream = &stream[1..],
                    [] => {
                        *stream = stream_slice_preserve;
                        let err = error::syntax_error(
                            format!("expected instruction, found end of file"),
                            stream[stream.len() - 1].line(),
                            stream[stream.len() - 1].column() + stream[stream.len() - 1].len(),
                            1
                        );
                        return Err(err);
                    },
                    _ => return Err(syntax_error!(stream[0] was stream_slice_preserve, "expected instruction, found `{}`", ref))
                }
                if stream.len() > 0 && stream[0].class() == TokenClass::Comma {
                    *stream = &stream[1..];
                }
                let operand = Operand { size, address: false, segment_override, expression };
                Ok(Some(operand))
            }
        }
    }
}

/// Represents an instruction statement.
#[derive(Clone, Debug)]
pub struct InstructionStatement<'a> {
    token: Token<'a>,
    operands: Vec<Operand<'a>>,
}
impl<'a> InstructionStatement<'a> {
    pub fn try_accept<'b>(stream: &mut &'b [Token<'a>]) -> CompilerResult<(Option<LabelDefinition<'a>>, Option<Quantifier<'a>>, Option<PrefixInstruction<'a>>, Option<Self>)> {
        let stream_slice_preserve = &stream[..];

        let prelude = |s: &mut &'b [Token<'a>]| {
            let label = LabelDefinition::try_accept(s)?;
            let quantifier = Quantifier::try_accept(s)?;
            let prefix = PrefixInstruction::try_accept(s)?;

            Ok((label, quantifier, prefix))
        };
        let (label, quantifier, prefix) = match prelude(stream) {
            Ok(lqp) => lqp,
            Err(e) => {
                *stream = stream_slice_preserve;
                return Err(e);
            }
        };
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(3).collect();

        match &classes[..] {
            [] => Ok((label, quantifier, prefix, None)),
            [TokenClass::EndOfLine, ..]
            => {
                Ok((label, quantifier, prefix, None))
            },
            [TokenClass::Ident, TokenClass::EndOfLine, ..]
            => {
                if INSTRUCTION.iter().any(|i| *i == stream[0].as_ref()) {
                    let token = stream[0].clone();
                    let instruction = InstructionStatement { token, operands: Vec::new() };
                    *stream = &stream[2..];
                    Ok((label, quantifier, prefix, Some(instruction)))
                } else if prefix.is_none() && PSEUDO_INSTRUCTION.iter().any(|i| *i == stream[0].as_ref()) {
                    let token = stream[0].clone();
                    let instruction = InstructionStatement { token, operands: Vec::new() };
                    *stream = &stream[2..];
                    Ok((label, quantifier, prefix, Some(instruction)))
                } else {
                    Err(syntax_error!(stream[0] was stream_slice_preserve, "expected instruction, found `{}`", ref))
                }
            },
            [TokenClass::Ident, ..]
            => {
                let mut operands = Vec::new();
                let token = stream[0].clone();
                *stream = &stream[1..];

                while stream.len() > 0 {
                    match Operand::try_accept(stream) {
                        Ok(Some(operand)) => operands.push(operand),
                        Ok(None) => break,
                        Err(e) => {
                            *stream = stream_slice_preserve;
                            return Err(e);
                        }
                    }
                }

                let instruction = InstructionStatement { token, operands };
                Ok((label, quantifier, prefix, Some(instruction)))
            },
            _ => {
                Err(syntax_error!(stream[0] was stream_slice_preserve, "expected instruction, found `{}`", ref))
            }
        }
    }
}



#[derive(Clone, Debug)]
pub enum Statement<'a> {
    Origin(OriginStatement<'a>),
    Declaration(DeclarationStatement<'a>),
    Section(SectionStatement<'a>),
    Instruction {
        label: Option<LabelDefinition<'a>>,
        quantifier: Option<Quantifier<'a>>,
        prefix: Option<PrefixInstruction<'a>>,
        instruction: Option<InstructionStatement<'a>>
    }
}
impl<'a> Statement<'a> {
    pub fn try_accept(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Statement<'a>>> {
        let classes: Vec<_> = stream.iter().map(|t| t.class()).take(1).collect();

        match &classes[..] {
            [] => Ok(None),
            [TokenClass::EndOfLine] => {
                *stream = &stream[1..];
                Ok(None)
            },
            _ => {
                let statement = if let Some(origin) = OriginStatement::try_accept(stream)? {
                    Statement::Origin(origin)
                } else if let Some(declaration) = DeclarationStatement::try_accept(stream)? {
                    Statement::Declaration(declaration)
                } else if let Some(section) = SectionStatement::try_accept(stream)? {
                    Statement::Section(section)
                } else {
                    let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(stream)?;
                    Statement::Instruction {
                        label,
                        quantifier,
                        prefix,
                        instruction
                    }
                };
                Ok(Some(statement))
            }
        }
    }

    pub fn translate(mut stream: &[Token<'a>]) -> Result<Vec<Statement<'a>>, Vec<CompilerError>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while stream.len() > 0 {
            match Self::try_accept(&mut stream) {
                Ok(Some(statement)) => statements.push(statement),
                Ok(None) => {},
                Err(err) => {
                    errors.push(err);
                    if let Some(pos) = stream.iter().position(|s| s.class() == TokenClass::EndOfLine) {
                        stream = &stream[pos + 1..];
                    } else {
                        stream = &stream[stream.len()..];
                    }
                }
            }
        }

        if errors.len() == 0 {
            Ok(statements)
        } else {
            Err(errors)
        }
    }
}


#[cfg(test)]
mod test {
    use crate::token::TokenStream;
    use crate::statement::{OriginStatement, NumericLiteral, DeclarationStatement, DeclarationKind, SectionStatement, LabelDefinition, InstructionStatement, PrefixInstruction, Quantifier, Statement};

    #[test]
    fn numeric_literal() {
        let asm = r"0x7C_00";
        let stream = TokenStream::tokenize(asm).unwrap();
        let tokens = stream.as_ref();
        assert!(NumericLiteral::try_parse(&tokens[0]).unwrap().is_some());

        // TODO: complete test
    }

    #[test]
    fn origin() {
        let asm = r"ORG 0x7C00";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(OriginStatement::try_accept(&mut tokens).unwrap().unwrap().value, 0x7C00);
    }

    #[test]
    fn declaration() {
        let asm = r"GLOBAL main
        EXTERN print";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(DeclarationStatement::try_accept(&mut tokens).unwrap().unwrap().kind, DeclarationKind::Global);
        assert_eq!(DeclarationStatement::try_accept(&mut tokens).unwrap().unwrap().kind, DeclarationKind::Extern);
    }

    #[test]
    fn section() {
        let asm = r"SECTION .text";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(SectionStatement::try_accept(&mut tokens).unwrap().unwrap().token.as_ref(), ".text");
    }

    #[test]
    fn label() {
        let asm = r"table:";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(LabelDefinition::try_accept(&mut tokens).unwrap().unwrap().token.as_ref(), "table");

        let asm = r"loop:";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(LabelDefinition::try_accept(&mut tokens).unwrap().unwrap().token.as_ref(), "loop");

        let asm = r"loop";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(LabelDefinition::try_accept(&mut tokens).unwrap().is_none());
    }

    #[test]
    fn quantifier() {
        let asm = r"times 512 - ($ - $$)";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(Quantifier::try_accept(&mut tokens).unwrap().unwrap().token.as_ref(), "times");

        let asm = r"add bx, si";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Quantifier::try_accept(&mut tokens).unwrap().is_none());

        let asm = r"times";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Quantifier::try_accept(&mut tokens).is_err());

        let asm = r"times:";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Quantifier::try_accept(&mut tokens).is_err());
    }

    #[test]
    fn prefix() {
        let asm = r"repe";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert_eq!(PrefixInstruction::try_accept(&mut tokens).unwrap().unwrap().token.as_ref(), "repe");

        let asm = r"add";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(PrefixInstruction::try_accept(&mut tokens).unwrap().is_none());

        let asm = r"lock:";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(PrefixInstruction::try_accept(&mut tokens).is_err());
    }

    #[test]
    fn instruction() {
        let asm = r"stosw";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert!(label.is_none());
        assert!(quantifier.is_none());
        assert!(prefix.is_none());
        assert_eq!(instruction.unwrap().token.as_ref(), "stosw");

        let asm = r"lock";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert!(label.is_none());
        assert!(quantifier.is_none());
        assert_eq!(prefix.unwrap().token.as_ref(), "lock");
        assert!(instruction.is_none());

        let asm = r"loop: rep stosb";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert_eq!(label.unwrap().token.as_ref(), "loop");
        assert!(quantifier.is_none());
        assert_eq!(prefix.unwrap().token.as_ref(), "rep");
        assert_eq!(instruction.unwrap().token.as_ref(), "stosb");

        let asm = r"loop rep stosb";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(InstructionStatement::try_accept(&mut tokens).is_err());
    }

    #[test]
    fn instruction_with_args() {
        let asm = r"    inc ax";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert!(label.is_none());
        assert!(quantifier.is_none());
        assert!(prefix.is_none());
        let instruction = instruction.unwrap();
        assert_eq!(instruction.token.as_ref(), "inc");
        assert_eq!(instruction.operands.len(), 1);

        let asm = r"    add ax, cx";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert!(label.is_none());
        assert!(quantifier.is_none());
        assert!(prefix.is_none());
        let instruction = instruction.unwrap();
        assert_eq!(instruction.token.as_ref(), "add");
        assert_eq!(instruction.operands.len(), 2);

        let asm = r"    or WORD [es: bx + si + (caller_segment + 1) * 16 + caller_offset], 0b00010000";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert!(label.is_none());
        assert!(quantifier.is_none());
        assert!(prefix.is_none());
        let instruction = instruction.unwrap();
        assert_eq!(instruction.token.as_ref(), "or");
        assert_eq!(instruction.operands.len(), 2);
    }

    #[test]
    fn instruction_times() {
        let asm = r"push_n: TIMES (size + 1) / 2 lock push ax";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let (label, quantifier, prefix, instruction) = InstructionStatement::try_accept(&mut tokens).unwrap();
        assert_eq!(label.unwrap().token.as_ref(), "push_n");
        assert!(quantifier.is_some());
        assert_eq!(prefix.unwrap().token.as_ref(), "lock");
        let instruction = instruction.unwrap();
        assert_eq!(instruction.token.as_ref(), "push");
        assert_eq!(instruction.operands.len(), 1);
    }

    #[test]
    fn listing_ok() {
        let asm = r"
                    ORG 1000h
        _start:
                    jmp main
        my_byte     db  42
        my_word     dw  0x0102
        my_ptr      dw  0xB800
        main:
                    mov dl, [my_byte]
                    xor dh, dh
                    add [my_word], dx
                    hlt
        ";

        let stream = TokenStream::tokenize(asm).unwrap();
        let _ = Statement::translate(stream.as_ref()).unwrap();
    }
}