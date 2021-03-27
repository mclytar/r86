#![macro_use]

pub mod expression;
pub mod instruction;
pub mod label;
pub mod literal;

use std::path::PathBuf;
use std::rc::Rc;

use crate::compiler::prelude::*;
use crate::lexer::prelude::*;
use crate::result::prelude::*;
use expression::Expression;
use instruction::Instruction;
use label::Label;
use literal::IntegerLiteral;

#[derive(Clone, Debug)]
pub enum StatementKind {
    Origin(IntegerLiteral),
    Global(Label),
    Extern(Label),
    Section(Label),
    Label(Label),
    Instruction(Instruction)
}

#[derive(Clone, Debug)]
pub struct Statement {
    location: Location,
    kind: StatementKind
}
impl Statement {
    pub fn expect(token_stream: &mut TokenStream) -> CompilerResult<Option<Statement>> {
        if token_stream.is_empty() {
            Ok(None)
        } else if token_stream.capture(syntax_rule![EndOfLine]).is_some() {
            Ok(None)
        } else if let Some(capture) = token_stream.capture(syntax_rule![ORG]) {
            // `ORG xxxx`
            let location = capture.locate();
            let number = IntegerLiteral::expect(token_stream)?;
            let location = location | token_stream.expect(syntax_rule![EndOfLine])?.locate();
            Ok(Some(Statement { location, kind: StatementKind::Origin(number) }))
        } else if let Some(capture) = token_stream.capture(syntax_rule![{ Global | Extern | Section }]) {
            // `GLOBAL name`
            // `EXTERN name`
            // `SECTION name`
            let location = capture.locate();
            let class = capture[0].class();
            let label = Label::expect(token_stream)?;
            let location = location | token_stream.expect(syntax_rule![EndOfLine])?.locate();
            match class {
                TokenClass::Global => Ok(Some(Statement { location, kind: StatementKind::Global(label) })),
                TokenClass::Extern => Ok(Some(Statement { location, kind: StatementKind::Extern(label) })),
                TokenClass::Section => Ok(Some(Statement { location, kind: StatementKind::Section(label) })),
                _ => unreachable!()
            }
        } else {
            // Actual instruction/pseudo-instruction
            let label = Label::capture_definition(token_stream);
            if label.is_some() && token_stream.capture(syntax_rule![EndOfLine]).is_some() {
                let label = label.unwrap();
                return Ok(Some(Statement { location: label.locate(), kind: StatementKind::Label(label) }));
            }
            let quantifier = if token_stream.capture(syntax_rule![Times]).is_some() {
                Some(Expression::expect(token_stream)?)
            } else {
                None
            };
            let mut instruction = Instruction::expect(token_stream)?;
            if let Some(label) = label {
                instruction.set_label(label);
            }
            if let Some(quantifier) = quantifier {
                instruction.set_quantifier(quantifier);
            }
            let location = instruction.locate();
            Ok(Some(Statement { location, kind: StatementKind::Instruction(instruction) }))
        }
    }

    pub fn kind(&self) -> &StatementKind {
        &self.kind
    }
}

#[derive(Debug)]
pub struct Listing {
    pub(crate) filename: PathBuf,
    pub(crate) contents: Rc<String>,
    pub(crate) log: CompilerLog,
    pub(crate) stream: Vec<Token>,
    pub(crate) statements: Vec<Statement>
}
impl Listing {
    pub fn from_token_stream(mut token_stream: TokenStream) -> Result<Self, CompilerLog> {
        let mut statements = Vec::new();

        while !token_stream.is_empty() {
            match Statement::expect(&mut token_stream) {
                Ok(Some(statement)) => {
                    statements.push(statement);
                },
                Ok(None) => {},
                Err(err) => {
                    token_stream.err(err);
                    token_stream.skip_line();
                }
            }
        }

        let TokenStream { filename, contents, log, stream, .. } = token_stream;

        if log.is_err() {
            Err(log)
        } else {
            Ok(Listing { filename, contents, log, stream, statements })
        }
    }

    pub fn compile(self) -> Result<UnlinkedBinary, CompilerLog> {
        UnlinkedBinary::from_listing(self)
    }
}