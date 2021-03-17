use std::fmt::{Display, Formatter, Debug};
use std::error::Error;

pub type CompilerResult<T> = std::result::Result<T, CompilerError>;

pub mod error {
    use super::*;

    pub fn illegal_symbol(line: usize, column: usize) -> CompilerError {
        CompilerError {
            line: Some(line),
            column: Some(column),
            length: None,
            description: Some(String::from("illegal symbol in token")),
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(CompilerErrorKind::IllegalSymbolInToken)
        }
    }

    pub fn illegal_sequence(line: usize, column: usize, length: usize) -> CompilerError {
        CompilerError {
            line: Some(line),
            column: Some(column),
            length: Some(length),
            description: Some(String::from("illegal sequence in token")),
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(CompilerErrorKind::IllegalSymbolInToken)
        }
    }

    pub fn parse_int_error(error: std::num::ParseIntError, line: usize, column: usize, length: usize) -> CompilerError {
        let description = Some(format!("{}", error));
        let kind = CompilerErrorKind::ParseIntError;
        let error = Box::new(error);
        CompilerError {
            line: Some(line),
            column: Some(column),
            length: Some(length),
            description,
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::External(Box::new(CustomError { kind, error}))
        }
    }

    pub fn syntax_error(description: String, line: usize, column: usize, length: usize) -> CompilerError {
        CompilerError {
            line: Some(line),
            column: Some(column),
            length: Some(length),
            description: Some(description),
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(CompilerErrorKind::SyntaxError)
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CompilerErrorKind {
    IllegalSymbolInToken,
    LexerError,
    SyntaxError,
    ParseIntError,
    InvalidNumberFormat,
    Unimplemented
}
impl CompilerErrorKind {
    pub(self) fn as_str(&self) -> &'static str {
        match *self {
            CompilerErrorKind::IllegalSymbolInToken => "illegal symbol in token",
            CompilerErrorKind::LexerError => "lexer error",
            CompilerErrorKind::SyntaxError => "syntax error",
            CompilerErrorKind::ParseIntError => "integer parse error",
            CompilerErrorKind::InvalidNumberFormat => "invalid number format",
            CompilerErrorKind::Unimplemented => "functionality not yet implemented"
        }
    }
}

#[derive(Debug)]
struct CustomError {
    pub(self) kind: CompilerErrorKind,
    pub(self) error: Box<dyn Error + Send + Sync>,
}

enum ErrorSource {
    Internal(CompilerErrorKind),
    External(Box<CustomError>)
}
impl Debug for ErrorSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            ErrorSource::External(ref c) => Debug::fmt(c, f),
            ErrorSource::Internal(kind) => f.debug_tuple("Kind").field(&kind).finish()
        }
    }
}

pub struct CompilerError {
    pub(self) line: Option<usize>,
    pub(self) column: Option<usize>,
    pub(self) length: Option<usize>,
    pub(self) description: Option<String>,
    pub(self) help: Option<String>,
    pub(self) notes: Vec<String>,
    pub(self) hint: Option<String>,
    pub(self) source: ErrorSource
}
impl CompilerError {
    pub fn new<E>(kind: CompilerErrorKind, error: E) -> Self
        where
            E: Into<Box<dyn Error + Send + Sync>>
    {
        CompilerError {
            line: None,
            column: None,
            length: None,
            description: None,
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::External(Box::new(CustomError {
                kind,
                error: error.into()
            }))
        }
    }

    pub fn get_ref(&self) -> Option<&(dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::External(ref c) => Some(&*c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn get_mut(&mut self) -> Option<&mut (dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::External(ref mut c) => Some(&mut *c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn into_inner(self) -> Option<Box<dyn Error + Send + Sync>> {
        match self.source {
            ErrorSource::External(c) => Some(c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn line(&self) -> Option<usize> {
        self.line
    }

    pub fn column(&self) -> Option<usize> {
        self.column
    }

    pub fn length(&self) -> Option<usize> {
        self.length
    }

    pub fn kind(&self) -> Option<CompilerErrorKind> {
        match self.source {
            ErrorSource::External(ref c) => Some(c.kind),
            ErrorSource::Internal(kind) => Some(kind)
        }
    }

    pub fn description(&self) -> String {
        if let Some(ref d) = self.description {
            d.to_owned()
        } else {
            self.to_string()
        }
    }

    pub fn build_hint(mut self, hint: String) -> Self {
        self.help = Some(hint);
        self
    }

    pub fn help(&self) -> Option<&String> {
        self.help.as_ref()
    }

    pub fn notes(&self) -> &[String] {
        &self.notes[..]
    }

    pub fn hint(&self) -> Option<&String> {
        self.hint.as_ref()
    }
}
impl From<CompilerErrorKind> for CompilerError {
    fn from(kind: CompilerErrorKind) -> Self {
        CompilerError {
            line: None,
            column: None,
            length: None,
            description: None,
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(kind)
        }
    }
}
impl From<(CompilerErrorKind, usize)> for CompilerError {
    fn from((kind, line): (CompilerErrorKind, usize)) -> Self {
        CompilerError {
            line: Some(line),
            column: None,
            length: None,
            description: None,
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(kind)
        }
    }
}
impl From<(CompilerErrorKind, usize, usize)> for CompilerError {
    fn from((kind, line, column): (CompilerErrorKind, usize, usize)) -> Self {
        CompilerError {
            line: Some(line),
            column: Some(column),
            length: None,
            description: None,
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(kind)
        }
    }
}
impl From<(CompilerErrorKind, usize, usize, usize)> for CompilerError {
    fn from((kind, line, column, length): (CompilerErrorKind, usize, usize, usize)) -> Self {
        CompilerError {
            line: Some(line),
            column: Some(column),
            length: Some(length),
            description: None,
            help: None,
            notes: Vec::new(),
            hint: None,
            source: ErrorSource::Internal(kind)
        }
    }
}
impl Debug for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.source, f)
    }
}
impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.source {
            ErrorSource::External(ref c) => Display::fmt(&c.error, f),
            ErrorSource::Internal(kind) => write!(f, "{}", kind.as_str())
        }
    }
}
impl Error for CompilerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.source {
            ErrorSource::Internal(_) => None,
            ErrorSource::External(ref c) => c.error.source()
        }
    }
}