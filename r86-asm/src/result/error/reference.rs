use std::path::{PathBuf, Path};
use crate::lexer::token::Token;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Reference {
    pub(self) line: usize,
    pub(self) column: usize,
    pub(self) length: usize
}
impl Reference {
    pub fn new_anonymous(line: usize, column: usize, length: usize) -> Self {
        Reference {
            line,
            column,
            length
        }
    }

    pub fn from_token(token: &Token) -> ReferenceBuilder {
        ReferenceBuilder {
            filename: None,
            line: Some(token.line()),
            column: Some(token.column()),
            length: None,
            start: Some(token.start()),
            end: Some(token.end())
        }
    }

    pub fn build() -> ReferenceBuilder {
        ReferenceBuilder {
            filename: None,
            line: None,
            column: None,
            length: None,
            start: None,
            end: None
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn len(&self) -> usize {
        self.length
    }
}

#[derive(Debug)]
pub struct ReferenceBuilder {
    pub(self) filename: Option<PathBuf>,
    pub(self) line: Option<usize>,
    pub(self) column: Option<usize>,
    pub(self) length: Option<usize>,
    pub(self) start: Option<usize>,
    pub(self) end: Option<usize>
}
impl ReferenceBuilder {
    pub fn filename<P>(mut self, filename: P) -> Self where
        P: AsRef<Path>
    {
        self.filename = Some(filename.as_ref().to_path_buf());
        self
    }

    pub fn line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }

    pub fn column(mut self, column: usize) -> Self {
        self.column = Some(column);
        self
    }

    pub fn length(mut self, length: usize) -> Self {
        self.length = Some(length);
        self
    }

    pub fn start(mut self, start: usize) -> Self {
        self.start = Some(start);
        self
    }

    pub fn end(mut self, end: usize) -> Self {
        self.end = Some(end);
        self
    }

    pub fn finish(self) -> Reference {
        let length = match (self.length, self.start, self.end) {
            (Some(length), None, None) => length,
            (None, Some(start), Some(end)) => end - start,
            (None, None, None) => 0,
            (None, Some(_), None) => panic!("`start` with missing `end`"),
            (None, None, Some(_)) => panic!("`end` with missing `start`"),
            (Some(_), _, _) => panic!("`length` and `(start, end)` pair are incompatible")
        };
        Reference {
            line: self.line.unwrap_or(0),
            column: self.column.unwrap_or(0),
            length
        }
    }
}