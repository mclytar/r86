pub mod token;

pub mod prelude {
    pub use super::token::Token;
    pub use super::token::TokenClass;
    pub use super::token::TokenStream;
}

use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::result::prelude::*;
use token::TokenStream;

pub struct Assembly {
    pub(self) filename: PathBuf,
    pub(self) contents: Rc<String>,
    pub(self) log: CompilerLog
}
impl Assembly {
    pub fn new<P, S>(filename: P, contents: S) -> Assembly where
        P: AsRef<Path>,
        S: AsRef<str> {
        let contents = Rc::new(String::from(contents.as_ref()));
        let mut log = CompilerLog::new();
        log.set_filename(&filename);
        log.set_text(contents.clone());

        Assembly {
            filename: filename.as_ref().to_path_buf(),
            contents,
            log
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

    pub fn tokenize(self) -> Result<TokenStream, CompilerLog> {
        TokenStream::from_assembly(self)
    }

}