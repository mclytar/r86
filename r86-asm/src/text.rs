#[macro_export]
macro_rules! ctx {
    ($lt:literal) => {
        &mut Context::from_contents($lt)
    }
}

use std::path::{Path, PathBuf};
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use crate::result::error::{CompilerNotificationList};
use crate::lexer::token::{TokenStream};

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct TextReference {
    filename: PathBuf,
    line: usize,
    column: usize,
    length: usize
}

pub struct Text {
    pub(crate) filename: PathBuf,
    pub(crate) contents: Rc<String>,
}
impl Text {
    pub fn from_file<S>(filename: S) -> Result<Self, std::io::Error> where
        S: AsRef<Path>
    {
        let mut file = File::open(filename.as_ref())?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let contents = Rc::new(contents);

        Ok(Text {
            filename: filename.as_ref().to_owned(),
            contents,
        })
    }

    pub fn from_contents<S>(contents: S) -> Self where
        S: AsRef<str>
    {
        Text {
            filename: PathBuf::from("<anonymous>"),
            contents: Rc::new(contents.as_ref().to_owned()),
        }
    }

    pub fn filename(&self) -> &Path {
        &self.filename
    }

    pub fn contents(&self) -> &str {
        &*self.contents
    }

    pub fn into_token_stream(self) -> Result<TokenStream, CompilerNotificationList> {
        TokenStream::tokenize(self)
    }
}