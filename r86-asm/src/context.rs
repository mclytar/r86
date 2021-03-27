use std::path::{PathBuf, Path};
use std::rc::Rc;

use crate::result::prelude::*;
use std::ops::Index;

pub struct Context {
    filename: PathBuf,
    contents: Rc<String>,
    notification_list: CompilerLog
}
impl Context {
    pub fn new<P, S>(filename: P, contents: S) -> Context where
        P: AsRef<Path>,
        S: AsRef<str> {
        let contents = Rc::new(String::from(contents.as_ref()));
        let mut notification_list = CompilerLog::new();
        notification_list.set_filename(&filename);
        notification_list.set_text(contents.clone());

        Context {
            filename: filename.as_ref().to_path_buf(),
            contents,
            notification_list
        }
    }

    pub fn filename(&self) -> &Path {
        &self.filename
    }

    pub fn contents(&self) -> Rc<String> {
        self.contents.clone()
    }

    pub fn notifications(&self) -> &CompilerLog {
        &self.notification_list
    }

    pub fn notifications_mut(&mut self) -> &mut CompilerLog {
        &mut self.notification_list
    }

    pub fn as_str(&self) -> &str {
        &self.contents[..]
    }

    pub fn is_err(&self) -> bool {
        self.notification_list.is_err()
    }
}
impl Index<Location> for Context {
    type Output = str;

    fn index(&self, index: Location) -> &Self::Output {
        &self.contents[index]
    }
}