use crate::result::prelude::*;

#[derive(Clone, Debug)]
pub struct LinkerLabel {
    pub(self) location: Location,
    pub(self) name: String,
    pub(self) offset: i32
}
impl LinkerLabel {
    pub fn new<L, S>(location: L, label: S, offset: i32) -> Self where
        S: AsRef<str>,
        L: Locate {
        LinkerLabel {
            location: location.locate(),
            name: label.as_ref().to_owned(),
            offset
        }
    }

    pub fn name(&self) -> &str {
        &self.name[..]
    }

    pub fn offset(&self) -> i32 {
        self.offset
    }
}
impl Locate for LinkerLabel {
    fn locate(&self) -> Location {
        self.location
    }
}



#[derive(Clone, Debug)]
pub struct LinkerDeclaration {
    pub(self) location: Location,
    pub(self) name: String
}
impl LinkerDeclaration {
    pub fn new<L>(label: L) -> Self where
        L: AsRef<str> + Locate {
        LinkerDeclaration {
            location: label.locate(),
            name: label.as_ref().to_owned()
        }
    }

    pub fn name(&self) -> &str {
        &self.name[..]
    }
}
impl Locate for LinkerDeclaration {
    fn locate(&self) -> Location {
        self.location
    }
}
impl AsRef<str> for LinkerDeclaration {
    fn as_ref(&self) -> &str {
        &self.name
    }
}