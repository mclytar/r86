#![macro_use]

use std::hash::{Hash, Hasher};

use crate::result::prelude::*;
use crate::lexer::prelude::*;
use crate::compiler::binary::BinaryModule;

#[derive(Clone, Debug)]
pub struct Label {
    name: String,
    location: Location
}
impl Label {
    pub fn capture(token_stream: &mut TokenStream) -> Option<Label> {
        if let Some(capture) = token_stream.capture(syntax_rule![ { Ident | Instruction | PrefixInstruction | PseudoInstruction } ]) {
            let location = capture.locate();
            let name = token_stream[location].to_owned();
            Some( Label { name, location })
        } else {
            None
        }
    }

    pub fn expect(token_stream: &mut TokenStream) -> CompilerResult<Label> {
        let capture = token_stream.expect(syntax_rule![ { Ident | Instruction | PrefixInstruction | PseudoInstruction } ])?;
        let location = capture.locate();
        let name = token_stream[location].to_owned();
        Ok( Label { name, location })
    }

    pub fn expect_at<L>(token_stream: &mut TokenStream, location: L) -> CompilerResult<Label> where
        L: Locate {
        let location = location.locate();
        let name = token_stream[location].to_owned();
        Ok( Label { name, location })
    }

    pub fn capture_definition(token_stream: &mut TokenStream) -> Option<Label> {
        if let Some(capture) = token_stream.capture(syntax_rule![ { Ident | Instruction | PrefixInstruction | PseudoInstruction } Colon ]) {
            let location = capture[0].locate();
            let name = token_stream[location].to_owned();
            Some(Label { name, location })
        } else if let Some(capture) = token_stream.capture(syntax_rule![ { Ident } ]) {
            let location = capture.locate();
            let name = token_stream[location].to_owned();
            if !token_stream.is_empty() && token_stream[0].class() == TokenClass::EndOfLine {
                token_stream.warn(Notification::warning_label_alone_without_colon(location));
            }
            Some(Label { name, location })
        } else {
            None
        }
    }

    pub fn try_eval(&self, binary: &BinaryModule) -> Option<i32> {
        for section in binary.sections() {
            for label in section.vars() {
                if label.name() == self.name {
                    return Some(label.offset())
                }
            }
        }
        None
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}
impl AsRef<str> for Label {
    fn as_ref(&self) -> &str {
        &self.name
    }
}
impl Locate for Label {
    fn locate(&self) -> Location {
        self.location
    }
}
impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Label {}
impl Hash for Label {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}