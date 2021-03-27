#![macro_use]

use std::hash::{Hash, Hasher};
use std::str::FromStr;

use crate::lexer::prelude::*;
use crate::result::prelude::*;

#[derive(Clone, Debug)]
pub struct IntegerLiteral {
    value: i32,
    location: Location
}
impl IntegerLiteral {
    pub fn parse<S>(number: S) -> Result<i32, std::num::ParseIntError> where
        S: AsRef<str>
    {
        let number = number.as_ref().replace('_', "");

        let result = if number.starts_with("0x") {
            i32::from_str_radix(&number[2..], 16)?
        } else if number.ends_with(|c: char| c == 'h' || c == 'H') {
            i32::from_str_radix(&number[..number.len() - 1], 16)?
        } else if number.starts_with("0b") {
            i32::from_str_radix(&number[2..], 2)?
        } else {
            i32::from_str(&number[..])?
        };

        Ok(result)
    }

    pub fn capture(token_stream: &mut TokenStream) -> CompilerResult<Option<IntegerLiteral>> {
        if let Some(capture) = token_stream.capture(syntax_rule![ { IntegerLiteral } ]) {
            let location = capture.locate();
            let value = match IntegerLiteral::parse(&capture[location]) {
                Ok(value) => value,
                Err(_) => return Err(Notification::error_parser_invalid_number_format(location, &capture[location]))
            };
            Ok(Some( IntegerLiteral { value, location }))
        } else {
            Ok(None)
        }
    }

    pub fn expect(token_stream: &mut TokenStream) -> CompilerResult<IntegerLiteral> {
        let capture = token_stream.expect(syntax_rule![ { IntegerLiteral } ])?;
        let location = capture.locate();
        let value = match IntegerLiteral::parse(&capture[location]) {
            Ok(value) => value,
            Err(_) => return Err(Notification::error_parser_invalid_number_format(location, &capture[location]))
        };
        Ok( IntegerLiteral { value, location })
    }

    pub fn expect_at<L>(token_stream: &mut TokenStream, location: L) -> CompilerResult<IntegerLiteral> where
        L: Locate {
        let location = location.locate();
        let value = match IntegerLiteral::parse(&token_stream[location]) {
            Ok(value) => value,
            Err(_) => return Err(Notification::error_parser_invalid_number_format(location, &token_stream[location]))
        };
        Ok( IntegerLiteral { value, location })
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}
impl Locate for IntegerLiteral {
    fn locate(&self) -> Location {
        self.location
    }
}
impl PartialEq for IntegerLiteral {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for IntegerLiteral {}
impl Hash for IntegerLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}