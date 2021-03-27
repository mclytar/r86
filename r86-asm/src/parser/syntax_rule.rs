use std::fmt::Display;
use std::ops::Index;
use std::rc::Rc;

use crate::lexer::prelude::*;
use crate::result::prelude::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenRule {
    optional: bool,
    capture: bool,
    options: Vec<TokenClass>
}
impl TokenRule {
    pub fn new(options: Vec<TokenClass>) -> Self {
        TokenRule {
            optional: false,
            capture: false,
            options
        }
    }

    pub fn new_optional(options: Vec<TokenClass>) -> Self {
        TokenRule {
            optional: true,
            capture: false,
            options
        }
    }

    pub fn new_capture(options: Vec<TokenClass>) -> Self {
        TokenRule {
            optional: false,
            capture: true,
            options
        }
    }

    pub fn new_capture_optional(options: Vec<TokenClass>) -> Self {
        TokenRule {
            optional: true,
            capture: true,
            options
        }
    }

    pub fn capture(&self) -> bool {
        self.capture
    }

    pub fn optional(&self) -> bool {
        self.optional
    }

    pub fn is_match(&self, tc: TokenClass) -> bool {
        for opt in &self.options[..] {
            if *opt == tc {
                return true;
            }
        }
        false
    }
}
impl Display for TokenRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opt_count = self.options.len();
        match opt_count {
            0 => Ok(()),
            1 => write!(f, "{}", self.options[0]),
            2 => write!(f, "{} or {}", self.options[0], self.options[1]),
            _ => {
                for option in &self.options[0..opt_count - 2] {
                    write!(f, "{}, ", option)?;
                }
                write!(f, "{} or {}", self.options[opt_count - 2], self.options[opt_count - 1])
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SyntaxCapture {
    contents: Rc<String>,
    capture: Vec<Token>,
    location: Location
}
impl SyntaxCapture {
    pub(self) fn new(contents: Rc<String>, capture: Vec<Token>, location: Location) -> Self {
        SyntaxCapture {
            contents,
            capture,
            location
        }
    }

    pub fn amount(&self) -> usize {
        self.capture.len()
    }

    pub fn last(&self) -> &Token {
        self.capture.last().unwrap()
    }
}
impl Locate for SyntaxCapture {
    fn locate(&self) -> Location {
        self.location
    }
}
impl Index<usize> for SyntaxCapture {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.capture[index]
    }
}
impl Index<Location> for SyntaxCapture {
    type Output = str;

    fn index(&self, index: Location) -> &Self::Output {
        &self.contents[index]
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SyntaxRule {
    rule: Vec<TokenRule>
}
impl SyntaxRule {
    pub fn new(rule: Vec<TokenRule>) -> Self {
        SyntaxRule {
            rule
        }
    }

    pub fn capture(&self, contents: Rc<String>, candidate: &[Token]) -> Option<(usize, SyntaxCapture)> {
        let mut matches = Vec::new();
        let mut location: Option<Location> = None;
        let mut pos = 0;

        for i in 0..self.rule.len() {
            if let Some(candidate) = candidate.get(pos) {
                if self.rule[i].is_match(candidate.class()) {
                    if let Some(ref mut location) = location {
                        location.set_end(candidate.locate().end());
                    } else {
                        location = Some(candidate.locate());
                    }

                    if self.rule[i].capture() {
                        matches.push(candidate.to_owned());
                    }

                    pos += 1;
                } else if !self.rule[i].optional() {
                    return None;
                }
            } else {
                if !self.rule[i].optional() {
                    return None;
                }
            }
        }
        let location = location.expect("malformed syntax rule");
        Some((pos, SyntaxCapture::new(contents,matches, location)))
    }

    pub fn expect(&self, contents: Rc<String>, candidate: &[Token]) -> CompilerResult<(usize, SyntaxCapture)> {
        let mut matches = Vec::new();
        let mut location: Option<Location> = None;
        let mut pos = 0;

        for i in 0..self.rule.len() {
            if let Some(candidate) = candidate.get(pos) {
                if self.rule[i].is_match(candidate.class()) {
                    if let Some(ref mut location) = location {
                        location.set_end(candidate.locate().end());
                    } else {
                        location = Some(candidate.locate());
                    }

                    if self.rule[i].capture() {
                        matches.push(candidate.to_owned());
                    }

                    pos += 1;
                } else if !self.rule[i].optional() {
                    return Err(Notification::error_parser_expected_found(&candidate.locate(), &self.rule[i], "???"));
                }
            } else {
                if !self.rule[i].optional() {
                    unimplemented!("unexpected end of file");
                }
            }
        }
        let location = location.expect("malformed syntax rule");
        Ok((pos, SyntaxCapture::new(contents, matches, location)))
    }
}