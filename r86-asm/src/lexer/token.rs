use std::fmt::Display;
use std::ops::Index;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::{REG8, REG16, REG_SEGMENT, INSTRUCTION, PSEUDO_INSTRUCTION, PREFIX_INSTRUCTION};
use crate::result::prelude::*;
use crate::parser::prelude::*;
use super::Assembly;


/// Defines the class of the token.
///
/// Any token must be among the ones described by this enumerator, otherwise it is invalid.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum TokenClass {
    /// Matches the end of the line, that is, `\n`, a comment followed by `\n`, the end of the string
    /// or a comment followed by the end of the string.
    EndOfLine,
    /// Matches the word `ORG`.
    ORG,
    /// Matches the word `EXTERN`.
    Extern,
    /// Matches the word `GLOBAL`.
    Global,
    /// Matches the word `SECTION`.
    Section,
    /// Matches the word `SEG`.
    SEG,
    /// Matches the word `WRT`.
    WRT,
    /// Matches the word `BYTE`.
    Byte,
    /// Matches the word `WORD`.
    Word,
    /// Matches the word `TIMES`.
    Times,
    /// Matches any 8-bit register.
    Reg8,
    /// Matches any 16-bit register.
    Reg16,
    /// Matches any segment register.
    RegSegment,
    /// Matches any instruction.
    Instruction,
    /// Matches any prefix instruction.
    PrefixInstruction,
    /// Matches any pseudo-instruction.
    PseudoInstruction,
    /// Matches an identifier.
    ///
    /// Identifiers may start with `.`, `_`, `@` or a letter and can contain any number of `.`, `_`, `@`, letters and numbers.
    Ident,
    /// Matches a numeric literal.
    IntegerLiteral,
    /// Matches a string literal.
    StringLiteral,
    /// Matches the token `$`.
    InstructionStart,
    /// Matches the token `$$`.
    SectionStart,
    /// Matches an operator.
    ///
    /// Currently supported operators are `+`, `-`, `*` and `/`.
    Operator,
    /// Matches the token `:`.
    Colon,
    /// Matches the token `,`.
    Comma,
    /// Matches the token `(`.
    LeftParen,
    /// Matches the token `)`.
    RightParen,
    /// Matches the token `[`.
    LeftSquareParen,
    /// Matches the token `]`.
    RightSquareParen,
}
impl Display for TokenClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenClass::*;
        let name = match *self {
            EndOfLine => "end of line",
            ORG => "`ORG`",
            Extern => "`EXTERN`",
            Global => "`GLOBAL`",
            Section => "`SECTION`",
            SEG => "`SEG`",
            WRT => "`WRT`",
            Byte => "`BYTE`",
            Word => "`WORD`",
            Times => "`TIMES`",
            Reg8 => "8-bit register",
            Reg16 => "16-bit register",
            RegSegment => "segment register",
            Instruction => "instruction",
            PrefixInstruction => "prefix instruction",
            PseudoInstruction => "pseudo-instruction",
            Ident => "identifier",
            IntegerLiteral => "number",
            StringLiteral => "string literal",
            InstructionStart => "`$`",
            SectionStart => "`$$`",
            Operator => "operator",
            Colon => "`:`",
            Comma => "`,`",
            LeftParen => "`(`",
            RightParen => "`)`",
            LeftSquareParen => "`[`",
            RightSquareParen => "`]`",
        };
        write!(f, "{}", name)
    }
}



/// Defines a token in a string.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Token {
    class: TokenClass,
    location: Location
}
impl Token {
    /// Creates a new `EndOfLine` token from the given location.
    pub fn end_of_line(location: Location) -> Token {
        Token { class: TokenClass::EndOfLine, location }
    }
    /// Tries to create a new token from the given text and the given location.
    pub fn expect(text: &str, location: &mut Location) -> CompilerResult<Option<Token>> {
        let line = location.line();
        let column = location.column();
        let start = location.start();

        if start == location.end() {
            return Ok(Some(Token { class: TokenClass::EndOfLine, location: *location }));
        }

        let s = &text[start..];
        let eof = text.len();

        let (end, class) = if s.starts_with(';') || s.starts_with('\n') {             // Comment
            let end = s.find('\n').map(|p| p + start + 1).unwrap_or(eof);
            (end, TokenClass::EndOfLine)
        } else if s.starts_with(|c: char| c.is_whitespace()) {                                      // Whitespace
            if let Some(pos) = s.find(|c: char| !c.is_whitespace() || c == '\n') {
                location.set_start(start + pos);
                location.set_column(column + pos);
            } else {
                location.set_start(eof);
            };
            return Ok(None);
        } else if s.starts_with(|c: char| c == '.' || c.is_alphabetic() || c == '_' || c == '@') {  // Identifier or keyword
            let pos = s.find(|c: char| c != '.' && !c.is_alphanumeric() && c != '_' && c != '@')
                .map(|pos| pos)
                .unwrap_or(eof);
            let end = start + pos;
            let class = match &s[..pos].to_lowercase()[..] {
                "org" => TokenClass::ORG,
                "extern" => TokenClass::Extern,
                "global" => TokenClass::Global,
                "section" => TokenClass::Section,
                "seg" => TokenClass::SEG,
                "wrt" => TokenClass::WRT,
                "byte" => TokenClass::Byte,
                "word" => TokenClass::Word,
                "times" => TokenClass::Times,
                token => {
                    if REG8.iter().any(|s| *s == token) {
                        TokenClass::Reg8
                    } else if REG16.iter().any(|s| *s == token) {
                        TokenClass::Reg16
                    } else if REG_SEGMENT.iter().any(|s| *s == token) {
                        TokenClass::RegSegment
                    } else if INSTRUCTION.iter().any(|s| *s == token) {
                        TokenClass::Instruction
                    } else if PREFIX_INSTRUCTION.iter().any(|s| *s == token) {
                        TokenClass::PrefixInstruction
                    } else if PSEUDO_INSTRUCTION.iter().any(|s| *s == token) {
                        TokenClass::PseudoInstruction
                    } else {
                        TokenClass::Ident
                    }
                }
            };
            (end, class)
        } else if s.starts_with(|c: char| c.is_numeric()) {                                         // Numeric literal
            let end = s.find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                .map(|pos| pos + start)
                .unwrap_or(eof);
            (end, TokenClass::IntegerLiteral)
        } else if s.starts_with(|c: char| c == '\'' || c == '"') {                                  // String literal
            unimplemented!()
        } else if s.starts_with('$') {                                                         // Dollar or DoubleDollar
            let end = s.find(|c: char| c != '$').map(|pos| pos + start).unwrap_or(eof);
            let class = match end - start {
                1 => TokenClass::InstructionStart,
                2 => TokenClass::SectionStart,
                _ => {
                    location.set_start(end);
                    let location = Location::new(line, column, start, end);
                    let err = Notification::error_lexer_too_many_dollars(&location);
                    return Err(err)
                }
            };
            (end, class)
        } else {
            let end = start + 1;
            let class = match s.chars().next().unwrap() {
                '+' | '-' | '*' | '/' => TokenClass::Operator,
                ':' => TokenClass::Colon,
                ',' => TokenClass::Comma,
                '(' => TokenClass::LeftParen,
                ')' => TokenClass::RightParen,
                '[' => TokenClass::LeftSquareParen,
                ']' => TokenClass::RightSquareParen,
                _ => {
                    let location = Location::new(line, column, start, end);
                    let err = Notification::error_lexer_illegal_symbol(&location);
                    return Err(err)
                }
            };
            (end, class)
        };

        let token = Token {
            class,
            location: Location::new(line, column, start, end)
        };

        location.set_start(end);
        if class == TokenClass::EndOfLine {
            location.set_line(line + 1);
            location.set_column(1);
        } else {
            location.set_column(column + end - start);
        }

        Ok(Some(token))
    }
    /// Returns the class of the token.
    pub fn class(&self) -> TokenClass {
        self.class
    }
}
impl Locate for Token {
    fn locate(&self) -> Location {
        self.location
    }
}



/// Defines a stream of tokens.
#[derive(Debug)]
pub struct TokenStream {
    pub(crate) filename: PathBuf,
    pub(crate) contents: Rc<String>,
    pub(crate) log: CompilerLog,
    pub(crate) stream: Vec<Token>,
    position: usize
}
impl TokenStream {
    /// Converts the input [`Text`] into a stream of tokens.
    ///
    /// # Errors
    /// Will return [`Err`] if an illegal token is encountered during the process.
    ///
    /// The [`Err`] variant contains a list of the detected errors.
    pub fn from_assembly(mut context: Assembly) -> Result<Self, CompilerLog> {
        let mut location = Location::new(1, 1, 0, context.as_str().len());
        let mut stream = Vec::new();

        while location.len() > 0 {
            match Token::expect(context.as_str(), &mut location) {
                Ok(Some(token)) => stream.push(token),
                Ok(None) => {},
                Err(err) => context.err(err)
            }
        }
        stream.push(Token::end_of_line(location));

        let Assembly { filename, contents, log } = context;

        if log.is_err() {
            Err(log)
        } else {
            Ok(TokenStream { filename, contents, log, stream, position: 0 })
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

    pub fn skip_line(&mut self) {
        if let Some(position) = self.stream[self.position..].iter()
            .position(|t| t.class() == TokenClass::EndOfLine) {
            self.position += position + 1;
        } else {
            self.position = self.stream.len();
        }
    }

    pub fn capture(&mut self, rule: SyntaxRule) -> Option<SyntaxCapture> {
        if let Some((len, capture)) = rule.capture(self.contents.clone(), &self.stream[self.position..]) {
            self.position += len;
            Some(capture)
        } else {
            None
        }
    }

    pub fn expect(&mut self, rule: SyntaxRule) -> CompilerResult<SyntaxCapture> {
        let (len, capture) = rule.expect(self.contents.clone(), &self.stream[self.position..])?;
        self.position += len;
        Ok(capture)
    }
    /// Checks if there are available tokens in the iterator.
    pub fn is_empty(&self) -> bool {
        self.position == self.stream.len()
    }

    pub fn parse(self) -> Result<Listing, CompilerLog> {
        Listing::from_token_stream(self.into_iter())
    }
}
impl AsRef<[Token]> for TokenStream {
    fn as_ref(&self) -> &[Token] {
        &self.stream[..]
    }
}
impl Index<usize> for TokenStream {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.stream[self.position + index]
    }
}
impl Index<Location> for TokenStream {
    type Output = str;

    fn index(&self, index: Location) -> &Self::Output {
        &self.contents[index]
    }
}
impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.stream.len() {
            let token = self.stream[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.stream.len() - self.position;
        (len, Some(len))
    }

    fn count(self) -> usize where
        Self: Sized, {
        self.stream.len() - self.position
    }

    fn last(self) -> Option<Self::Item> where
        Self: Sized, {
        let len = self.stream.len();
        if self.position < self.stream.len() {
            Some(self.stream[len - 1].clone())
        } else {
            None
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        if self.position + n < self.stream.len() {
            self.position += n;
            let token = self.stream[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            self.position = self.stream.len();
            None
        }
    }
}



#[cfg(test)]
mod test {
    use crate::lexer::token::TokenClass;

    #[test]
    fn single_token_macro() {
        let _ = token!("add");
        let _ = token!(",");
        let _ = token!("$$");
    }

    #[test]
    fn token_end_of_line() {
        assert_eq!(token!("").class(), TokenClass::EndOfLine);
        assert_eq!(token!("\n").class(), TokenClass::EndOfLine);
        assert_eq!(token!("; this is a comment").class(), TokenClass::EndOfLine);
    }

    #[test]
    fn token_keywords() {
        assert_eq!(token!("org").class(), TokenClass::ORG);
        assert_eq!(token!("extern").class(), TokenClass::Extern);
        assert_eq!(token!("global").class(), TokenClass::Global);
        assert_eq!(token!("section").class(), TokenClass::Section);
        // also test case-insensitive
        assert_eq!(token!("SEG").class(), TokenClass::SEG);
        assert_eq!(token!("wRt").class(), TokenClass::WRT);
        assert_eq!(token!("BYte").class(), TokenClass::Byte);
        assert_eq!(token!("word").class(), TokenClass::Word);
        assert_eq!(token!("TIMES").class(), TokenClass::Times);
    }

    #[test]
    fn token_reg8() {
        assert_eq!(token!("al").class(), TokenClass::Reg8);
        assert_eq!(token!("cl").class(), TokenClass::Reg8);
        assert_eq!(token!("dl").class(), TokenClass::Reg8);
        assert_eq!(token!("bl").class(), TokenClass::Reg8);
        // also test case-insensitive
        assert_eq!(token!("AH").class(), TokenClass::Reg8);
        assert_eq!(token!("Ch").class(), TokenClass::Reg8);
        assert_eq!(token!("dH").class(), TokenClass::Reg8);
        assert_eq!(token!("bh").class(), TokenClass::Reg8);
    }

    #[test]
    fn token_reg16() {
        assert_eq!(token!("ax").class(), TokenClass::Reg16);
        assert_eq!(token!("cx").class(), TokenClass::Reg16);
        assert_eq!(token!("dx").class(), TokenClass::Reg16);
        assert_eq!(token!("bx").class(), TokenClass::Reg16);
        // also test case-insensitive
        assert_eq!(token!("SP").class(), TokenClass::Reg16);
        assert_eq!(token!("Bp").class(), TokenClass::Reg16);
        assert_eq!(token!("sI").class(), TokenClass::Reg16);
        assert_eq!(token!("di").class(), TokenClass::Reg16);
    }

    #[test]
    fn token_reg_segment() {
        assert_eq!(token!("es").class(), TokenClass::RegSegment);
        assert_eq!(token!("cs").class(), TokenClass::RegSegment);
        // also test case-insensitive
        assert_eq!(token!("SS").class(), TokenClass::RegSegment);
        assert_eq!(token!("Ds").class(), TokenClass::RegSegment);
    }

    #[test]
    fn token_instruction() {
        assert_eq!(token!("add").class(), TokenClass::Instruction);
        assert_eq!(token!("loop").class(), TokenClass::Instruction);
        assert_eq!(token!("lock").class(), TokenClass::PrefixInstruction);
        assert_eq!(token!("resb").class(), TokenClass::PseudoInstruction);
        // also test case-insensitive
        assert_eq!(token!("XOR").class(), TokenClass::Instruction);
        assert_eq!(token!("Push").class(), TokenClass::Instruction);
        assert_eq!(token!("RepNZ").class(), TokenClass::PrefixInstruction);
        assert_eq!(token!("ResW").class(), TokenClass::PseudoInstruction);
    }

    #[test]
    fn token_ident() {
        assert_eq!(token!("my_label").class(), TokenClass::Ident);
        assert_eq!(token!(".my_label").class(), TokenClass::Ident);
        assert_eq!(token!("_start").class(), TokenClass::Ident);
        assert_eq!(token!("@main").class(), TokenClass::Ident);
    }

    #[test]
    fn token_numeric_literal() {
        // Base 10 integer
        assert_eq!(token!("1234").class(), TokenClass::IntegerLiteral);
        assert_eq!(token!("5_678").class(), TokenClass::IntegerLiteral);
        // Binary integer
        assert_eq!(token!("0b01010101").class(), TokenClass::IntegerLiteral);
        assert_eq!(token!("0b1001_0000").class(), TokenClass::IntegerLiteral);
        // Hex integer, `0x` notation
        assert_eq!(token!("0x1234").class(), TokenClass::IntegerLiteral);
        assert_eq!(token!("0x12_34").class(), TokenClass::IntegerLiteral);
        // Hex integer, `h`/`H` notation
        assert_eq!(token!("7C00h").class(), TokenClass::IntegerLiteral);
        assert_eq!(token!("7C_00h").class(), TokenClass::IntegerLiteral);
        assert_eq!(token!("0FFF0H").class(), TokenClass::IntegerLiteral);
        assert_eq!(token!("0_FF_F0H").class(), TokenClass::IntegerLiteral);
        // Not numbers
        // Numbers cannot start with letters
        assert_ne!(token!("C700h").class(), TokenClass::IntegerLiteral);
    }

    #[test]
    fn token_string_literal() {
        // Single quote
        assert_eq!(token!(r"'Hello, World!'").class(), TokenClass::StringLiteral);
        // Single quote with escape
        assert_eq!(token!(r"'Hello, Ma\'am!'").class(), TokenClass::StringLiteral);
        // Double quote
        // Single quote
        assert_eq!(token!(r#""Hello, World!""#).class(), TokenClass::StringLiteral);
        // Single quote with escape
        assert_eq!(token!(r#""Hello, \"World\"!"#).class(), TokenClass::StringLiteral);
    }

    #[test]
    fn token_relative_position() {
        assert_eq!(token!("$").class(), TokenClass::InstructionStart);
        assert_eq!(token!("$$").class(), TokenClass::SectionStart);
    }

    #[test]
    fn token_relative_position_disallowed() {
        assert!(try_token!("$$$").is_err());
        assert!(try_token!("$$$$").is_err());
        assert!(try_token!("$$$$$").is_err());
        assert!(try_token!("$$$\n").is_err());
    }

    #[test]
    fn token_single_symbol() {
        assert_eq!(token!("+").class(), TokenClass::Operator);
        assert_eq!(token!("-").class(), TokenClass::Operator);
        assert_eq!(token!("*").class(), TokenClass::Operator);
        assert_eq!(token!("/").class(), TokenClass::Operator);
        assert_eq!(token!(":").class(), TokenClass::Colon);
        assert_eq!(token!(",").class(), TokenClass::Comma);
        assert_eq!(token!("(").class(), TokenClass::LeftParen);
        assert_eq!(token!(")").class(), TokenClass::RightParen);
        assert_eq!(token!("[").class(), TokenClass::LeftSquareParen);
        assert_eq!(token!("]").class(), TokenClass::RightSquareParen);
    }

    #[test]
    fn token_single_symbol_disallowed() {
        assert!(try_token!("°").is_err());
        assert!(try_token!("§").is_err());
        assert!(try_token!("§°ç").is_err());
        assert!(try_token!("§@°\n").is_err());
    }
}