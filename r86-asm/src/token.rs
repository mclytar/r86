use crate::{REG8, REG16, REG_SEGMENT};
use crate::error::{error, CompilerError};

/// Defines the class of the token.
///
/// Any token must be among the ones described by this enumerator, otherwise it is invalid.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum TokenClass {
    /// Matches the end of the line, that is, `\n`, a comment followed by `\n`, the end of the string
    /// or a comment followed by the end of the string.
    EndOfLine,
    /// Matches the word `ORG`.
    Origin,
    /// Matches the word `EXTERN`.
    Extern,
    /// Matches the word `GLOBAL`.
    Global,
    /// Matches the word `SECTION`.
    Section,
    /// Matches the word `SEG`.
    Segment,
    /// Matches the word `WRT`.
    WithReferenceTo,
    /// Matches the word `BYTE`.
    Byte,
    /// Matches the word `WORD`.
    Word,
    /// Matches the word `TIMES`.
    Times,
    /// Matches an 8-bit register.
    Reg8,
    /// Matches a 16-bit register.
    Reg16,
    /// Matches a segment register.
    RegSegment,
    /// Matches an identifier.
    ///
    /// Identifiers can start with `.`, `_`, `@` or a letter and can contain any number of `.`, `_`, `@`, letters and numbers.
    Ident,
    /// Matches a numeric literal.
    NumericLiteral,
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
impl TokenClass {
    pub fn into_expression(self) -> TokenExpression {
        TokenExpression::Terminal(self)
    }
}

/// Defines a token in a string.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Token<'a> {
    pub(self) class: TokenClass,
    pub(self) line: usize,
    pub(self) column: usize,
    pub(self) slice: &'a str
}
impl<'a> Token<'a> {
    /// Returns the class of the token.
    pub fn class(&self) -> TokenClass {
        self.class
    }
    /// Returns the line number in which the token appears in the original text.
    pub fn line(&self) -> usize {
        self.line
    }
    /// Returns the column number in which the token appears in the original text.
    pub fn column(&self) -> usize {
        self.column
    }
    /// Returns the length of the token.
    pub fn len(&self) -> usize {
        self.slice.len()
    }
}
impl<'a> AsRef<str> for Token<'a> {
    fn as_ref(&self) -> &str {
        self.slice
    }
}

/// Defines a stream of tokens.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct TokenStream<'a> {
    stream: Vec<Token<'a>>
}
impl<'a> TokenStream<'a> {
    /// Converts the input string into a stream of tokens.
    ///
    /// # Errors
    /// Will return [`Err`] if an illegal token is encountered during the process.
    ///
    /// The [`Err`] variant contains a list of the detected errors.
    pub fn tokenize(mut s: &'a str) -> Result<Self, Vec<CompilerError>> {
        let mut line = 1;
        let mut column = 1;

        let mut errors = Vec::new();
        let mut stream = Vec::new();

        while s.len() > 0 {
            // --------------------------------
            // $comment = ; (...) `\n`
            // $end_of_line = $comment | `\n`
            // $ident = {.A-Za-z_@} {.A-Za-z0-9_@}*
            // $literal = {0-9} {0-9A-Za-z}*
            // $operator = `+` | `-` | `*` | `/`
            // $hash = `#`
            // $left_paren = `(`
            // $right_paren = `)`
            // $left_square_paren = `[`
            // $right_square_paren = `]`
            // --------------------------------
            let token = if s.starts_with(';') {
                // --------------------------------
                // $comment
                // --------------------------------
                let class = TokenClass::EndOfLine;
                let slice = if let Some (pos) = s.find('\n') {
                    &s[..pos + 1]
                } else {
                    &s[..]
                };
                let token = Token { class, line, column, slice };
                line += 1;
                column = 1;
                s = &s[slice.len()..];
                token
            } else if s.starts_with('\n') {
                // --------------------------------
                // \n
                // --------------------------------
                let class = TokenClass::EndOfLine;
                let token = Token { class, line, column, slice: &s[..1] };
                line += 1;
                column = 1;
                s = &s[1..];
                token
            } else if s.starts_with(|c: char| c.is_whitespace()) {
                // --------------------------------
                // $whitespace (ignore)
                // --------------------------------
                if let Some (pos) = s.find(|c: char| !c.is_whitespace()) {
                    s = &s[pos..];
                    column += pos;
                } else {
                    s = &s[s.len()..];
                };
                continue;
            } else if s.starts_with(|c: char| c == '.' || c.is_alphabetic() || c == '_' || c == '@') {
                // --------------------------------
                // $ident or reserved word
                // --------------------------------
                let slice = if let Some (pos) = s.find(|c: char| c != '.' && !c.is_alphanumeric() && c != '_' && c != '@') {
                    &s[..pos]
                } else {
                    &s[..]
                };
                let class = match &slice.to_lowercase()[..] {
                    "org" => TokenClass::Origin,
                    "extern" => TokenClass::Extern,
                    "global" => TokenClass::Global,
                    "section" => TokenClass::Section,
                    "seg" => TokenClass::Segment,
                    "wrt" => TokenClass::WithReferenceTo,
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
                        } else {
                            TokenClass::Ident
                        }
                    }
                };
                let token = Token { class, line, column, slice };
                column += slice.len();
                s = &s[slice.len()..];
                token
            } else if s.starts_with(|c: char| c.is_numeric()) {
                // --------------------------------
                // $literal (numeric)
                // --------------------------------
                let class = TokenClass::NumericLiteral;
                let slice = if let Some (pos) = s.find(|c: char| !c.is_ascii_alphanumeric() && c != '_') {
                    &s[..pos]
                } else {
                    &s[..]
                };
                let token = Token { class, line, column, slice };
                column += slice.len();
                s = &s[slice.len()..];
                token
            } else if s.starts_with(|c: char| c == '\'' || c == '"') {
                // --------------------------------
                // $literal (string)
                // --------------------------------
                let _class = TokenClass::StringLiteral;
                unimplemented!()
            } else if s.starts_with('$') {
                let (class, slice) = match s.find(|c: char| c != '$') {
                    Some(0) => unreachable!(),
                    Some(1) => (TokenClass::InstructionStart, &s[..1]),
                    Some(2) => (TokenClass::SectionStart, &s[..2]),
                    Some(n) => {
                        errors.push(error::illegal_sequence(line, column, n)
                            .build_hint(format!("did you mean `$` or `$$`?")));
                        s = s.trim_start_matches(|c: char| c != '\n');
                        line += 1;
                        column = 1;
                        continue;
                    },
                    None => {
                        if s.len() > 2 {
                            errors.push(error::illegal_sequence(line, column, s.len())
                                .build_hint(format!("did you mean `$` or `$$`?")));
                            s = s.trim_start_matches(|_: char| true);
                            continue;
                        }
                        if s.len() == 1 {
                            (TokenClass::InstructionStart, s)
                        } else {
                            (TokenClass::SectionStart, s)
                        }
                    }
                };
                let token = Token { class, line, column, slice };
                column += slice.len();
                s = &s[slice.len()..];
                token
            } else {
                let class = match s.chars().next().unwrap() {
                    '+' | '-' | '*' | '/' => TokenClass::Operator,
                    ':' => TokenClass::Colon,
                    ',' => TokenClass::Comma,
                    '(' => TokenClass::LeftParen,
                    ')' => TokenClass::RightParen,
                    '[' => TokenClass::LeftSquareParen,
                    ']' => TokenClass::RightSquareParen,
                    _ => {
                        let err = error::illegal_symbol(line, column);
                        errors.push(err);
                        s = s.trim_start_matches(|c: char| c != '\n');
                        line += 1;
                        column = 1;
                        continue;
                    }
                };
                let token = Token { class, line, column, slice: &s[..1] };
                column += 1;
                s = &s[1..];
                token
            };

            stream.push(token);
        }
        stream.push(Token { class: TokenClass::EndOfLine, line, column, slice: s });

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(TokenStream { stream })
        }
    }

    pub fn classes(self) -> Vec<TokenClass> {
        self.stream.iter()
            .map(|t| t.class)
            .collect()
    }
}
impl<'a> AsRef<[Token<'a>]> for TokenStream<'a> {
    fn as_ref(&self) -> &[Token<'a>] {
        &self.stream[..]
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum TokenExpression {
    Terminal(TokenClass),
    Variant(Vec<TokenExpression>),
    Sequence(Vec<TokenExpression>),
    Optional(Box<TokenExpression>),
}
impl TokenExpression {
    pub fn get<'a, 'b>(&self, stream: &mut &'b [Token<'a>]) -> Option<&'b [Token<'a>]> {
        if stream.len() == 0 {
            return None;
        }

        let mut matched_stream = *stream;

        match *self {
            TokenExpression::Terminal(c) => if matched_stream[0].class == c {
                matched_stream = &matched_stream[..1];
                *stream = &stream[1..];
                Some(matched_stream)
            } else {
                None
            },
            TokenExpression::Variant(ref v) => {
                for expr in v.iter() {
                    if let Some(m) = expr.get(&mut matched_stream) {
                        *stream = &stream[m.len()..];
                        return Some(m);
                    }
                }
                None
            },
            TokenExpression::Sequence(ref v) => {
                let len = matched_stream.len();
                for expr in v.iter() {
                    if expr.get(&mut matched_stream).is_none() {
                        return None;
                    }
                }
                let len = len - matched_stream.len();
                let result = &stream[..len];
                *stream = &stream[len..];
                Some(result)
            }
            _ => unimplemented!()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::{TokenStream, TokenClass};

    #[test]
    fn test_end_of_line() {
        assert_eq!(TokenStream::tokenize("; this is a comment").unwrap().classes(), vec![TokenClass::EndOfLine, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("; this is a comment\n").unwrap().classes(), vec![TokenClass::EndOfLine, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("\n").unwrap().classes(), vec![TokenClass::EndOfLine, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("").unwrap().classes(), vec![TokenClass::EndOfLine]);
    }

    #[test]
    fn test_ident() {
        assert_eq!(TokenStream::tokenize("add").unwrap().classes(), vec![TokenClass::Ident, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize(".MyLabel").unwrap().classes(), vec![TokenClass::Ident, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize(".L0").unwrap().classes(), vec![TokenClass::Ident, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("__@SPECIAL_LABEL").unwrap().classes(), vec![TokenClass::Ident, TokenClass::EndOfLine]);
    }

    #[test]
    fn test_literal() {
        assert_eq!(TokenStream::tokenize("13").unwrap().classes(), vec![TokenClass::NumericLiteral, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("0x02").unwrap().classes(), vec![TokenClass::NumericLiteral, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("0b0101_1010").unwrap().classes(), vec![TokenClass::NumericLiteral, TokenClass::EndOfLine]);
        // TODO:
        //assert_eq!(TokenStream::tokenize("'hello'").unwrap().classes(), vec![TokenClass::Literal, TokenClass::EndOfLine]);
    }

    #[test]
    fn test_start() {
        assert_eq!(TokenStream::tokenize("$").unwrap().classes(), vec![TokenClass::InstructionStart, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("$$").unwrap().classes(), vec![TokenClass::SectionStart, TokenClass::EndOfLine]);
        assert!(TokenStream::tokenize("$$$").is_err());
    }

    #[test]
    fn test_single_symbol() {
        assert_eq!(TokenStream::tokenize("+").unwrap().classes(), vec![TokenClass::Operator, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("-").unwrap().classes(), vec![TokenClass::Operator, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("*").unwrap().classes(), vec![TokenClass::Operator, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("/").unwrap().classes(), vec![TokenClass::Operator, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize(":").unwrap().classes(), vec![TokenClass::Colon, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize(",").unwrap().classes(), vec![TokenClass::Comma, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("(").unwrap().classes(), vec![TokenClass::LeftParen, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize(")").unwrap().classes(), vec![TokenClass::RightParen, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("[").unwrap().classes(), vec![TokenClass::LeftSquareParen, TokenClass::EndOfLine]);
        assert_eq!(TokenStream::tokenize("]").unwrap().classes(), vec![TokenClass::RightSquareParen, TokenClass::EndOfLine]);
    }

    #[test]
    fn test_listing() {
        let listing = r"
    main:
        nop
        add     WORD [es: bx + si + (StrSeg + 1) * 16], 0x08
        ; Nothing useful here
    loop: jmp     loop
        ";

        let classes = TokenStream::tokenize(listing).unwrap().classes();
        let expect = vec![TokenClass::EndOfLine,
                          TokenClass::Ident, TokenClass::Colon, TokenClass::EndOfLine,
                          TokenClass::Ident, TokenClass::EndOfLine,
                          TokenClass::Ident, TokenClass::Word, TokenClass::LeftSquareParen,
                          TokenClass::RegSegment, TokenClass::Colon, TokenClass::Reg16, TokenClass::Operator, TokenClass::Reg16, TokenClass::Operator,
                          TokenClass::LeftParen, TokenClass::Ident, TokenClass::Operator, TokenClass::NumericLiteral, TokenClass::RightParen,
                          TokenClass::Operator, TokenClass::NumericLiteral, TokenClass::RightSquareParen, TokenClass::Comma, TokenClass::NumericLiteral, TokenClass::EndOfLine,
                          TokenClass::EndOfLine,
                          TokenClass::Ident, TokenClass::Colon, TokenClass::Ident, TokenClass::Ident, TokenClass::EndOfLine,
                          TokenClass::EndOfLine
        ];

        assert_eq!(classes, expect);
    }
}