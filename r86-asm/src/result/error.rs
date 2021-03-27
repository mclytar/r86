//pub mod reference;

use std::fmt::{Display, Formatter, Debug};
use std::error::Error;
use std::ops::{Index, BitOr, BitOrAssign};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use console::Style;

pub type CompilerResult<T> = std::result::Result<T, Notification>;



/// Defines a bright cyan text, used for styling help messages.
pub fn help_style() -> Style {
    Style::new()
        .cyan()
        .bright()
}

/// Defines a bright yellow text, used for styling warning messages.
pub fn warning_style() -> Style {
    Style::new()
        .yellow()
        .bright()
}

/// Defines a bright green text, used for styling success messages.
pub fn success_style() -> Style {
    Style::new()
        .green()
        .bright()
}

/// Defines a bright red text, used for styling error messages.
pub fn error_style() -> Style {
    Style::new()
        .red()
        .bright()
}

/// Defines a bright white text, used for styling important parts of messages.
pub fn white_style() -> Style {
    Style::new()
        .white()
        .bright()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Location {
    line: usize,
    column: usize,
    start: usize,
    end: usize
}
impl Location {
    pub fn new(line: usize, column: usize, start: usize, end: usize) -> Location {
        Location {
            line,
            column,
            start,
            end
        }
    }

    pub fn set_line(&mut self, line: usize) {
        self.line = line;
    }

    pub fn set_column(&mut self, column: usize) {
        self.column = column
    }

    pub fn set_start(&mut self, start: usize) {
        self.start = start
    }

    pub fn set_end(&mut self, end: usize) {
        self.end = end;
    }
}
impl Locate for Location {
    fn locate(&self) -> Location {
        *self
    }

    fn line(&self) -> usize {
        self.line
    }

    fn column(&self) -> usize {
        self.column
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn len(&self) -> usize {
        self.end - self.start
    }
}
impl BitOr for Location {
    type Output = Location;

    fn bitor(self, rhs: Self) -> Self::Output {
        let line = std::cmp::min(self.line, rhs.line);
        let column = std::cmp::min(self.column, rhs.column);
        let start = std::cmp::min(self.start, rhs.start);
        let end = std::cmp::max(self.end, rhs.end);
        Location { line, column, start, end }
    }
}
impl BitOrAssign for Location {
    fn bitor_assign(&mut self, rhs: Self) {
        self.line = std::cmp::min(self.line, rhs.line);
        self.column = std::cmp::min(self.column, rhs.column);
        self.start = std::cmp::min(self.start, rhs.start);
        self.end = std::cmp::max(self.end, rhs.end);
    }
}

pub trait Locate {
    fn locate(&self) -> Location;
    fn line(&self) -> usize {
        self.locate().line()
    }
    fn column(&self) -> usize {
        self.locate().column()
    }
    fn start(&self) -> usize {
        self.locate().start()
    }
    fn end(&self) -> usize {
        self.locate().end()
    }
    fn len(&self) -> usize {
        self.end() - self.start()
    }
}
impl Index<Location> for String {
    type Output = str;

    fn index(&self, index: Location) -> &Self::Output {
        &self[index.start()..index.end()]
    }
}
impl<T> Locate for &T where
    T: Locate {
    fn locate(&self) -> Location {
        (*self).locate()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NotificationWarningKind {

}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NotificationErrorKind {
    Unimplemented,
    LexerIllegalSymbol,
    LexerTooManyDollars,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NotificationKind {
    Warning,
    Error
}
/*impl ToString for NotificationKind {
    fn to_string(&self) -> String {
        use NotificationKind::{Warning as W, Error as E};
        use NotificationErrorKind as Error;
        let description = match *self {
            W => "",
            E(Error::Unimplemented) => "this feature is currently unimplemented",
            E(Error::LexerIllegalSymbol) => "illegal symbol in token",
            E(Error::LexerTooManyDollars) => "illegal sequence of `$`"
        };
        String::from(description)
    }
}*/

#[derive(Clone, Eq, PartialEq)]
pub enum NotificationLine {
    MainLine { style: Style, location: Location, note: Option<String> },
    MainLineWithHelp { style: Style, location: Location, note: Option<String>, help_locations: Vec<Location>, help_note: Option<String> },
    SecondaryFileSpecification { filename: PathBuf, line_number: usize, column: usize },
    Footnote { contents: String }
}
impl NotificationLine {
    /*for line in self.lines.iter() {
            let caption = if let Some(ref caption) = line.caption {
                &caption[..]
            } else {
                ""
            };
            let (l, c, len) = line.coords();
            let source = source.lines()
                .skip(l - 1)
                .next()
                .unwrap();
            if let Some(last_line) = last_line {
                if last_line != line.coords().0 && last_line != line.coords().0 + 1 {
                    println!("{}",style("...").cyan().bright());
                }
            }
            println!("{:width$} {}", "", style("|").cyan().bright(), width = l_width);
            println!("{:width$} {} {}", style(l).cyan().bright(), style("|").cyan().bright(), source, width = l_width);
            println!("{:width$} {}{}{} {}",
                     "",
                     style("|").cyan().bright(),
                     " ".repeat(c),
                     line.style().apply_to(if *line.style() == help_style() { "-" } else { "^" }.repeat(len)),
                     line.style().apply_to(caption),
                     width = l_width
            );
            last_line = Some(line.coords().0)
        }
        for remark in self.remarks.iter() {
            let mut remark_lines = remark.iter();
            if let Some(line) = remark_lines.next() {
                println!("{:width$} {} {}","", style("=").cyan().bright(), line, width = l_width);
            }
            for line in remark_lines {
                println!("{:width$}   {}", "", line, width = l_width);
            }
        }*/

    pub fn display(&self, width: usize, text: &String, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NotificationLine::MainLine { style, location, note } => {
                let line = text.lines().nth(location.line() - 1).unwrap();
                writeln!(f, "{:width$} {}", "", help_style().apply_to("|"), width = width)?;
                writeln!(f, "{:width$} {} {}", help_style().apply_to(location.line()), help_style().apply_to("|"), line, width = width)?;
                let underline_symbol = if *style == error_style() || *style == warning_style() {
                    "^"
                } else {
                    "-"
                };
                if let Some(note) = note {
                    writeln!(f, "{:width$} {}{}{} {}",
                             "",
                             help_style().apply_to("|"),
                             " ".repeat(location.column()),
                             style.apply_to(underline_symbol.repeat(location.len())),
                             style.apply_to(note),
                             width = width
                    )?;
                } else {
                    writeln!(f, "{:width$} {}{}{}",
                             "",
                             help_style().apply_to("|"),
                             " ".repeat(location.column()),
                             style.apply_to(underline_symbol.repeat(location.len())),
                             width = width
                    )?;
                }
            },
            NotificationLine::MainLineWithHelp { style, location, note, help_locations, help_note } => {
                let line = text.lines().nth(location.line() - 1).unwrap();
                writeln!(f, "{:width$} {}", "", help_style().apply_to("|"), width = width)?;
                writeln!(f, "{:width$} {} {}", help_style().apply_to(location.line()), help_style().apply_to("|"), line, width = width)?;
                let underline_symbol = if *style == error_style() || *style == warning_style() {
                    "^"
                } else {
                    "-"
                };
                if let Some(note) = note {
                    writeln!(f, "{:width$} {}{}{} {}",
                             "",
                             help_style().apply_to("|"),
                             " ".repeat(location.column()),
                             style.apply_to(underline_symbol.repeat(location.len())),
                             style.apply_to(note),
                             width = width
                    )?;
                } else {
                    writeln!(f, "{:width$} {}{}{}",
                             "",
                             help_style().apply_to("|"),
                             " ".repeat(location.column()),
                             style.apply_to(underline_symbol.repeat(location.len())),
                             width = width
                    )?;
                }
            },
            _ => unimplemented!()
        }
        writeln!(f)
    }

    pub fn line_number(&self) -> usize {
        match self {
            NotificationLine::MainLine { location, .. } => location.line(),
            NotificationLine::MainLineWithHelp { location, .. } => location.line(),
            NotificationLine::SecondaryFileSpecification { line_number, .. } => *line_number,
            NotificationLine::Footnote { .. } => 0
        }
    }
}



#[derive(Clone, Eq, PartialEq)]
pub struct Notification {
    kind: NotificationKind,
    description: String,
    line_number: usize,
    column: usize,
    filename: Option<PathBuf>,
    text: Option<Rc<String>>,
    lines: Vec<NotificationLine>
}
impl Notification {
    pub fn error_unimplemented<L>(location: L) -> Self where
        L: Locate {
        let kind = NotificationKind::Error;
        let description = String::from("this feature is currently unimplemented");
        let line_number = location.line();
        let column = location.column();
        let lines = vec![NotificationLine::MainLine { style: error_style(), location: location.locate(), note: None }];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_lexer_illegal_symbol<L>(location: L) -> Self where
        L: Locate {
        let kind = NotificationKind::Error;
        let description = String::from("illegal symbol in token");
        let line_number = location.line();
        let column = location.column();
        let lines = vec![NotificationLine::MainLine { style: error_style(), location: location.locate(), note: None }];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_lexer_too_many_dollars<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_parser_expected_found<L, E, F>(location: L, expected: E, found: F) -> Self where
        L: Locate,
        E: ToString,
        F: ToString {
        let kind = NotificationKind::Error;
        let description = if found.to_string().ends_with('\n') {
            format!("expected {}, found end of line", expected.to_string())
        } else {
            format!("expected {}, found `{}`", expected.to_string(), found.to_string())
        };
        let line_number = location.line();
        let column = location.column();
        let lines = vec![NotificationLine::MainLine { style: error_style(), location: location.locate(), note: Some(format!("expected {}", expected.to_string())) }];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_parser_invalid_number_format<L, F>(_location: L, _format: F) -> Self where
        L: Locate,
        F: ToString {
        unimplemented!()
    }

    pub fn error_parser_wrong_register_in_expression<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_parser_mismatched_left_paren<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_parser_mismatched_right_paren<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_parser_non_scalar_product<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_parser_invalid_combination_of_registers<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_parser_non_scalar_division<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_division_by_zero<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_critical_expression_evaluation<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_invalid_expression<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn error_duplicate_definition<S, F, L>(name: S, first: F, location: L) -> Self where
        S: ToString,
        F: Locate,
        L: Locate {
        let kind = NotificationKind::Error;
        let description = format!("duplicate definitions with name `{}`", name.to_string());
        let line_number = location.line();
        let column = location.column();
        let lines = vec![
            NotificationLine::MainLine { style: help_style(), location: first.locate(), note: Some(format!("previous definition of `{}` here", name.to_string())) },
            NotificationLine::MainLine { style: error_style(), location: location.locate(), note: Some(String::from("duplicate definition")) }
        ];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_operands_amount<L, OP>(location: L, expected: usize, found: usize, operands_location: &Vec<OP>) -> Self where
        L: Locate,
        OP: Locate {
        let plural = |amount: usize| { if amount == 1 { "" } else { "s"} };
        let kind = NotificationKind::Error;
        let description = format!("this instruction takes {} operand{} but {} operand{} were supplied", expected, plural(expected), found, plural(found));
        let line_number = location.line();
        let column = location.column();
        let help_locations = operands_location.iter().map(|op| op.locate()).collect();
        let lines = vec![
            NotificationLine::MainLineWithHelp {
                style: error_style(),
                location: location.locate(),
                note: Some(format!("requires {} operand{}", expected, plural(expected))),
                help_locations,
                help_note: Some(format!("supplied {} operand{}", found, plural(found)))
            }
        ];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_operands_range<L, OP>(location: L, expected: std::ops::Range<usize>, found: usize, operands_location: &Vec<OP>) -> Self where
        L: Locate,
        OP: Locate {
        let plural = |amount: usize| { if amount == 1 { "" } else { "s"} };
        let kind = NotificationKind::Error;
        let (description, note) = match (expected.start, expected.end) {
            (0, max) => (
                format!("this instruction takes at most {} operand{} but {} operand{} were supplied", max - 1, plural(max - 1), found, plural(found)),
                Some(format!("requires at most {} operand{}", max, plural(max)))
            ),
            (min, usize::MAX) => (
                format!("this instruction takes at least {} operand{} but {} operand{} were supplied", min, plural(min), found, plural(found)),
                Some(format!("requires at least {} operand{}", min, plural(min)))
            ),
            (min, max) => (
                format!("this instruction takes from {} to {} operands but {} operand{} were supplied", min, max - 1, found, plural(found)),
                Some(format!("requires from {} to {} operands", min, max))
            )
        };
        let line_number = location.line();
        let column = location.column();
        let help_locations = operands_location.iter().map(|op| op.locate()).collect();
        let lines = vec![
            NotificationLine::MainLineWithHelp {
                style: error_style(),
                location: location.locate(),
                note,
                help_locations,
                help_note: Some(format!("supplied {} operand{}", found, plural(found)))
            }
        ];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_incompatible_operand_size<L1, L2>(op1: L1, op2: L2) -> Self where
        L1: Locate,
        L2: Locate {
        let kind = NotificationKind::Error;
        let description = format!("incompatible operands size");
        let line_number = op1.line();
        let column = op1.column();
        let lines = vec![
            NotificationLine::MainLine {
                style: error_style(),
                location: op1.locate() | op2.locate(),
                note: None
            }
        ];

        Notification { kind, description, line_number, column, filename: None, text: None, lines }
    }

    pub fn error_invalid_set_of_operands<L, OP>(_location: L, _operands_location: &Vec<OP>) -> Self where
        L: Locate,
        OP: Locate {
        unimplemented!()
    }

    pub fn warning_label_alone_without_colon<L>(_location: L) -> Self where
        L: Locate {
        unimplemented!()
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn set_filename<P>(&mut self, filename: P) where
        P: AsRef<Path> {
        self.filename = Some(filename.as_ref().to_path_buf());
    }

    pub fn filename(&mut self) -> Option<&PathBuf> {
        self.filename.as_ref()
    }

    pub fn set_text(&mut self, text: Rc<String>)  {
        self.text = Some(text);
    }

    pub fn text(&mut self) -> Option<&str> {
        self.text.as_ref().map(|s| &s[..])
    }
}
impl Debug for Notification {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let filename = if let Some(ref filename) = self.filename {
            filename.to_string_lossy()
        } else {
            std::borrow::Cow::Borrowed("<anonymous>")
        };
        write!(f, "{}:{}:{}", filename, self.line_number, self.column)?;
        if f.alternate() {
            writeln!(f, ": {} ({:#?})", self.description, &self.kind)
        } else {
            write!(f, ": {} ({:?})", self.description, &self.kind)
        }
    }
}
impl Display for Notification {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let filename = if let Some(ref filename) = self.filename {
            filename.to_string_lossy()
        } else {
            std::borrow::Cow::Borrowed("<anonymous>")
        };
        let sample = self.lines.iter().map(|l| l.line_number()).max().unwrap_or(0);
        let width = format!("{}", sample).len();
        match self.kind {
            NotificationKind::Warning => {
                if f.alternate() {
                    writeln!(f, "{}: {}", warning_style().apply_to("warning"), white_style().apply_to(&self.description))?;
                    writeln!(f, "{:width$}{} {}:{}:{}", "", help_style().apply_to("-->"), filename, self.line_number, self.column, width = width)?;
                    let text = self.text.clone().unwrap();
                    for line in self.lines.iter() {
                        line.display(width, text.as_ref(), f)?;
                    }
                } else {
                    return writeln!(f, "warning: {}", self.description);
                }
            },
            NotificationKind::Error => {
                if f.alternate() {
                    writeln!(f, "{}: {}", error_style().apply_to("error"), white_style().apply_to(&self.description))?;
                    writeln!(f, "{:width$}{} {}:{}:{}", "", help_style().apply_to("-->"), filename, self.line_number, self.column, width = width)?;
                    let text = self.text.clone().unwrap();
                    for line in self.lines.iter() {
                        line.display(width, text.as_ref(), f)?;
                    }
                } else {
                    return writeln!(f, "error: {}", self.description);
                }
            }
        }
        Ok(())
    }
}
impl Error for Notification {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}



/*#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CompilerErrorKind {
    Warning,
    IllegalSymbolInToken,
    LexerError,
    SyntaxError,
    ParseIntError,
    InvalidNumberFormat,
    InsufficientOperands,
    TooManyOperands,
    SymbolRedefinition,
    Unimplemented
}
impl CompilerErrorKind {
    pub(self) fn as_str(&self) -> &'static str {
        match *self {
            CompilerErrorKind::Warning => "warning",
            CompilerErrorKind::IllegalSymbolInToken => "illegal symbol in token",
            CompilerErrorKind::LexerError => "lexer error",
            CompilerErrorKind::SyntaxError => "syntax error",
            CompilerErrorKind::ParseIntError => "integer parse error",
            CompilerErrorKind::InvalidNumberFormat => "invalid number format",
            CompilerErrorKind::InsufficientOperands => "insufficient number of operands",
            CompilerErrorKind::TooManyOperands => "too many operands",
            CompilerErrorKind::SymbolRedefinition => "symbol redefinition",
            CompilerErrorKind::Unimplemented => "functionality not yet implemented"
        }
    }
}

#[derive(Debug)]
struct CustomError {
    pub(self) kind: CompilerErrorKind,
    pub(self) error: Box<dyn Error + Send + Sync>,
}

enum ErrorSource {
    Internal(CompilerErrorKind),
    External(Box<CustomError>)
}
impl Debug for ErrorSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            ErrorSource::External(ref c) => Debug::fmt(c, f),
            ErrorSource::Internal(kind) => f.debug_tuple("Kind").field(&kind).finish()
        }
    }
}


pub struct DisplayLine {
    line: usize,
    column: usize,
    length: usize,
    caption: Option<String>,
    style: Style
}
impl DisplayLine {
    pub fn new(line: usize, column: usize, length: usize, style: Style) -> Self {
        DisplayLine {
            line,
            column,
            length,
            caption: None,
            style
        }
    }

    pub fn from_reference(reference: &Reference, style: Style) -> Self {
        DisplayLine {
            line: reference.line(),
            column: reference.column(),
            length: reference.len(),
            caption: None,
            style
        }
    }

    pub fn with_caption(line: usize, column: usize, length: usize, style: Style, caption: String) -> Self {
        DisplayLine {
            line,
            column,
            length,
            caption: Some(caption),
            style
        }
    }

    pub fn from_reference_with_caption(reference: &Reference, style: Style, caption: String) -> Self {
        DisplayLine {
            line: reference.line(),
            column: reference.column(),
            length: reference.len(),
            caption: Some(caption),
            style
        }
    }

    pub fn coords(&self) -> (usize, usize, usize) {
        (self.line, self.column, self.length)
    }

    pub fn caption(&self) -> Option<&String> {
        self.caption.as_ref()
    }

    pub fn style(&self) -> &Style {
        &self.style
    }
}
pub struct DisplayBlock {
    filename: PathBuf,
    line: usize,
    column: usize,
    lines: Vec<DisplayLine>,
    remarks: Vec<Vec<String>>
}
impl DisplayBlock {
    pub fn new<S>(filename: S, line: usize, column: usize) -> Self where
        S: AsRef<Path>
    {
        DisplayBlock {
            filename: filename.as_ref().to_owned(),
            line,
            column,
            lines: Vec::new(),
            remarks: Vec::new()
        }
    }

    pub fn build_line(mut self, line: DisplayLine) -> Self {
        self.lines.push(line);

        self
    }

    pub fn add_line(&mut self, line: DisplayLine) {
        self.lines.push(line);
    }

    pub fn build_remark<S>(mut self, text: &[S]) -> Self where
        S: AsRef<str>
    {
        let remark = text.iter()
            .map(|s| s.as_ref().to_owned())
            .collect();
        self.remarks.push(remark);

        self
    }

    pub fn add_remark<S>(&mut self, text: &[S]) where
        S: AsRef<str>
    {
        let remark = text.iter()
            .map(|s| s.as_ref().to_owned())
            .collect();
        self.remarks.push(remark);
    }

    pub fn print(&self, source: &str) {
        println!(" {} {}:{}:{}", style("-->").cyan().bright(), self.filename.to_string_lossy(), self.line, self.column);
        let l_width = format!("{}", self.lines.iter()
            .map(|l| l.coords().0)
            .max().unwrap_or(0)).len();
        let mut last_line = None;
        for line in self.lines.iter() {
            let caption = if let Some(ref caption) = line.caption {
                &caption[..]
            } else {
                ""
            };
            let (l, c, len) = line.coords();
            let source = source.lines()
                .skip(l - 1)
                .next()
                .unwrap();
            if let Some(last_line) = last_line {
                if last_line != line.coords().0 && last_line != line.coords().0 + 1 {
                    println!("{}",style("...").cyan().bright());
                }
            }
            println!("{:width$} {}", "", style("|").cyan().bright(), width = l_width);
            println!("{:width$} {} {}", style(l).cyan().bright(), style("|").cyan().bright(), source, width = l_width);
            println!("{:width$} {}{}{} {}",
                     "",
                     style("|").cyan().bright(),
                     " ".repeat(c),
                     line.style().apply_to(if *line.style() == help_style() { "-" } else { "^" }.repeat(len)),
                     line.style().apply_to(caption),
                     width = l_width
            );
            last_line = Some(line.coords().0)
        }
        for remark in self.remarks.iter() {
            let mut remark_lines = remark.iter();
            if let Some(line) = remark_lines.next() {
                println!("{:width$} {} {}","", style("=").cyan().bright(), line, width = l_width);
            }
            for line in remark_lines {
                println!("{:width$}   {}", "", line, width = l_width);
            }
        }
        println!();
    }
}*/

/*pub struct CompilerError {
    message: String,
    blocks: Vec<DisplayBlock>,
    source: ErrorSource
}*/
/*impl CompilerError {
    pub fn new<E>(kind: CompilerErrorKind, error: E) -> Self where
        E: Into<Box<dyn Error + Send + Sync>> {
        let error = error.into();
        CompilerError {
            message: format!("{}", error),
            blocks: Vec::new(),
            source: ErrorSource::External(Box::new(CustomError {
                kind,
                error
            }))
        }
    }

    pub fn get_ref(&self) -> Option<&(dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::External(ref c) => Some(&*c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn get_mut(&mut self) -> Option<&mut (dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::External(ref mut c) => Some(&mut *c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn into_inner(self) -> Option<Box<dyn Error + Send + Sync>> {
        match self.source {
            ErrorSource::External(c) => Some(c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn kind(&self) -> Option<CompilerErrorKind> {
        match self.source {
            ErrorSource::External(ref c) => Some(c.kind),
            ErrorSource::Internal(kind) => Some(kind)
        }
    }

    pub fn print(&self, source: &str) where
    {
        match self.source {
            ErrorSource::Internal(CompilerErrorKind::Warning) => println!("{}: {}", style("warning").yellow().bright(), self.message),
            _ => println!("{}: {}", style("error").red().bright(), self.message)
        }
        for block in self.blocks.iter() {
            block.print(source)
        }
    }
}
impl From<CompilerErrorKind> for CompilerError {
    fn from(kind: CompilerErrorKind) -> Self {
        CompilerError {
            message: kind.as_str().to_owned(),
            blocks: Vec::new(),
            source: ErrorSource::Internal(kind)
        }
    }
}
impl Debug for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.source, f)
    }
}
impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.source {
            ErrorSource::External(ref c) => Display::fmt(&c.error, f),
            ErrorSource::Internal(kind) => write!(f, "{}", kind.as_str())
        }
    }
}
impl Error for CompilerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.source {
            ErrorSource::Internal(_) => None,
            ErrorSource::External(ref c) => c.error.source()
        }
    }
}*/




    /*pub fn unimplemented(reference: Reference) -> Self where
    {
        let message = String::from("this feature is currently not implemented");
        let source = ErrorSource::Internal(CompilerErrorKind::Unimplemented);
        let error_line = DisplayLine::from_reference(&reference, error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_generic<S>(reference: Reference, caption: S) -> Self where
        S: AsRef<str>
    {
        let message = String::from(caption.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::from_reference(&reference, error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_legal_symbol(reference: Reference) -> Self {
        let message = String::from("illegal symbol in token");
        let source = ErrorSource::Internal(CompilerErrorKind::IllegalSymbolInToken);
        let error_line = DisplayLine::new(reference.line(), reference.column(), 1, error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_legal_dollar_mark(reference: Reference) -> Self {
        let message = String::from("illegal sequence");
        let source = ErrorSource::Internal(CompilerErrorKind::IllegalSymbolInToken);
        let error_line = DisplayLine::new(reference.line(), reference.column(), reference.len(), error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line)
            .build_remark(&["help: did you mean `$` or `$$`?"]);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_valid_integer(reference: Reference, error: std::num::ParseIntError) -> Self {
        let message = format!("{}", error);
        let kind = CompilerErrorKind::ParseIntError;
        let source = ErrorSource::External(Box::new(CustomError { kind, error: Box::new(error) }));
        let error_line = DisplayLine::from_reference(&reference, error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }*/

    /*pub fn expect_end_of_line(filename: &Path, token: &Token) -> Self {
        let message = format!("unexpected `{}` at end of statement", token.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::from_reference(token, error_style());
        let error_block = DisplayBlock::new(filename, token.line(), token.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }*/

    /*pub fn expect_token_not_eof<S>(filename: &Path, token: &Token, expected: S) -> Self where
        S: AsRef<str>
    {
        let message = format!("expected {}, found end of file", expected.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::new(token.line(), token.column() + token.len(), 1, error_style());
        let error_block = DisplayBlock::new(filename, token.line(), token.column() + token.len())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }*/

    /*pub fn expect_token_not_eol<S>(reference: Reference, expected: S) -> Self where
        S: AsRef<str>
    {
        let message = format!("expected {}, found end of line", expected.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::from_reference(&reference, error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_token_not_token<E, F>(reference: Reference, expected: E, found: F) -> Self where
        E: AsRef<str>,
        F: AsRef<str>
    {
        let message = format!("expected {}, found `{}`", expected.as_ref(), found.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::from_reference(&reference, error_style());
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_expr_suitable_register<S>(reference: Reference, register_name: S) -> Self where
        S: AsRef<str>
    {
        let message = format!("register `{}` cannot be used in expressions", register_name.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::from_reference_with_caption(&reference, error_style(), String::from("cannot be used in expressions"));
        let error_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(error_line)
            .build_remark(&[format!("{}: allowed registers: `bx`, `bp`, `si`, `di`", help_style().apply_to("help"))]);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn compile_already_defined(first: &Label, second: &Label) -> Self {
        let message = format!("duplicate definitions with name `{}`", second.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SymbolRedefinition);
        let error_line_first = DisplayLine::from_reference_with_caption(first.reference(), help_style(), format!("previous definition of `{}`", first.as_ref()));
        let error_line_next = DisplayLine::from_reference_with_caption(second.reference(), error_style(), format!("duplicate definition"));
        let error_block = DisplayBlock::new(second.reference().filename(), second.reference().line(), second.reference().column())
            .build_line(error_line_first)
            .build_line(error_line_next);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }*/

    /*pub fn unexpected_seg(filename: &Path, token_seg: &Token, token_non_rel: &Token) -> Self {
        let message = format!("cannot apply `{}` to a non-relocatable value", token_seg.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::SyntaxError);
        let error_line = DisplayLine::new(
            token_seg.line(),
            token_seg.column(),
            token_non_rel.end() - token_seg.start(),
            error_style());
        let error_block = DisplayBlock::new(filename, token_seg.line(), token_seg.column())
            .build_line(error_line);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn expect_operand_not_eol<S>(filename: &Path, token_instruction: &Token, token_last_op: &Token, help: S) -> Self where
        S: AsRef<str>
    {
        let message = format!("insufficient number of operands supplied");
        let source = ErrorSource::Internal(CompilerErrorKind::InsufficientOperands);
        let error_line = DisplayLine::new(
            token_instruction.line(),
            token_instruction.column(),
            token_last_op.end() - token_instruction.start(),
            error_style()
        );
        let error_block = DisplayBlock::new(filename, token_instruction.line(), token_last_op.end())
            .build_line(error_line)
            .build_remark(&[format!("{}: {}", help_style().apply_to("help"), help.as_ref())]);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }

    pub fn unexpected_operand<S>(filename: &Path, token_instruction: &Token, token_first_ex_op: &Token, token_last_ex_op: &Token, help: S) -> Self where
        S: AsRef<str>
    {
        let message = format!("too many operands supplied");
        let source = ErrorSource::Internal(CompilerErrorKind::TooManyOperands);
        let error_line = DisplayLine::new(
            token_instruction.line(),
            token_first_ex_op.column(),
            token_last_ex_op.end() - token_first_ex_op.start(),
            error_style()
        );
        let error_block = DisplayBlock::new(filename, token_instruction.line(), token_first_ex_op.column())
            .build_line(error_line)
            .build_remark(&[format!("{}: {}", help_style().apply_to("help"), help.as_ref())]);
        CompilerError {
            message,
            blocks: vec![error_block],
            source
        }
    }*/

    /*pub fn warn_generic<S>(reference: Reference, caption: S) -> Self where
        S: AsRef<str>
    {
        let message = String::from(caption.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::Warning);
        let warning_line = DisplayLine::from_reference(&reference, warning_style());
        let warning_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(warning_line);
        CompilerError {
            message,
            blocks: vec![warning_block],
            source
        }
    }

    pub fn warn_not_at_start<S>(reference: Reference, statement_name: S) -> Self where
        S: AsRef<str>
    {
        let message = format!("`{}` statement not at start of section", statement_name.as_ref());
        let source = ErrorSource::Internal(CompilerErrorKind::Warning);
        let warning_line = DisplayLine::from_reference(&reference, warning_style());
        let warning_block = DisplayBlock::new(reference.filename(), reference.line(), reference.column())
            .build_line(warning_line);
        CompilerError {
            message,
            blocks: vec![warning_block],
            source
        }
    }*/


#[derive(Debug)]
pub struct CompilerLog {
    filename: Option<PathBuf>,
    text: Option<Rc<String>>,
    warnings: Vec<Notification>,
    errors: Vec<Notification>
}
impl CompilerLog {
    pub fn new() -> Self {
        CompilerLog {
            filename: None,
            text: None,
            warnings: Vec::new(),
            errors: Vec::new()
        }
    }

    pub fn warn(&mut self, warning: Notification) {
        self.warnings.push(warning);
    }

    pub fn err(&mut self, error: Notification) {
        self.errors.push(error);
    }

    pub fn set_filename<P>(&mut self, filename: P) where
        P: AsRef<Path> {
        self.filename = Some(filename.as_ref().to_path_buf());
    }

    pub fn set_text(&mut self, text: Rc<String>) {
        self.text = Some(text.clone());
    }

    pub fn update(&mut self) {
        for notification in self.warnings.iter_mut().chain(self.errors.iter_mut()) {
            notification.set_filename(self.filename.as_ref().unwrap())
        }
        for notification in self.warnings.iter_mut().chain(self.errors.iter_mut()) {
            notification.set_text(self.text.as_ref().unwrap().to_owned())
        }
    }

    pub fn is_err(&self) -> bool {
        self.errors.len() > 0
    }
}
impl Display for CompilerLog {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let iter = self.warnings.iter()
            .chain(self.errors.iter());
        for notification in iter {
            Display::fmt(notification, f)?;
        }
        if f.alternate() {
            let errors = self.errors.len();
            let warnings = self.warnings.len();
            match (errors, warnings) {
                (0, 0) => {},
                (0, 1) => writeln!(f, "{}: {}", warning_style().apply_to("warning"), white_style().apply_to("1 warning emitted"))?,
                (0, _) => writeln!(f, "{}: {} {}", warning_style().apply_to("warning"), white_style().apply_to(warnings), white_style().apply_to("warnings emitted"))?,
                (1, 0) => writeln!(f, "{}: {}", error_style().apply_to("error"), white_style().apply_to("aborting due to previous error"))?,
                (1, 1) => writeln!(f, "{}: {}", error_style().apply_to("error"), white_style().apply_to("aborting due to previous error; 1 warning emitted"))?,
                (1, _) => writeln!(f, "{}: {} {} {}", error_style().apply_to("error"), white_style().apply_to("aborting due to previous error;"), white_style().apply_to(warnings), white_style().apply_to("warnings emitted"))?,
                (_, 0) => writeln!(f, "{}: {} {} {}", error_style().apply_to("error"), white_style().apply_to("aborting due to previous"), white_style().apply_to(errors), white_style().apply_to("errors"))?,
                (_, 1) => writeln!(f, "{}: {} {} {}", error_style().apply_to("error"), white_style().apply_to("aborting due to previous"), white_style().apply_to(errors), white_style().apply_to("errors; 1 warning emitted"))?,
                (_, _) => writeln!(f, "{}: {} {} {} {} {}", error_style().apply_to("error"), white_style().apply_to("aborting due to previous"), white_style().apply_to(errors), white_style().apply_to("errors;warning emitted"), white_style().apply_to(warnings), white_style().apply_to("warnings emitted"))?,
            }
        }
        Ok(())
    }
}