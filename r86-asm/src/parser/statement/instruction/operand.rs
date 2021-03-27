#![macro_use]

use crate::{Reg8, Reg16, RegSegment};
use crate::result::prelude::*;
use crate::lexer::prelude::*;
use crate::parser::statement::label::Label;
use crate::parser::statement::literal::IntegerLiteral;
use crate::parser::statement::expression::{Expression, ExpressionTree};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SizeConstraint {
    Byte,
    Word
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OperandKind {
    /// Identifies a register by register number.
    Reg(u8),
    /// Identifies a register by register number.
    SegmentReg(u8),
    /// Identifies the segment relative to a label.
    SegmentOf(Label),
    /// Identifies an expression.
    Expression(Expression),
    /// Identifies a memory address.
    Address(Option<u8>, Expression)
}
#[derive(Clone, Debug, Eq)]
pub struct Operand {
    location: Location,
    size: Option<SizeConstraint>,
    kind: OperandKind,
    last: bool
}
impl Operand {
    pub fn capture(token_stream: &mut TokenStream) -> CompilerResult<Option<Operand>> {
        if token_stream.capture(syntax_rule![EndOfLine]).is_some() {
            return Ok(None)
        } else if let Some(capture) = token_stream.capture(syntax_rule![ { Reg8 | Reg16 | RegSegment | Ident | IntegerLiteral | InstructionStart | SectionStart } { EndOfLine | Comma } ]) {
            let last = capture.last().class() == TokenClass::EndOfLine;
            let location = capture[0].locate();
            let (size, kind) = match capture[0].class() {
                TokenClass::Reg8 => (Some(SizeConstraint::Byte), OperandKind::Reg(Reg8::parse(&token_stream[location]) as u8)),
                TokenClass::Reg16 => (Some(SizeConstraint::Word), OperandKind::Reg(Reg16::parse(&token_stream[location]) as u8)),
                TokenClass::RegSegment => (Some(SizeConstraint::Word), OperandKind::SegmentReg(RegSegment::parse(&token_stream[location]) as u8)),
                TokenClass::Ident => (None, OperandKind::Expression(Expression::from_label(location, Label::expect_at(token_stream, location)?, capture.last().class()))),
                TokenClass::IntegerLiteral => (None, OperandKind::Expression(Expression::from_number(location, IntegerLiteral::expect_at(token_stream, location)?, capture.last().class()))),
                TokenClass::InstructionStart => (None, OperandKind::Expression(Expression::from_instruction_start(location, capture.last().class()))),
                TokenClass::SectionStart => (None, OperandKind::Expression(Expression::from_section_start(location, capture.last().class()))),
                _ => unreachable!()
            };
            Ok(Some(Operand { location, size, kind, last }))
        } else if let Some(capture) = token_stream.capture(syntax_rule![ SEG { Ident } { EndOfLine | Comma } ]) {
            let _last = capture.last().class() == TokenClass::EndOfLine;
            let location = capture.locate();
            Err(Notification::error_unimplemented(location))
        } else if let Some(capture) = token_stream.capture(syntax_rule![ Operator IntegerLiteral { EndOfLine | Comma } ]) {
            let _last = capture.last().class() == TokenClass::EndOfLine;
            let location = capture.locate();
            return Err(Notification::error_unimplemented(location));
        } else if let Some(capture) = token_stream.capture(syntax_rule![ (Byte | Word) IntegerLiteral { EndOfLine | Comma } ]) {
            let _last = capture.last().class() == TokenClass::EndOfLine;
            let location = capture.locate();
            return Err(Notification::error_unimplemented(location));
        } else if let Some(capture) = token_stream.capture(syntax_rule![ (Byte | Word) Operator IntegerLiteral { EndOfLine | Comma } ]) {
            let _last = capture.last().class() == TokenClass::EndOfLine;
            let location = capture.locate();
            return Err(Notification::error_unimplemented(location))
        } else if let Some(capture) = token_stream.capture(syntax_rule![LeftSquareParen]) {
            let location = capture.locate();
            return Err(Notification::error_unimplemented(location))
        } else {
            let expression = Expression::expect(token_stream)?;
            let size = None;
            let last = expression.last_token_class() == TokenClass::EndOfLine;
            Ok(Some(Operand { location: expression.locate(), size, kind: OperandKind::Expression(expression), last }))
        }
    }

    pub fn kind(&self) -> &OperandKind {
        &self.kind
    }

    pub fn is_last(&self) -> bool {
        self.last
    }

    pub fn size(&self) -> Option<SizeConstraint> {
        self.size
    }
}
impl Locate for Operand {
    fn locate(&self) -> Location {
        self.location
    }
}
impl PartialEq for Operand {
    fn eq(&self, other: &Self) -> bool {
        if let (Some(self_size), Some(other_size)) = (self.size, other.size) {
            if self_size != other_size {
                 return false;
            }
        }
        use OperandKind::*;
        match (&self.kind, &other.kind) {
            (Reg(self_reg), Reg(other_reg)) => *self_reg == *other_reg,
            (SegmentReg(self_reg), SegmentReg(other_reg)) => *self_reg == *other_reg,
            (SegmentOf(self_label), SegmentOf(other_label)) => *self_label == *other_label,
            (Expression(self_expr), Expression(other_expr)) => *self_expr == *other_expr,
            (
                Address(self_override, self_expr),
                Address(other_override, other_expr)
            ) => *self_override == *other_override && *self_expr == *other_expr,
            _ => false
        }
    }
}