#![macro_use]

use std::cmp::Ordering;

use crate::Reg16;
use crate::result::prelude::*;
use crate::lexer::prelude::*;
use crate::parser::statement::literal::IntegerLiteral;
use crate::parser::statement::label::Label;
/*use crate::parser:::label::Label;
use super::literal::IntegerLiteral;*/

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Operation {
    /// Defines the operation `+`.
    Add(Location),
    /// Defines the operation `-`.
    Sub(Location),
    /// Defines the operation `*`.
    Mul(Location),
    /// Defines the operation `/`.
    Div(Location),
}
impl Operation {
    pub fn capture(token_stream: &mut TokenStream) -> Option<Operation> {
        if let Some(capture) = token_stream.capture(syntax_rule![ { Operator } ]) {
            let location = capture.locate();
            match &token_stream[location] {
                "+" => Some(Operation::Add(location)),
                "-" => Some(Operation::Sub(location)),
                "*" => Some(Operation::Mul(location)),
                "/" => Some(Operation::Div(location)),
                _ => panic!("undefined operation: `{}`", &token_stream[location])
            }
        } else {
            None
        }
    }
}
impl PartialOrd for Operation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Operation {
    fn cmp(&self, other: &Self) -> Ordering {
        let priority = |op: Operation| {
            match op {
                Operation::Mul(_) | Operation::Div(_) => 2,
                Operation::Add(_) | Operation::Sub(_) => 1
            }
        };
        let self_priority = priority(*self);
        let other_priority = priority(*other);
        self_priority.cmp(&other_priority)
    }
}
impl Locate for Operation {
    fn locate(&self) -> Location {
        match *self {
            Operation::Add(location) => location,
            Operation::Sub(location) => location,
            Operation::Mul(location) => location,
            Operation::Div(location) => location
        }
    }
}



#[derive(Clone, Debug)]
enum Operand {
    Number(IntegerLiteral),
    Variable(Label),
    Reg16(Location, Reg16),
    InstructionStart(Location),
    SectionStart(Location),
}



#[derive(Clone, Debug)]
enum ShuntingYardItem {
    Operation(Operation),
    Operand(Operand),
    LeftParen(Location)
}



pub fn expect(token_stream: &mut TokenStream) -> CompilerResult<super::Expression> {
    let (location, last, mut output) = shunting_yard(token_stream)?;
    let tree = parse_rpn(&mut output);
    let coeff_bx = tree.coefficient_of_register(Reg16::BX)?;
    let coeff_bp = tree.coefficient_of_register(Reg16::BP)?;
    let coeff_si = tree.coefficient_of_register(Reg16::SI)?;
    let coeff_di = tree.coefficient_of_register(Reg16::DI)?;
    let ea_calculation = match (coeff_bx, coeff_bp, coeff_si, coeff_di) {
        (0, 0, 0, 0) => None,
        (1, 0, 1, 0) => Some(super::EffectiveAddressCalculation::BxSi),
        (1, 0, 0, 1) => Some(super::EffectiveAddressCalculation::BxDi),
        (0, 1, 1, 0) => Some(super::EffectiveAddressCalculation::BpSi),
        (0, 1, 0, 1) => Some(super::EffectiveAddressCalculation::BpDi),
        (0, 0, 1, 0) => Some(super::EffectiveAddressCalculation::Si),
        (0, 0, 0, 1) => Some(super::EffectiveAddressCalculation::Di),
        (0, 1, 0, 0) => Some(super::EffectiveAddressCalculation::Bp),
        (1, 0, 0, 0) => Some(super::EffectiveAddressCalculation::Bx),
        _ => return Err(Notification::error_parser_invalid_combination_of_registers(location))
    };
    let expression_tree = tree.build()?;
    Ok(super::Expression { location, ea_calculation, expression_tree, last })
}



/// Runs the "Shunting Yard" algorithm and outputs an expression in Reverse Polish Notation.
fn shunting_yard(token_stream: &mut TokenStream) -> CompilerResult<(Location, TokenClass, Vec<ShuntingYardItem>)> {
    let mut output = Vec::new();
    let mut operator_stack = Vec::new();

    let mut nested_parens = 0;
    let mut expect_operand = true;

    let mut last = TokenClass::EndOfLine;
    let mut location: Option<Location> = None;

    let update_location = |l: &mut Option<Location>, o: &dyn Locate| {
        if let Some(ref mut l) = *l {
            *l |= o.locate();
        } else {
            *l = Some(o.locate());
        }
    };

    while !token_stream.is_empty() {
        if expect_operand {
            update_location(&mut location, &token_stream[0]);
            if let Some(number) = IntegerLiteral::capture(token_stream)? {
                output.push(ShuntingYardItem::Operand(Operand::Number(number)));
                expect_operand = false;
            } else if let Some(variable) = Label::capture(token_stream) {
                output.push(ShuntingYardItem::Operand(Operand::Variable(variable)));
                expect_operand = false;
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { Reg16 } ]) {
                let location = capture.locate();
                let reg = Reg16::parse(&token_stream[location]);
                match reg {
                    Reg16::AX | Reg16::CX | Reg16::DX | Reg16::SP => return Err(Notification::error_parser_wrong_register_in_expression(&capture)),
                    _ => {}
                }
                output.push(ShuntingYardItem::Operand(Operand::Reg16(location, reg)));
                expect_operand = false;
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { Reg8 | RegSegment } ]) {
                return Err(Notification::error_parser_wrong_register_in_expression(&capture));
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { InstructionStart } ]) {
                output.push(ShuntingYardItem::Operand(Operand::InstructionStart(capture.locate())));
                expect_operand = false;
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { SectionStart } ]) {
                output.push(ShuntingYardItem::Operand(Operand::SectionStart(capture.locate())));
                expect_operand = false;
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { LeftParen } ]) {
                operator_stack.push(ShuntingYardItem::LeftParen(capture.locate()));
                nested_parens += 1;
            } else {
                let location = token_stream.next().unwrap().locate();
                return Err(Notification::error_parser_expected_found(location, "expression", &token_stream[location]));
            }
        } else {
            if let Some(operation) = Operation::capture(token_stream) {
                update_location(&mut location, &operation);
                while let Some(op) = operator_stack.last() {
                    match *op {
                        ShuntingYardItem::Operation(op) => {
                            if op >= operation {
                                output.push(operator_stack.pop().unwrap())
                            } else { break }
                        },
                        _ => break
                    }
                }
                operator_stack.push(ShuntingYardItem::Operation(operation));
                expect_operand = true;
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { RightParen } ]) {
                update_location(&mut location, &capture);
                let location = capture.locate();
                if nested_parens == 0 {
                    return Err(Notification::error_parser_mismatched_right_paren(location));
                }
                while let Some(ShuntingYardItem::Operation(op)) = operator_stack.pop() {
                    output.push(ShuntingYardItem::Operation(op));
                }
                nested_parens -= 1;
            } else if let Some(capture) = token_stream.capture(syntax_rule![ { EndOfLine | Comma | RightSquareParen } ]) {
                last = capture[0].class();
                break;
            } else if token_stream[0].class() == TokenClass::Instruction
                || token_stream[0].class() == TokenClass::PrefixInstruction
                || token_stream[0].class() == TokenClass::PseudoInstruction {
                last = token_stream[0].class();
                break;
            } else {
                let location = token_stream.next().unwrap().locate();
                let expected = if nested_parens > 0 {
                    "`+`, `-`, `*`, `/`, `(`, `)` or end of expression"
                } else {
                    "`+`, `-`, `*`, `/`, `(` or end of expression"
                };
                return Err(Notification::error_parser_expected_found(location, expected, &token_stream[location]));
            }
        }
    }

    while let Some(top) = operator_stack.pop() {
        match top {
            ShuntingYardItem::Operation(op) => output.push(ShuntingYardItem::Operation(op)),
            ShuntingYardItem::LeftParen(location) => return Err(Notification::error_parser_mismatched_left_paren(location)),
            _ => unreachable!()
        }
    }

    /*let mut expression = Self::from_rpn(&mut output);
    expression.last = last;
    Ok(expression)*/

    Ok((location.unwrap(), last, output))
}



/// Converts the output of the "Shunting Yard" algorithm into an expression tree.
fn parse_rpn(shunting_yard_output: &mut Vec<ShuntingYardItem>) -> ExpressionTree {
    let (location, kind) = match shunting_yard_output.pop().unwrap() {
        ShuntingYardItem::Operand(Operand::Variable(var)) => (var.locate(), ExpressionKind::Variable(var)),
        ShuntingYardItem::Operand(Operand::Number(num)) => (num.locate(), ExpressionKind::Number(num)),
        ShuntingYardItem::Operand(Operand::InstructionStart(location)) => (location, ExpressionKind::InstructionStart(location)),
        ShuntingYardItem::Operand(Operand::SectionStart(location)) => (location, ExpressionKind::SectionStart(location)),
        ShuntingYardItem::Operand(Operand::Reg16(location, reg)) => (location, ExpressionKind::Reg16(location, reg)),
        ShuntingYardItem::Operation(op) => {
            match op {
                Operation::Add(_) | Operation::Sub(_) | Operation::Mul(_) | Operation::Div(_) => {
                    let op2 = Box::new(parse_rpn(shunting_yard_output));
                    let op1 = Box::new(parse_rpn(shunting_yard_output));
                    (op1.locate() | op2.locate(), ExpressionKind::BinaryOperation(op, op1, op2))
                }
            }
        },
        _ => unreachable!()
    };
    ExpressionTree { last: TokenClass::EndOfLine, location, kind }
}



#[derive(Clone, Debug, Eq, PartialEq)]
enum ExpressionKind {
    Number(IntegerLiteral),
    Variable(Label),
    Reg16(Location, Reg16),
    InstructionStart(Location),
    SectionStart(Location),
    BinaryOperation(Operation, Box<ExpressionTree>, Box<ExpressionTree>)
}



#[derive(Clone, Debug, Eq, PartialEq)]
struct ExpressionTree {
    location: Location,
    kind: ExpressionKind,
    last: TokenClass
}
impl ExpressionTree {
    pub fn coefficient_of_register(&self, reg: Reg16) -> CompilerResult<i32> {
        match &self.kind {
            ExpressionKind::Number(_)
            | ExpressionKind::Variable(_)
            | ExpressionKind::InstructionStart(_)
            | ExpressionKind::SectionStart(_)
            => Ok(0),
            ExpressionKind::Reg16(_, found) => Ok(if *found == reg { 1 } else { 0 }),
            ExpressionKind::BinaryOperation(op, expr1, expr2) => {
                match op {
                    Operation::Add(_) => Ok(expr1.coefficient_of_register(reg)? + expr2.coefficient_of_register(reg)?),
                    Operation::Sub(_) => Ok(expr1.coefficient_of_register(reg)? - expr2.coefficient_of_register(reg)?),
                    Operation::Mul(location) => Err(Notification::error_parser_non_scalar_product(location)),
                    Operation::Div(location) => Err(Notification::error_parser_non_scalar_division(location)),
                }
            }
        }
    }

    pub fn build(self) -> CompilerResult<Option<super::ExpressionTree>> {
        let tree = match self.kind {
            ExpressionKind::Number(num) => if num.value() == 0 { None } else { Some(super::ExpressionTree::Number(num)) },
            ExpressionKind::Variable(var) => Some(super::ExpressionTree::Variable(var)),
            ExpressionKind::Reg16(_, _) => None,
            ExpressionKind::InstructionStart(_) => Some(super::ExpressionTree::InstructionStart),
            ExpressionKind::SectionStart(_) => Some(super::ExpressionTree::SectionStart),
            ExpressionKind::BinaryOperation(op, expr1, expr2) => {
                let expr1 = expr1.build()?;
                let expr2 = expr2.build()?;
                match (op, expr1, expr2) {
                    (Operation::Add(_), Some(expr1), Some(expr2)) => Some(super::ExpressionTree::OperationAdd(Box::new(expr1), Box::new(expr2))),
                    (Operation::Add(_), Some(expr), None)
                    | (Operation::Add(_), None, Some(expr))
                    => Some(expr),
                    (Operation::Sub(_), Some(expr1), Some(expr2)) => Some(super::ExpressionTree::OperationSub(Box::new(expr1), Box::new(expr2))),
                    (Operation::Sub(_), Some(expr), None) => Some(expr),
                    (Operation::Sub(_), None, Some(expr)) => Some(super::ExpressionTree::OperationNeg(Box::new(expr))),
                    (Operation::Mul(_), Some(expr1), Some(expr2)) => Some(super::ExpressionTree::OperationMul(Box::new(expr1), Box::new(expr2))),
                    (Operation::Div(_), Some(expr1), Some(expr2)) => Some(super::ExpressionTree::OperationDiv(Box::new(expr1), Box::new(expr2))),
                    (Operation::Div(location), _, None) => return Err(Notification::error_division_by_zero(location)),
                    _ => None
                }
            }
        };
        Ok(tree)
    }
}
impl Locate for ExpressionTree {
    fn locate(&self) -> Location {
        self.location
    }
}