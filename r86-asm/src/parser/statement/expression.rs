#![macro_use]

mod algorithms;

use std::collections::HashMap;

use crate::result::prelude::*;
use crate::lexer::prelude::*;
use crate::parser::statement::label::Label;
use crate::parser::statement::literal::IntegerLiteral;
use crate::compiler::binary::UnsolvedBinary;

#[derive(Debug)]
struct ExpressionEval {
    pub(self) scalar: i32,
    pub(self) sections: HashMap<Option<String>, i32>
}
impl ExpressionEval {
    pub fn from_scalar(scalar: i32) -> ExpressionEval {
        ExpressionEval { scalar, sections: HashMap::new() }
    }

    pub fn from_variable(binary: &UnsolvedBinary, var: &Label) -> Option<ExpressionEval> {
        let mut sections = HashMap::new();
        for section in binary.sections() {
            for label in section.vars() {
                if label.name() == var.as_str() {
                    sections.insert(section.name(), 1);
                    return Some(ExpressionEval { scalar: label.offset(), sections });
                }
            }
        }
        None
    }

    pub fn from_instruction_start(binary: &UnsolvedBinary) -> ExpressionEval {
        let section = binary.current_section().name();
        let scalar = binary.offset() as i32;
        let mut sections = HashMap::new();
        sections.insert(section, 1);
        ExpressionEval { scalar, sections }
    }

    pub fn from_section_start(binary: &UnsolvedBinary) -> ExpressionEval {
        let section = binary.current_section().name();
        let mut sections = HashMap::new();
        sections.insert(section, 1);
        ExpressionEval { scalar: 0, sections }
    }
}
impl std::ops::Add for ExpressionEval {
    type Output = ExpressionEval;

    fn add(self, rhs: Self) -> Self::Output {
        let scalar = self.scalar + rhs.scalar;
        let mut sections = self.sections;
        for (name, coefficient) in rhs.sections {
            if let Some(c) = sections.get_mut(&name) {
                if *c + coefficient == 0 {
                    sections.remove(&name);
                } else {
                    *c += coefficient;
                }
            } else {
                sections.insert(name, coefficient);
            }
        }
        ExpressionEval { scalar, sections }
    }
}
impl std::ops::Sub for ExpressionEval {
    type Output = ExpressionEval;

    fn sub(self, rhs: Self) -> Self::Output {
        let scalar = self.scalar - rhs.scalar;
        let mut sections = self.sections;
        for (name, coefficient) in rhs.sections {
            if let Some(c) = sections.get_mut(&name) {
                if *c - coefficient == 0 {
                    sections.remove(&name);
                } else {
                    *c -= coefficient;
                }
            } else {
                sections.insert(name, coefficient);
            }
        }
        ExpressionEval { scalar, sections }
    }
}
impl std::ops::Neg for ExpressionEval {
    type Output = ExpressionEval;

    fn neg(self) -> Self::Output {
        let scalar = -self.scalar;
        let mut sections = self.sections;
        for (_, coefficient) in sections.iter_mut() {
            *coefficient = -*coefficient;
        }
        ExpressionEval { scalar, sections }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EffectiveAddressCalculation {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    Bp,
    Bx
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExpressionTree {
    Number(IntegerLiteral),
    Variable(Label),
    InstructionStart,
    SectionStart,
    OperationNeg(Box<ExpressionTree>),
    OperationAdd(Box<ExpressionTree>, Box<ExpressionTree>),
    OperationSub(Box<ExpressionTree>, Box<ExpressionTree>),
    OperationMul(Box<ExpressionTree>, Box<ExpressionTree>),
    OperationDiv(Box<ExpressionTree>, Box<ExpressionTree>)
}
impl ExpressionTree {
    pub(self) fn try_eval(&self, binary: &UnsolvedBinary) -> CompilerResult<Option<ExpressionEval>> {
        match self {
            ExpressionTree::Number(num) => Ok(Some(ExpressionEval::from_scalar(num.value()))),
            ExpressionTree::Variable(var) => Ok(ExpressionEval::from_variable(binary, var)),
            ExpressionTree::InstructionStart => Ok(Some(ExpressionEval::from_instruction_start(binary))),
            ExpressionTree::SectionStart => Ok(Some(ExpressionEval::from_section_start(binary))),
            ExpressionTree::OperationNeg(expr) => {
                if let Some(eval) = expr.try_eval(binary)? {
                    Ok(Some(-eval))
                } else {
                    Ok(None)
                }
            },
            ExpressionTree::OperationAdd(expr1, expr2) => {
                if let (Some(eval1), Some(eval2)) = (expr1.try_eval(binary)?, expr2.try_eval(binary)?) {
                    Ok(Some(eval1 + eval2))
                } else {
                    Ok(None)
                }
            },
            ExpressionTree::OperationSub(expr1, expr2) => {
                if let (Some(eval1), Some(eval2)) = (expr1.try_eval(binary)?, expr2.try_eval(binary)?) {
                    Ok(Some(eval1 - eval2))
                } else {
                    Ok(None)
                }
            },
            _ => unimplemented!()
        }
    }

    pub fn check_references(&self, binary: &UnsolvedBinary) {
        match &self {
            ExpressionTree::Variable(label) => if binary.locate_name(label.as_str()).is_none() {
                binary.err(Notification::error_undefined_symbol(&label, &label));
            },
            ExpressionTree::OperationNeg(expr) => expr.as_ref().check_references(binary),
            ExpressionTree::OperationAdd(expr1, expr2) => {
                expr1.check_references(binary);
                expr2.check_references(binary);
            },
            ExpressionTree::OperationSub(expr1, expr2) => {
                expr1.check_references(binary);
                expr2.check_references(binary);
            },
            ExpressionTree::OperationMul(expr1, expr2) => {
                expr1.check_references(binary);
                expr2.check_references(binary);
            },
            ExpressionTree::OperationDiv(expr1, expr2) => {
                expr1.check_references(binary);
                expr2.check_references(binary);
            },
            _ => {}
        }
    }
}

/// Defines an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expression {
    location: Location,
    ea_calculation: Option<EffectiveAddressCalculation>,
    expression_tree: Option<ExpressionTree>,
    last: TokenClass
}
impl Expression {
    pub fn from_label(location: Location, label: Label, last: TokenClass) -> Expression {
        Expression {
            location,
            ea_calculation: None,
            expression_tree: Some(ExpressionTree::Variable(label)),
            last
        }
    }

    pub fn from_number(location: Location, num: IntegerLiteral, last: TokenClass) -> Expression {
        Expression {
            location,
            ea_calculation: None,
            expression_tree: Some(ExpressionTree::Number(num)),
            last
        }
    }

    pub fn from_instruction_start(location: Location, last: TokenClass) -> Expression {
        Expression {
            location,
            ea_calculation: None,
            expression_tree: Some(ExpressionTree::InstructionStart),
            last
        }
    }

    pub fn from_section_start(location: Location, last: TokenClass) -> Expression {
        Expression {
            location,
            ea_calculation: None,
            expression_tree: Some(ExpressionTree::SectionStart),
            last
        }
    }

    pub fn expect(token_stream: &mut TokenStream) -> CompilerResult<Expression> {
        algorithms::expect(token_stream)
    }

    pub fn last_token_class(&self) -> TokenClass {
        self.last
    }

    pub fn effective_address(&self) -> Option<EffectiveAddressCalculation> {
        self.ea_calculation
    }

    pub fn eval(&self, _binary: &UnsolvedBinary) -> CompilerResult<i32> {
        unimplemented!()
    }

    pub fn try_eval(&self, binary: &UnsolvedBinary) -> CompilerResult<Option<i32>> {
        if let Some(ref expr_tree) = self.expression_tree {
            if let Some(eval) = expr_tree.try_eval(binary)? {
                if eval.sections.is_empty() {
                    Ok(Some(eval.scalar))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(Some(0))
        }
    }

    pub fn try_eval_offset(&self, binary: &UnsolvedBinary) -> CompilerResult<(Option<Option<String>>, Option<i32>)> {
        if let Some(ref expr_tree) = self.expression_tree {
            if let Some(eval) = expr_tree.try_eval(binary)? {
                if eval.sections.is_empty() {
                    Ok((None, Some(eval.scalar)))
                } else if eval.sections.len() == 1 {
                    let name = eval.sections.keys().last().unwrap();
                    Ok((Some(name.to_owned()), Some(eval.scalar)))
                } else {
                    return Err(Notification::error_unimplemented(self))
                }
            } else {
                Ok((None, None))
            }
        } else {
            Ok((None, Some(0)))
        }
    }

    pub fn check_references(&self, binary: &UnsolvedBinary) {
        if let Some(expression_tree) = &self.expression_tree {
            expression_tree.check_references(binary)
        }
    }
}
impl Locate for Expression {
    fn locate(&self) -> Location {
        self.location
    }
}