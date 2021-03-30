#![macro_use]

pub mod operand;

use crate::lexer::prelude::*;
use crate::result::prelude::*;

use super::expression::Expression;
use super::label::Label;
use operand::Operand;

#[derive(Clone, Debug)]
pub struct Instruction {
    location: Location,
    label: Option<Label>,
    quantifier: Option<Expression>,
    prefix: Option<(Location, String)>,
    name: String,
    operands: Vec<Operand>
}
impl Instruction {
    pub fn expect(token_stream: &mut TokenStream) -> CompilerResult<Instruction> {
        let (prefix, location, name) = if let Some(capture) = token_stream.capture(syntax_rule![ { Instruction | PrefixInstruction | PseudoInstruction } EndOfLine ]) {
            let location = capture[0].locate();
            return Ok(Instruction { location, label: None, quantifier: None, prefix: None, name: token_stream[location].to_lowercase(), operands: Vec::new() });
        } else if let Some(capture) = token_stream.capture(syntax_rule![ { [ PrefixInstruction ] } { Instruction } ]) {
            if capture.count() > 1 {
                let prefix_location = capture[0].locate();
                let prefix_name = token_stream[prefix_location].to_lowercase();
                let location = capture[1].locate();
                let name = token_stream[location].to_lowercase();
                (Some((prefix_location, prefix_name)), location, name)
            } else {
                let location = capture[0].locate();
                let name = token_stream[location].to_lowercase();
                (None, location, name)
            }
        } else if let Some(capture) = token_stream.capture( syntax_rule![ { PseudoInstruction } ]) {
            let location = capture[0].locate();
            let name = token_stream[location].to_lowercase();
            (None, location, name)
        } else {
            let location = token_stream.next().unwrap().locate();
            return Err(Notification::error_parser_expected_found(location, "instruction", &token_stream[location]));
        };

        let mut operands = Vec::new();

        while let Some(operand) = Operand::capture(token_stream)? {
            let last = operand.is_last();
            operands.push(operand);
            if last { break; }
        }

        Ok(Instruction { location, label: None, quantifier: None, prefix, name, operands })
    }

    pub fn label(&self) -> Option<&Label> {
        self.label.as_ref()
    }

    pub fn quantifier(&self) -> Option<&Expression> {
        self.quantifier.as_ref()
    }

    pub fn prefix(&self) -> Option<&(Location, String)> {
        self.prefix.as_ref()
    }

    pub fn set_label(&mut self, label: Label) {
        self.label = Some(label);
    }

    pub fn set_quantifier(&mut self, quantifier: Expression) {
        self.quantifier = Some(quantifier);
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn into_operands(self) -> Vec<Operand> {
        self.operands
    }
}
impl Locate for Instruction {
    fn locate(&self) -> Location {
        self.location
    }
}
