use crate::result::prelude::*;
use super::opcode::OpCode;
use super::vars::LinkerLabel;

#[derive(Clone, Debug)]
pub struct Section {
    pub(self) location: Location,
    pub(self) name: Option<String>,
    pub(self) local_vars: Vec<LinkerLabel>,
    pub(self) opcodes: Vec<OpCode>
}
impl Section {
    pub fn new_unnamed() -> Self {
        Section { location: Location::new(0, 0, 0, 0), name: None, local_vars: Vec::new(), opcodes: Vec::new() }
    }

    pub fn new<S>(name: S) -> Self where
        S: AsRef<str> + Locate {
        Section { location: name.locate(), name: Some(name.as_ref().to_owned()), local_vars: Vec::new(), opcodes: Vec::new() }
    }

    pub fn name(&self) -> Option<String> {
        self.name.clone()
    }

    pub fn define(&mut self, label: LinkerLabel) {
        self.local_vars.push(label);
    }

    pub fn vars(&self) -> &[LinkerLabel] {
        &self.local_vars[..]
    }

    pub fn push_opcode(&mut self, opcode: OpCode) {
        self.opcodes.push(opcode);
    }

    pub fn opcodes(&self) -> &[OpCode] {
        &self.opcodes[..]
    }

    pub fn opcodes_mut(&mut self) -> &mut [OpCode] {
        &mut self.opcodes[..]
    }
}
impl Locate for Section {
    fn locate(&self) -> Location {
        self.location
    }
}