use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::parser::prelude::*;
use crate::result::prelude::*;
use super::opcode::OpCode;
use super::section::Section;
use super::vars::{LinkerDeclaration, LinkerLabel};

#[derive(Debug)]
pub struct BinaryModule {
    pub(crate) filename: PathBuf,
    pub(crate) contents: Rc<String>,
    pub(crate) log: RefCell<CompilerLog>,
    pub(crate) sections: Vec<Section>,
    pub(crate) global_vars: Vec<LinkerDeclaration>,
    pub(crate) extern_vars: Vec<LinkerDeclaration>,
    pub(self) origin: i32,
    pub(self) offset: i32,
    pub(self) last_label: Option<String>,
    pub(self) current_section: usize
}
impl BinaryModule {
    pub fn new(filename: PathBuf, contents: Rc<String>, log: CompilerLog) -> Self {
        BinaryModule {
            filename,
            contents,
            log: RefCell::new(log),
            sections: vec![Section::new_unnamed()],
            current_section: 0,
            origin: 0,
            offset: 0,
            last_label: None,
            global_vars: Vec::new(),
            extern_vars: Vec::new()
        }
    }

    pub fn filename(&self) -> &Path {
        &self.filename
    }

    pub fn contents(&self) -> Rc<String> {
        self.contents.clone()
    }

    pub fn warn(&self, warning: Notification) {
        self.log.borrow_mut().warn(warning);
    }

    pub fn err(&self, error: Notification) {
        self.log.borrow_mut().err(error);
    }

    pub fn as_str(&self) -> &str {
        self.contents.as_str()
    }

    pub fn declare_global<S>(&mut self, var: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        if let Some(dup) = self.global_vars.iter().find(|s| s.as_ref() == var.as_ref()) {
            return Err(Notification::error_duplicate_definition(var.as_ref(), dup, &var));
        }
        self.global_vars.push(LinkerDeclaration::new(var));
        Ok(())
    }

    pub fn declare_extern<S>(&mut self, var: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        if let Some(dup) = self.locate_name(&var) {
            return Err(Notification::error_duplicate_definition(&var, dup, &var))
        }
        self.extern_vars.push(LinkerDeclaration::new(var));
        Ok(())
    }

    pub fn declare_section<S>(&mut self, name: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        let result = if let Some(dup) = self.sections.iter().find(|s| if let Some(ref dup) = s.name() { dup == name.as_ref() } else { false }) {
            Err(Notification::error_duplicate_definition(name.as_ref(), dup.locate(), &name))
        } else {
            Ok(())
        };
        self.sections.push(Section::new(name));
        self.origin = 0;
        self.offset = 0;
        self.last_label = None;
        self.current_section += 1;
        result
    }

    pub fn declare_label<S>(&mut self, name: S) -> CompilerResult<()> where
        S: AsRef<str> + Locate {
        let location = name.locate();
        let name = name.as_ref();
        let name = if name.starts_with('.') {
            if let Some(label) = self.last_label.as_ref() {
                label.to_owned() + name
            } else {
                name.to_owned()
            }
        } else {
            name.to_owned()
        };
        if let Some(dup) = self.locate_name(&name) {
            return Err(Notification::error_duplicate_definition(&name, dup, location))
        }
        let label = LinkerLabel::new(location, name, self.offset as i32);
        self.current_section_mut().define(label);
        Ok(())
    }

    pub fn locate_name<S>(&self, name: S) -> Option<Location> where
        S: AsRef<str> {
        for sec in self.sections.iter() {
            for dup in sec.vars().iter() {
                if dup.name() == name.as_ref() {
                    return Some(dup.locate());
                }
            }
        }
        for dup in self.extern_vars.iter() {
            if dup.name() == name.as_ref() {
                return Some(dup.locate());
            }
        }
        None
    }

    pub fn set_origin(&mut self, origin: i32) {
        self.origin = origin;
    }

    pub fn advance(&mut self, amount: i32) {
        self.offset += amount;
    }

    pub fn sections(&self) -> &[Section] {
        &self.sections[..]
    }

    pub fn current_section(&self) -> &Section {
        &self.sections[self.current_section]
    }

    pub fn current_section_mut(&mut self) -> &mut Section {
        &mut self.sections[self.current_section]
    }

    pub fn offset(&self) -> i32 {
        self.offset
    }

    pub fn position(&self) -> i32 {
        self.origin + self.offset
    }

    pub fn process_statement(&mut self, statement: Statement) -> CompilerResult<()> {
        match statement.kind() {
            StatementKind::Global(label) => self.declare_global(label)?,
            StatementKind::Extern(label) => self.declare_extern(label)?,
            StatementKind::Section(label) => self.declare_section(label)?,
            StatementKind::Origin(num) => self.set_origin(num.value()),
            StatementKind::Label(label) => self.declare_label(label)?,
            StatementKind::Instruction(instr) => {
                if let Some(label) = instr.label() {
                    if let Err(error) = self.declare_label(label) {
                        self.err(error);
                    }
                }
                let opcode = OpCode::from_instr(self, instr.to_owned())?;
                self.current_section_mut().push_opcode(opcode);
            }
        }
        Ok(())
    }

    pub fn from_listing(listing: Listing) -> Result<Self, CompilerLog> {
        let Listing { filename, contents, log, statements, .. } = listing;
        let mut binary = Self::new(filename, contents, log);
        for statement in statements {
            if let Err(error) = binary.process_statement(statement) {
                binary.err(error);
            }
        }
        for section in &binary.sections[..] {
            for opcode in section.opcodes() {
                opcode.check_references(&binary);
                opcode.solve_references(&binary, section.name().as_ref());
            }
        }
        /*let mut bytes = Vec::new();
        for section in &binary.sections[..] {
            for opcode in section.opcodes() {
                for byte in &opcode.payload()[..] {
                    bytes.push(*byte);
                }
            }
        }
        for i in 0..bytes.len() {
            if i % 16 == 0 {
                print!("\n{:04x}:", i);
            }
            print!(" {:02x}", bytes[i]);
        }
        println!();*/
        if binary.log.borrow().is_err() {
            Err(binary.log.into_inner())
        } else {
            Ok(binary)
        }
    }
}