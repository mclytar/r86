use std::path::PathBuf;
use std::rc::Rc;

use crate::context::Context;
use crate::result::error::{CompilerError, CompilerNotificationList};
use crate::statement::Statement;

pub struct Assembly {

}
impl Assembly {
    pub fn new(mut context: Context, mut warnings: Vec<CompilerError>, statements: Vec<Statement>) -> Result<Assembly, CompilerNotificationList> {
        let mut errors = Vec::new();
        for statement in statements {
            match statement {
                Statement::Label(label) => if let Err(err) = context.define_label(label) {
                    errors.push(err);
                },
                Statement::Global(label) => {
                    if context.offset() > 0 {
                        warnings.push(CompilerError::warn_not_at_start(label.reference().to_owned(), "GLOBAL"))
                    }
                    if let Err(err) = context.declare_global(label) {
                        errors.push(err);
                    }
                },
                Statement::Origin(number) => {
                    if context.offset() > 0 {
                        warnings.push(CompilerError::warn_not_at_start(number.reference().to_owned(), "ORG"))
                    }
                    context.set_origin(number.value() as usize);
                },
                Statement::Instruction(instruction) => {
                    let (label, quantifier) = match instruction.compile(&mut context) {
                        Ok(ci) => ci,
                        Err(err) => {
                            errors.push(err);
                            continue;
                        }
                    };
                    if let Some(label) = label {
                        if let Err(err) = context.define_label(label) {
                            errors.push(err);
                        }
                    }
                    context.advance(1);
                }
                _ => {}
            }
        }

        if errors.len() > 0 {
            let mut collection = CompilerNotificationList::from(context.contents().clone());
            collection.append(&mut warnings);
            collection.append(&mut errors);
            Err(collection)
        } else {
            unimplemented!()
        }
    }
}