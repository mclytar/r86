use crate::parser::prelude::*;
use super::binary::UnsolvedBinary;

#[derive(Clone, Debug)]
pub struct UnsolvedOperand {
    reference: usize,
    size: usize,
    operand: Operand
}
impl UnsolvedOperand {
    pub fn new_u8(payload: &mut Vec<u8>, operand: Operand) -> UnsolvedOperand {
        let reference = payload.len();
        payload.push(0);
        let size = 1;
        UnsolvedOperand { reference, size, operand }
    }

    pub fn new_u16(payload: &mut Vec<u8>, operand: Operand) -> UnsolvedOperand {
        let reference = payload.len();
        payload.push(0);
        payload.push(0);
        let size = 2;
        UnsolvedOperand { reference, size, operand }
    }

    pub fn new_u32(payload: &mut Vec<u8>, operand: Operand) -> UnsolvedOperand {
        let reference = payload.len();
        payload.push(0);
        payload.push(0);
        payload.push(0);
        payload.push(0);
        let size = 4;
        UnsolvedOperand { reference, size, operand }
    }

    pub fn reference(&self) -> usize {
        self.reference
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn check_references(&self, binary: &UnsolvedBinary) {
        self.operand.check_references(binary);
    }

    pub fn try_solve(&self, binary: &UnsolvedBinary, section: Option<&String>) -> Option<i32> {
        match self.operand.kind() {
            OperandKind::Expression(expr)
            | OperandKind::Address(_, expr)
            => match expr.try_eval_offset(binary) {
                Ok((Some(sec), Some(val))) => if sec.as_ref() == section { Some(val) } else { None },
                Ok((None, Some(val))) => Some(val),
                _ => None
            },
            OperandKind::SegmentOf(_label) => unimplemented!(),
            _ => unreachable!()
        }
    }
}