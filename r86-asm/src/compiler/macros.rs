/// Puts boolean flags or registers into a byte.
macro_rules! byte {
    (@param(d): $value:expr) => {{ if $value { 0x02 } else { 0x00 } }};
    (@param(s): $value:expr) => {{ if $value { 0x02 } else { 0x00 } }};
    (@param(v): $value:expr) => {{ if $value { 0x02 } else { 0x00 } }};
    (@param(w): $value:expr) => {{ if $value { 0x01 } else { 0x00 } }};
    (@param(wh): $value:expr) => {{ if $value { 0x08 } else { 0x00 } }};
    (@param(reg): $value:expr) => {{ $value }};
    (@param(regh): $value:expr) => {{ $value << 3 }};
    ($base:literal, $( $param:ident = $value:expr ),+) => {{
        let mut base = $base;
        $( base |= byte!(@param($param): $value); )+
        base
    }}
}



/// Asserts that the number of operands is the correct one.
macro_rules! expect_operands {
    ($location:expr; $operands:expr, $n:literal) => {
        if $operands.len() != $n {
            return Err(Notification::error_operands_amount($location, $n, $operands.len(), &$operands));
        }
    };
    ($location:expr; $operands:expr, $min:literal .. $max:literal) => {
        if $operands.len() < $min || $operands.len() >= $max {
            return Err(Notification::error_operands_range($location, $min..$max, $operands.len(), &$operands));
        }
    };
}



/// Generates rules for operand-matching.
macro_rules! match_operands {
    (@operands($location:expr): 2 $operands:expr) => {
        if $operands.len() == 2 {
            if ($operands[0].size() == Some(SizeConstraint::Byte) && $operands[1].size() == Some(SizeConstraint::Word))
                || ($operands[0].size() == Some(SizeConstraint::Word) && $operands[1].size() == Some(SizeConstraint::Byte)) {
                return Err(Notification::error_incompatible_operand_size(&$operands[0], &$operands[1]));
            } else {
                Some(($operands[0].kind(), $operands[1].kind()))
            }
        } else {
            None
        }
    };
    (@operands($location:expr): 1 $operands:expr) => {
        if $operands.len() == 1 {
            Some($operands[0].kind())
        } else {
            None
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Reg($reg:ident) => $code:block) => {
        if let Some(OperandKind::Reg($reg)) = match_operands!(@operands($location): 1 $operands) {
            let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] SegReg($seg_reg:ident) => $code:block) => {
        if let Some(OperandKind::Reg($seg_reg)) = match_operands!(@operands($location): 1 $operands) {
            let $word = true;
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): Imm($expr:ident) => $code:block) => {
        if let Some(OperandKind::Expression($expr)) = match_operands!(@operands($location): 1 $operands) {
            if $operands[0].size().is_some() {
                $binary.warn(Notification::warning_ignored_size_specifier(&$operands[0].locate().limit(4)));
            }
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Imm($expr:ident) => $code:block) => {
        if let Some(OperandKind::Expression($expr)) = match_operands!(@operands($location): 1 $operands) {
            let $word = $operands[0].size();
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some(OperandKind::Address($segment, $address)) = match_operands!(@operands($location): 1 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | (*segment << 3)); }
            if $operands[0].size().is_some() {
                $binary.warn(Notification::warning_ignored_size_specifier(&$operands[0].locate().limit(4)));
            }
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some(OperandKind::Address($segment, $address)) = match_operands!(@operands($location): 1 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | (*segment << 3)); }
            let $word = $operands[0].size();
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Acc <$dir:ident> Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::Reg(0), OperandKind::Address($segment, $address))) = match_operands!(@operands($location): 2 $operands) {
            if $address.effective_address().is_none() {
                if let Some(segment) = $segment { $payload.push(0b001_00_110 | (*segment << 3)); }
                let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
                let $dir = false;
                Some($code)
            } else { None }
        } else if let Some((OperandKind::Address($segment, $address), OperandKind::Reg(0))) = match_operands!(@operands($location): 2 $operands) {
            if $address.effective_address().is_none() {
                if let Some(segment) = $segment { $payload.push(0b001_00_110 | (*segment << 3)); }
                let $word = $operands[1].size().unwrap() == SizeConstraint::Word;
                let $dir = true;
                Some($code)
            } else { None }
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Acc <- Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Reg(0), OperandKind::Expression($expr))) = match_operands!(@operands($location): 2 $operands) {
            if $expr.effective_address().is_none() {
                let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
                Some($code)
            } else {
                return Err(Notification::error_invalid_expression(&$expr));
            }
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Reg($reg:ident) <- Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg), OperandKind::Expression($expr))) = match_operands!(@operands($location): 2 $operands) {
            if $expr.effective_address().is_none() {
                let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
                Some($code)
            } else {
                return Err(Notification::error_invalid_expression(&$expr));
            }
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Mem($segment:ident, $address:ident) <- Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Address($segment, $address), OperandKind::Expression($expr))) = match_operands!(@operands($location): 2 $operands) {
            if $expr.effective_address().is_none() {
                if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
                let $word = match ($operands[0].size(), $operands[1].size()) {
                    (Some(size), None) => size,
                    (None, Some(size)) => size,
                    (Some(size1), Some(size2)) => if size1 == size2 { size1 } else { return Err(Notification::error_unimplemented($location)); },
                    (None, None) => return Err(Notification::error_unimplemented($location))
                } == SizeConstraint::Word;
                Some($code)
            } else {
                return Err(Notification::error_invalid_expression(&$expr));
            }
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Reg($reg1:ident) <- Reg($reg2:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg1), OperandKind::Reg($reg2))) = match_operands!(@operands($location): 2 $operands) {
            let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): [$word:ident] Reg($reg:ident) <$dir:ident> Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg), OperandKind::Address($segment, $address))) = match_operands!(@operands($location): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
            let $dir = true;
            Some($code)
        } else if let Some((OperandKind::Address($segment, $address), OperandKind::Reg($reg))) = match_operands!(@operands($location): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            let $word = $operands[1].size().unwrap() == SizeConstraint::Word;
            let $dir = false;
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): SegReg($seg_reg:ident) <$dir:ident> Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::SegmentReg($seg_reg), OperandKind::Address($segment, $address))) = match_operands!(@operands($location): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            let $dir = true;
            Some($code)
        } else if let Some((OperandKind::Address($segment, $address), OperandKind::SegmentReg($seg_reg))) = match_operands!(@operands($location): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            let $dir = false;
            Some($code)
        } else { None }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr): SegReg($seg_reg:ident) <$dir:ident> Reg($reg:ident) => $code:block) => {
        if let Some((OperandKind::SegmentReg($seg_reg), OperandKind::Reg($reg))) = match_operands!(@operands($location): 2 $operands) {
            let $dir = true;
            Some($code)
        } else if let Some((OperandKind::Reg($reg), OperandKind::SegmentReg($seg_reg))) = match_operands!(@operands($location): 2 $operands) {
            let $dir = false;
            Some($code)
        } else { None }
    };
    ($binary:expr, $payload:expr, $location:expr, $operands:expr;
        $( ( $($params:tt)+ ) => $code:block ),+
    ) => {
        #[allow(unused_assignments)]
        let mut result = None;
        $(
            if result.is_none() { result = match_operands!(@if_let($binary, $payload, $location, $operands): $($params)+ => $code); }
        )+
        if result.is_none() {
            return Err(Notification::error_invalid_set_of_operands($location, &$operands[..]));
        }
        result.unwrap();
    };
}



macro_rules! push_modrm {
    ($binary:expr, $operands:expr, $unsolved_ops:expr, $payload:expr; [$word:ident] $reg:ident <$dir:ident> $address:ident) => {
        if let (_, Some(offset)) = $address.try_eval_offset($binary)? {
            if let Some(ea) = $address.effective_address() {
                if offset == 0 && ea as u8 != 6 { $payload.push(byte!(0b00_000_000, regh = $reg, reg = ea as u8)); }
                else if offset < 0x80 || offset >= 0xFF80 {
                    $payload.push(byte!(0b01_000_000, regh = $reg, reg = ea as u8));
                    $payload.push(offset as u8);
                } else {
                    $payload.push(byte!(0b10_000_000, regh = $reg, reg = ea as u8));
                    push_word!($payload, offset);
                }
            } else {
                $payload.push(byte!(0b00_000_000, regh = $reg, reg = 6));
                push_word!($payload, offset);
            }
        } else {
            if let Some(ea) = $address.effective_address() {
                $payload.push(byte!(0b10_000_000, regh = $reg, reg = ea as u8));
            } else {
                $payload.push(byte!(0b00_000_000, regh = $reg, reg = 6));
            }
            let operand = if $dir { $operands[1].to_owned() } else { $operands[0].to_owned() };
            $unsolved_ops.push(UnsolvedOperand::new_u16(&mut $payload, operand));
        }
    }
}



/// Resizes expressions.
macro_rules! resize {
    ($value:expr) => {{
        let value = if $value > 0 { $value % 65536 } else { 65536 - ($value % 65536) };
        if value < 0x80 || value >= 0xFF80 {
            (false, value as u16)
        } else {
            (true, value as u16)
        }
    }};
}

macro_rules! push_word {
    ($payload:expr, $word:expr) => {{
        $payload.push(($word & 0xFF) as u8);
        $payload.push(($word >> 8) as u8);
    }};
}

macro_rules! push_operand {
    ($binary:expr, $operand:expr, $unsolved_ops:expr, $payload:expr; [true] $value:expr) => {{
        if let (_, Some(offset)) = $value.try_eval_offset($binary)? {
            push_word!($payload, offset);
        } else {
            let operand = $operand.to_owned();
            $unsolved_ops.push(UnsolvedOperand::new_u16(&mut $payload, operand));
        }
    }};
    ($binary:expr, $operand:expr, $unsolved_ops:expr, $payload:expr; [false] $value:expr) => {{
        if let (_, Some(offset)) = $value.try_eval_offset($binary)? {
            $payload.push(offset as u8);
        } else {
            let operand = $operand.to_owned();
            $unsolved_ops.push(UnsolvedOperand::new_u8(&mut $payload, operand));
        }
    }};
    ($binary:expr, $operand:expr, $unsolved_ops:expr, $payload:expr; [$word:expr] $value:expr) => {{
        if let (_, Some(offset)) = $value.try_eval_offset($binary)? {
            if $word { push_word!($payload, offset); } else { $payload.push(offset as u8); }
        } else {
            let operand = $operand.to_owned();
            if $word {
                $unsolved_ops.push(UnsolvedOperand::new_u16(&mut $payload, operand));
            } else {
                $unsolved_ops.push(UnsolvedOperand::new_u8(&mut $payload, operand));
            }
        }
    }};
}