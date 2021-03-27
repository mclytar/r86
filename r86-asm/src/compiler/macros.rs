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
    (@operands($location:expr, $condition:expr): 2 $operands:expr) => {
        if $operands.len() == 2 {
            if ($operands[0].size() == Some(SizeConstraint::Byte) && $operands[1].size() == Some(SizeConstraint::Word))
                || ($operands[0].size() == Some(SizeConstraint::Word) && $operands[1].size() == Some(SizeConstraint::Byte)) {
                return Err(Notification::error_incompatible_operand_size(&$operands[0], &$operands[1]));
            } else {
                Some(($operands[0].kind(), $operands[1].kind(), $condition))
            }
        } else {
            None
        }
    };
    (@operands($location:expr, $condition:expr): 1 $operands:expr) => {
        if $operands.len() == 1 {
            Some(($operands[0].kind(), $condition))
        } else {
            None
        }
    };

    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Reg($reg:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg), false)) = match_operands!(@operands($location, $found): 1 $operands) {
            $found = true;
            let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] SegReg($seg_reg:ident) => $code:block) => {
        if let Some((OperandKind::Reg($seg_reg), false)) = match_operands!(@operands($location, $found): 1 $operands) {
            $found = true;
            let $word = true;
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Expression($expr), false)) = match_operands!(@operands($location, $found): 1 $operands) {
            $found = true;
            let $word = $operands[0].size();
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::Address($segment, $address), false)) = match_operands!(@operands($location, $found): 1 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            $found = true;
            let $word = $operands[0].size();
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Acc <$dir:ident> Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::Reg(0), OperandKind::Address($segment, $address), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if $address.effective_address().is_none() {
                if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
                $found = true;
                let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
                let $dir = false;
                $code
            }
        } else if let Some((OperandKind::Address($segment, $address), OperandKind::Reg(0), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if $address.effective_address().is_none() {
                if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
                $found = true;
                let $word = $operands[1].size().unwrap() == SizeConstraint::Word;
                let $dir = true;
                $code
            }
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Acc <- Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Reg(0), OperandKind::Expression($expr), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if $expr.effective_address().is_none() {
                $found = true;
                let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
                $code
            } else {
                return Err(Notification::error_invalid_expression(&$expr));
            }
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Reg($reg:ident) <- Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg), OperandKind::Expression($expr), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if $expr.effective_address().is_none() {
                $found = true;
                let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
                $code
            } else {
                return Err(Notification::error_invalid_expression(&$expr));
            }
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Mem($segment:ident, $address:ident) <- Imm($expr:ident) => $code:block) => {
        if let Some((OperandKind::Address($segment, $address), OperandKind::Expression($expr), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if $expr.effective_address().is_none() {
                if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
                $found = true;
                let $word = $operands[0].size();
                $code
            } else {
                return Err(Notification::error_invalid_expression(&$expr));
            }
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Reg($reg1:ident) <- Reg($reg2:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg1), OperandKind::Reg($reg2), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            $found = true;
            let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] Reg($reg:ident) <$dir:ident> Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::Reg($reg), OperandKind::Address($segment, $address), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            $found = true;
            let $word = $operands[0].size().unwrap() == SizeConstraint::Word;
            let $dir = false;
            $code
        } else if let Some((OperandKind::Address($segment, $address), OperandKind::Reg($reg), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            $found = true;
            let $word = $operands[1].size().unwrap() == SizeConstraint::Word;
            let $dir = true;
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] SegReg($seg_reg:ident) <$dir:ident> Mem($segment:ident, $address:ident) => $code:block) => {
        if let Some((OperandKind::SegmentReg($seg_reg), OperandKind::Address($segment, $address), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            $found = true;
            let $word = true;
            let $dir = false;
            $code
        } else if let Some((OperandKind::Address($segment, $address), OperandKind::SegmentReg($seg_reg), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            if let Some(segment) = $segment { $payload.push(0b001_00_110 | *segment); }
            $found = true;
            let $word = true;
            let $dir = true;
            $code
        }
    };
    (@if_let($binary:expr, $payload:expr, $location:expr, $operands:expr, $found:ident): [$word:ident] SegReg($seg_reg:ident) <$dir:ident> Reg($reg:ident) => $code:block) => {
        if let Some((OperandKind::SegmentReg($seg_reg), OperandKind::Reg($reg), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            $found = true;
            let $word = true;
            let $dir = false;
            $code
        } else if let Some((OperandKind::Reg($reg), OperandKind::SegmentReg($seg_reg), false)) = match_operands!(@operands($location, $found): 2 $operands) {
            $found = true;
            let $word = true;
            let $dir = true;
            $code
        }
    };
    ($binary:expr, $payload:expr, $location:expr, $operands:expr;
        $( ( $($params:tt)+ ) => $code:block, )+
        default => $default:block
    ) => {
        let mut found = false;
        $(
            match_operands!(@if_let($binary, $payload, $location, $operands, found): $($params)+ => $code);
        )+
        if !found $default
    };
    ($location:expr, $operands:expr; $( [$opr1:pat] <$dir:ident> [$opr2:pat] => $code:block ),+) => {
        match operands!($location; $operands, 2)? {
            $(
                ($opr1, $opr2) => {
                    let $dir = false;
                    $code
                },
                ($opr2, $opr1) => {
                    let $dir = true;
                    $code
                },
            )+
            _ => unimplemented!()
        }
    };
    ($binary:expr, $location:expr, $operands:expr; [$word:ident] Acc <$dir:ident> Mem($segment:ident, $expr:ident) => $code:block) => {{
        let parser = |$word, $dir, $segment, $expr: &Expression| $code;
        match operands!($location; $operands, 2) {
            Err(err) => Err(err),
            Ok((op0, op1)) => {

            }
        }
    }};
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