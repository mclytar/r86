use crate::error::{error, CompilerResult};
use crate::token::{Token, TokenClass};
use crate::{INSTRUCTION, PREFIX_INSTRUCTION, PSEUDO_INSTRUCTION};

/// Defines an arithmetic operation to use inside an expression.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div
}
impl Operation {
    pub fn try_parse(token: &Token) -> Option<Self> {
        if token.class() != TokenClass::Operator {
            None
        } else {
            let op = match token.as_ref().chars().next().unwrap() {
                '+' => Operation::Add,
                '-' => Operation::Sub,
                '*' => Operation::Mul,
                '/' => Operation::Div,
                _ => unreachable!()
            };
            Some(op)
        }
    }
}

/// Defines a component of an expression, that is, either a single item or an arithmetic operation between two items.
#[derive(Clone, Debug)]
pub enum ExpressionItem<'a> {
    Terminal(Token<'a>),
    Operation {
        operand1: Box<Expression<'a>>,
        operation: Operation,
        operand2: Box<Expression<'a>>
    }
}

/// Defines an expression.
#[derive(Clone, Debug)]
pub struct Expression<'a> {
    critical: bool,
    value: ExpressionItem<'a>
}
impl<'a> Expression<'a> {
    fn from_rpn(stack: &mut Vec<Token<'a>>) -> Option<Expression<'a>> {
        if let Some(item) = stack.pop() {
            let value = match item.class() {
                TokenClass::Reg8
                | TokenClass::Reg16
                | TokenClass::RegSegment
                | TokenClass::NumericLiteral
                | TokenClass::StringLiteral
                | TokenClass::Ident
                | TokenClass::InstructionStart
                | TokenClass::SectionStart
                => {
                    ExpressionItem::Terminal(item)
                },
                TokenClass::Operator => {
                    let operand2 = Box::new(Expression::from_rpn(stack)?);
                    let operand1 = Box::new(Expression::from_rpn(stack)?);
                    let operation = Operation::try_parse(&item).unwrap();

                    ExpressionItem::Operation {
                        operand1,
                        operation,
                        operand2
                    }
                },
                _ => unreachable!()
            };

            let expr = Expression { critical: false, value };

            Some(expr)
        } else {
            None
        }
    }

    pub fn try_accept_critical(stream: &mut &[Token<'a>]) -> CompilerResult<Option<Expression<'a>>> {
        let expr = Self::try_accept(stream)?;
        let expr = if let Some(mut expr) = expr {
            expr.set_critical();
            Some(expr)
        } else {
            None
        };
        Ok(expr)
    }

    pub fn try_accept<'b>(stream: &mut &'b [Token<'a>]) -> CompilerResult<Option<Expression<'a>>> {
        let stream_slice_preserve = &stream[..];

        let mut output_stack = Vec::new();
        let mut operator_stack: Vec<Token> = Vec::new();
        let mut open_brackets: usize = 0;
        let mut previous_token_class = None;

        let err_expect_op_or_end = |s: &mut &'b [Token<'a>], ob: usize| {
            let err = error::syntax_error(
                format!("expected `+`, `-`, `*`, `/`, `(`{} or end of expression, found `{}`", if ob > 0 { ", `)`" } else { "" }, s[0].as_ref()),
                s[0].line(),
                s[0].column(),
                s[0].len()
            );
            *s = stream_slice_preserve;
            err
        };
        let err_expect_expr = |s: &mut &'b [Token<'a>]| {
            let err = error::syntax_error(
                format!("expected expression, found `{}`", s[0].as_ref()),
                s[0].line(),
                s[0].column(),
                s[0].len()
            );
            *s = stream_slice_preserve;
            err
        };

        while stream.len() > 0 {
            match stream[0].class() {
                TokenClass::Reg8
                | TokenClass::Reg16
                | TokenClass::RegSegment
                | TokenClass::NumericLiteral
                | TokenClass::StringLiteral
                | TokenClass::Ident
                | TokenClass::InstructionStart
                | TokenClass::SectionStart
                => {
                    match previous_token_class {
                        Some(TokenClass::Reg8)
                        | Some(TokenClass::Reg16)
                        | Some(TokenClass::RegSegment)
                        | Some(TokenClass::NumericLiteral)
                        | Some(TokenClass::StringLiteral)
                        | Some(TokenClass::InstructionStart)
                        | Some(TokenClass::SectionStart)
                        | Some(TokenClass::RightParen)
                        | Some(TokenClass::Ident)
                        => {
                            let err = if stream[0].class() == TokenClass::Ident
                                && (INSTRUCTION.iter().any(|i| *i == stream[0].as_ref())
                                    || PREFIX_INSTRUCTION.iter().any(|i| *i == stream[0].as_ref())
                                    || PSEUDO_INSTRUCTION.iter().any(|i| *i == stream[0].as_ref())
                                ) {
                                if open_brackets == 0 {
                                    break;
                                } else {
                                    err_expect_op_or_end(stream, open_brackets)
                                        .build_hint(String::from("perhaps a missing `)`?"))
                                }
                            } else {
                                err_expect_op_or_end(stream, open_brackets)
                            };
                            return Err(err);
                        },
                        _ => {}
                    }
                    previous_token_class = Some(stream[0].class());
                    output_stack.push(stream[0].clone());
                    *stream = &stream[1..];
                },
                TokenClass::Operator => {
                    match previous_token_class {
                        None
                        | Some(TokenClass::Operator)
                        | Some(TokenClass::LeftParen)
                        => {
                            let err = err_expect_expr(stream);
                            return Err(err);
                        },
                        _ => {}
                    }

                    while let Some(last) = operator_stack.pop() {
                        if last.class() == TokenClass::Operator &&
                            (last.as_ref().contains(|c: char| c == '*' || c == '/')
                                || (last.as_ref().contains(|c: char| c == '+' || c == '-') && stream[0].as_ref().contains(|c: char| c == '+' || c == '-'))) {
                            output_stack.push(last);
                        } else {
                            operator_stack.push(last);
                            break;
                        }
                    }

                    previous_token_class = Some(stream[0].class());
                    operator_stack.push(stream[0].clone());
                    *stream = &stream[1..];
                },
                TokenClass::LeftParen => {
                    match previous_token_class {
                        Some(TokenClass::Reg8)
                        | Some(TokenClass::Reg16)
                        | Some(TokenClass::RegSegment)
                        | Some(TokenClass::NumericLiteral)
                        | Some(TokenClass::StringLiteral)
                        | Some(TokenClass::Ident)
                        | Some(TokenClass::InstructionStart)
                        | Some(TokenClass::SectionStart)
                        | Some(TokenClass::RightParen)
                        => {
                            let err = err_expect_op_or_end(stream, open_brackets);
                            return Err(err);
                        },
                        _ => {}
                    }
                    open_brackets += 1;
                    previous_token_class = Some(stream[0].class());
                    operator_stack.push(stream[0].clone());
                    *stream = &stream[1..];
                },
                TokenClass::RightParen => {
                    match previous_token_class {
                        None
                        | Some(TokenClass::Operator)
                        | Some(TokenClass::LeftParen)
                        => {
                            let err = err_expect_expr(stream);
                            return Err(err);
                        },
                        _ => {}
                    }
                    if open_brackets == 0 {
                        let err = error::syntax_error(
                            String::from("mismatched closing delimiter"),
                            stream[0].line(),
                            stream[0].column(),
                            stream[0].len()
                        );
                        *stream = stream_slice_preserve;
                        return Err(err);
                    }
                    while let Some(last) = operator_stack.pop() {
                        if last.class() != TokenClass::LeftParen {
                            output_stack.push(last);
                        } else {
                            break;
                        }
                    }
                    open_brackets -= 1;
                    *stream = &stream[1..];
                },
                TokenClass::EndOfLine
                | TokenClass::Comma
                | TokenClass::RightSquareParen
                => {
                    if open_brackets > 0 {
                        let mut open_bracket = None;
                        while let Some(last) = operator_stack.pop() {
                            if last.class() == TokenClass::LeftParen {
                                open_bracket = Some(last);
                                break;
                            }
                        }
                        let open_bracket = open_bracket.unwrap();
                        let err = error::syntax_error(
                            String::from("mismatched opening delimiter"),
                            open_bracket.line(),
                            open_bracket.column(),
                            open_bracket.len()
                        );
                        *stream = stream_slice_preserve;
                        return Err(err);
                    } else {
                        break;
                    }
                },
                _ => {
                    match previous_token_class {
                        Some(TokenClass::Reg8)
                        | Some(TokenClass::Reg16)
                        | Some(TokenClass::RegSegment)
                        | Some(TokenClass::NumericLiteral)
                        | Some(TokenClass::StringLiteral)
                        | Some(TokenClass::Ident)
                        | Some(TokenClass::InstructionStart)
                        | Some(TokenClass::SectionStart)
                        | Some(TokenClass::RightParen)
                        => {
                            let err = err_expect_op_or_end(stream, open_brackets);
                            return Err(err);
                        },
                        _ => {
                            let err = err_expect_expr(stream);
                            return Err(err);
                        }
                    }
                }
            }
        }

        while let Some(last) = operator_stack.pop() {
            output_stack.push(last);
        }

        Ok(Self::from_rpn(&mut output_stack))
    }

    pub fn is_critical(&self) -> bool {
        self.critical
    }

    pub fn set_critical(&mut self) {
        self.critical = true;

        if let ExpressionItem::Operation { ref mut operand1, ref mut operand2, .. } = self.value {
            operand1.set_critical();
            operand2.set_critical();
        }
    }

    pub fn set_non_critical(&mut self) {
        self.critical = true;

        if let ExpressionItem::Operation { ref mut operand1, ref mut operand2, .. } = self.value {
            operand1.set_non_critical();
            operand2.set_non_critical();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::{TokenStream, TokenClass};
    use crate::statement::expression::{Expression, ExpressionItem, Operation};

    macro_rules! unwrap_operation {
        ($expr:expr) => {
            if let ExpressionItem::Operation { operand1, operation, operand2 } = $expr.value {
                (operand1, operation, operand2)
            } else {
                panic!("Unexpected expression");
            }
        };
    }

    macro_rules! unwrap_terminal {
        ($expr:expr) => {
            if let ExpressionItem::Terminal(t) = $expr.value {
                t
            } else {
                panic!("Unexpected expression");
            }
        };
    }

    #[test]
    fn expr_unexpected_operand() {
        let asm = r"5 ax";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());

        let asm = r") $";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());
    }

    #[test]
    fn expr_unexpected_operator() {
        let asm = r"+ 3";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());

        let asm = r"es + -";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());

        let asm = r"7 * ( 5 + )";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());
    }

    #[test]
    fn expr_matching_bracket() {
        let asm = r"(5 + 3";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());

        let asm = r"5 + 3)";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());

        let asm = r"(5 * (8 + 2) - (1 + bx)";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());

        let asm = r"label + (5 * (8 + 2) - 1) + bx)";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        assert!(Expression::try_accept(&mut tokens).is_err());
    }

    #[test]
    fn expression() {
        let asm = r"bx + si";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let expr = Expression::try_accept(&mut tokens).unwrap().unwrap();

        let (operand1, operation, operand2) = unwrap_operation!(expr);
        assert_eq!(unwrap_terminal!(operand1).as_ref(), "bx");
        assert_eq!(unwrap_terminal!(operand2).as_ref(), "si");
        assert_eq!(operation, Operation::Add);

        let asm = r"512 - ($ - $$)";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let expr = Expression::try_accept(&mut tokens).unwrap().unwrap();

        let (number, operation, expr) = unwrap_operation!(expr);
        assert_eq!(unwrap_terminal!(number).as_ref(), "512");
        assert_eq!(operation, Operation::Sub);
        let (start_instr, operation, start_sec) = unwrap_operation!(expr);
        assert_eq!(unwrap_terminal!(start_instr).as_ref(), "$");
        assert_eq!(operation, Operation::Sub);
        assert_eq!(unwrap_terminal!(start_sec).as_ref(), "$$");
    }

    #[test]
    fn expression_stop() {
        let asm = r"(my_offset - my_base) * 2       add     bx, si";
        let stream = TokenStream::tokenize(asm).unwrap();
        let mut tokens = stream.as_ref();
        let expr = Expression::try_accept(&mut tokens).unwrap().unwrap();

        assert_eq!(tokens.iter().map(|t| t.class()).collect::<Vec<_>>(), [TokenClass::Ident, TokenClass::Reg16, TokenClass::Comma, TokenClass::Reg16, TokenClass::EndOfLine]);
        let (diff, operation, multiplier) = unwrap_operation!(expr);
        assert_eq!(operation, Operation::Mul);
        assert_eq!(unwrap_terminal!(multiplier).as_ref(), "2");
        let (my_offset, operation, my_base) = unwrap_operation!(diff);
        assert_eq!(unwrap_terminal!(my_offset).as_ref(), "my_offset");
        assert_eq!(operation, Operation::Sub);
        assert_eq!(unwrap_terminal!(my_base).as_ref(), "my_base");
    }
}