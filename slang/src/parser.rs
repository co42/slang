use std::fmt;

use anyhow::bail;

use crate::compiler::{BinaryOp, Expr, Func, UnaryOp, Value};
use crate::error::source_error;
use crate::lexer::Keyword::*;
use crate::lexer::Symbol::*;
use crate::lexer::Token::*;
use crate::lexer::{self, Lexer, TokenLoc};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl Prefix {
    fn binding_power(&self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 11,
            Self::Not => 14,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Assign,
}

impl Infix {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Assign => (3, 4),
            Self::And | Self::Or | Self::Xor => (5, 6),
            Self::Eq | Self::Ne | Self::Lt | Self::Lte | Self::Gt | Self::Gte => (7, 8),
            Self::Add | Self::Sub => (9, 10),
            Self::Mul | Self::Div | Self::Mod => (11, 12),
        }
    }
}

fn error(input: &str, token: &TokenLoc, message: impl ToString) -> String {
    source_error(input, token.start, token.end, message)
}

pub struct Parser {
    pub root: Expr,
}

impl Parser {
    pub fn new(root: Expr) -> Self {
        Self { root }
    }

    pub fn parse(mut lexer: Lexer) -> anyhow::Result<Self> {
        let root = Self::expr(&mut lexer, 0)?;

        Ok(Self::new(root))
    }

    fn expr(lexer: &mut Lexer, min_bp: u8) -> anyhow::Result<Expr> {
        let mut lhs = match lexer.next() {
            Value(value) => Expr::Value(match value {
                lexer::Value::Null => Value::Null,
                lexer::Value::Bool(value) => Value::Bool(*value),
                lexer::Value::Int(value) => Value::Int(*value),
                lexer::Value::Float(value) => Value::Float(*value),
            }),
            Ident(ident) => {
                let ident = ident.clone();
                if lexer.peek() == &Symbol(OpenParen) {
                    lexer.next();
                    let mut args = Vec::new();
                    loop {
                        match lexer.peek() {
                            Symbol(CloseParen) => {
                                lexer.next();
                                break;
                            },
                            _ => args.push(Self::expr(lexer, 0)?),
                        }
                    }
                    Expr::Call {
                        func: Box::new(Expr::Var { ident }),
                        args,
                    }
                } else {
                    Expr::Var { ident }
                }
            },
            Symbol(Add) => Self::expr(lexer, Prefix::Plus.binding_power())?,
            Symbol(Sub) => {
                let rhs = Self::expr(lexer, Prefix::Minus.binding_power())?;
                Expr::UnaryOp {
                    op: UnaryOp::Negate,
                    operand: Box::new(rhs),
                }
            },
            Keyword(Not) => {
                let rhs = Self::expr(lexer, Prefix::Not.binding_power())?;
                Expr::UnaryOp {
                    op: UnaryOp::Not,
                    operand: Box::new(rhs),
                }
            },
            Symbol(OpenParen) => {
                let expr = Self::expr(lexer, 0)?;
                let token = lexer.next();
                if token != &Symbol(CloseParen) {
                    let token = &lexer.tokens[lexer.index - 1];
                    bail!(error(&lexer.input, token, format!("Expected ')' got '{}'", token.token),));
                }
                expr
            },
            Symbol(OpenBrace) => {
                let mut exprs = Vec::new();
                loop {
                    match lexer.peek() {
                        Symbol(CloseBrace) => {
                            lexer.next();
                            break;
                        },
                        Symbol(Semi) => {
                            lexer.next();
                        },
                        _ => exprs.push(Self::expr(lexer, 0)?),
                    }
                }
                Expr::Block { exprs }
            },
            Symbol(Pipe) => {
                let mut args = Vec::new();
                loop {
                    match lexer.next() {
                        Ident(ident) => args.push(ident.clone()),
                        Symbol(Pipe) => break,
                        _ => {
                            let token = &lexer.tokens[lexer.index - 1];
                            bail!(error(&lexer.input, token, format!("Expected identifier got '{}'", token.token),))
                        },
                    }
                }
                let body = Box::new(Expr::Return(Box::new(Self::expr(lexer, 0)?)));
                Expr::Value(Value::Func(Func { args, body }))
            },
            Keyword(If) => {
                let cond = Box::new(Self::expr(lexer, 0)?);
                if lexer.peek() != &Symbol(OpenBrace) {
                    let token = &lexer.tokens[lexer.index];
                    bail!(error(&lexer.input, token, format!("Expected '{{' got '{}'", token.token)))
                }
                let if_body = Box::new(Self::expr(lexer, 0)?);
                if lexer.next() != &Keyword(Else) {
                    let token = &lexer.tokens[lexer.index - 1];
                    bail!(error(&lexer.input, token, format!("Expected 'else' got '{}'", token.token),))
                }
                if lexer.peek() != &Symbol(OpenBrace) {
                    let token = &lexer.tokens[lexer.index];
                    bail!(error(&lexer.input, token, format!("Expected '{{' got '{}'", token.token)))
                }
                let else_body = Box::new(Self::expr(lexer, 0)?);
                Expr::IfElse {
                    cond,
                    if_body,
                    else_body,
                }
            },
            Keyword(While) => {
                let cond = Box::new(Self::expr(lexer, 0)?);
                if lexer.peek() != &Symbol(OpenBrace) {
                    let token = &lexer.tokens[lexer.index];
                    bail!(error(&lexer.input, token, format!("Expected '{{' got '{}'", token.token)))
                }
                let body = Box::new(Self::expr(lexer, 0)?);
                Expr::While { cond, body }
            },
            _ => {
                let token = &lexer.tokens[lexer.index - 1];
                bail!(error(&lexer.input, token, format!("Expected value got '{}'", token.token),))
            },
        };

        loop {
            let infix = match lexer.peek() {
                Symbol(Add) => Infix::Add,
                Symbol(Sub) => Infix::Sub,
                Symbol(Mul) => Infix::Mul,
                Symbol(Div) => Infix::Div,
                Symbol(Mod) => Infix::Mod,
                Keyword(And) => Infix::And,
                Keyword(Or) => Infix::Or,
                Keyword(Xor) => Infix::Xor,
                Symbol(Eq) => Infix::Eq,
                Symbol(Ne) => Infix::Ne,
                Symbol(Lt) => Infix::Lt,
                Symbol(Lte) => Infix::Lte,
                Symbol(Gt) => Infix::Gt,
                Symbol(Gte) => Infix::Gte,
                Symbol(Assign) => Infix::Assign,
                _ => break,
            };

            let (l_bp, r_bp) = infix.binding_power();
            if l_bp < min_bp {
                break;
            }

            lexer.next();
            let rhs = Self::expr(lexer, r_bp)?;

            lhs = match infix {
                Infix::Add => Expr::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Sub => Expr::BinaryOp {
                    op: BinaryOp::Sub,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Mul => Expr::BinaryOp {
                    op: BinaryOp::Mul,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Div => Expr::BinaryOp {
                    op: BinaryOp::Div,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Mod => Expr::BinaryOp {
                    op: BinaryOp::Mod,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::And => Expr::BinaryOp {
                    op: BinaryOp::And,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Or => Expr::BinaryOp {
                    op: BinaryOp::Or,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Xor => Expr::BinaryOp {
                    op: BinaryOp::Xor,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Eq => Expr::BinaryOp {
                    op: BinaryOp::Eq,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Ne => Expr::BinaryOp {
                    op: BinaryOp::Ne,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Lt => Expr::BinaryOp {
                    op: BinaryOp::Lt,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Lte => Expr::BinaryOp {
                    op: BinaryOp::Lte,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Gt => Expr::BinaryOp {
                    op: BinaryOp::Gt,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Gte => Expr::BinaryOp {
                    op: BinaryOp::Gte,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                Infix::Assign => {
                    if let Expr::Var { ident } = lhs {
                        Expr::Assign {
                            ident,
                            expr: Box::new(rhs),
                        }
                    } else {
                        bail!(error(&lexer.input, &lexer.tokens[lexer.index], "Expected variable"))
                    }
                },
            };
        }

        Ok(lhs)
    }
}

impl fmt::Debug for Parser {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // writeln!(f, "---")?;
        // let node_count = self.root.fmt_ident(f, 0)?;
        // writeln!(f, "---")?;
        // write!(f, "{node_count} nodes")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::expr::*;
    use crate::compiler::BinaryOp::*;
    use crate::compiler::Expr;
    use crate::compiler::UnaryOp::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test(input: &str, expected: Expr) {
        let lexer = Lexer::lex(input).expect("Failed to lex");
        let parser = Parser::parse(lexer).expect("Failed to parse");
        assert_eq!(parser.root, expected);
    }

    #[test]
    fn test_arith_op() {
        test("1 / 2 + 2 * 3", bop(Add, bop(Div, value(1), value(2)), bop(Mul, value(2), value(3))));
    }

    #[test]
    fn test_negate() {
        test("-1 + -2", bop(Add, uop(Negate, value(1)), uop(Negate, value(2))));
        test("-+++-42", uop(Negate, uop(Negate, value(42))));
    }

    #[test]
    fn test_logic_op() {
        test("not true and not false", bop(And, uop(Not, value(true)), uop(Not, value(false))));
        test("true or true", bop(Or, value(true), value(true)));
        test("true xor false", bop(Xor, value(true), value(false)));
    }

    #[test]
    fn test_rel_op() {
        test("1 < 2 and 2 >= 3", bop(And, bop(Lt, value(1), value(2)), bop(Gte, value(2), value(3))));
        test(
            "true == false and false != true",
            bop(And, bop(Eq, value(true), value(false)), bop(Ne, value(false), value(true))),
        );
    }

    #[test]
    fn test_paren() {
        test("(1 + 2) * ((4 / 2))", bop(Mul, bop(Add, value(1), value(2)), bop(Div, value(4), value(2))));
    }

    #[test]
    fn test_assign() {
        test("a = 1 + 2", assign("a", bop(Add, value(1), value(2))));
    }

    #[test]
    fn test_block() {
        test("2 * {1 + 2}", bop(Mul, value(2), block([bop(Add, value(1), value(2))])));
        test(
            "{a = 10; {b = 2; a * b} * a}",
            block([
                assign("a", value(10)),
                bop(Mul, block([assign("b", value(2)), bop(Mul, var("a"), var("b"))]), var("a")),
            ]),
        )
    }

    #[test]
    fn test_func() {
        test(
            "{add = |a b| a + b; add(2 3)}",
            block([
                assign("add", func(["a", "b"], bop(Add, var("a"), var("b")))),
                call(var("add"), [value(2), value(3)]),
            ]),
        );
    }

    #[test]
    fn test_if_else() {
        test("if true {1} else {2}", if_else(value(true), block([value(1)]), block([value(2)])));
    }

    #[test]
    fn test_while() {
        test("while true {1}", while_(value(true), block([value(1)])));
    }
}
