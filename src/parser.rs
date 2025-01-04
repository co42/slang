use std::fmt;

use anyhow::bail;

use crate::{
    error::source_error,
    lexer::{self, Lexer, Op, Token, TokenLoc},
};

#[derive(Clone, PartialEq)]
pub enum Node {
    Value(Value),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    And(Box<Node>, Box<Node>),
    Or(Box<Node>, Box<Node>),
    Xor(Box<Node>, Box<Node>),
    Negate(Box<Node>),
    Not(Box<Node>),
    Eq(Box<Node>, Box<Node>),
    Ne(Box<Node>, Box<Node>),
    Lt(Box<Node>, Box<Node>),
    Lte(Box<Node>, Box<Node>),
    Gt(Box<Node>, Box<Node>),
    Gte(Box<Node>, Box<Node>),
    Assign(String, Box<Node>),
    Var(String),
    Block(Vec<Node>),
}

impl Node {
    fn fmt_ident(&self, f: &mut fmt::Formatter<'_>, ident: usize) -> Result<usize, fmt::Error> {
        match self {
            Node::Value(value) => {
                writeln!(f, "{:ident$}{value}", "")?;
                Ok(1)
            }
            Node::Add(lhs, rhs) => {
                writeln!(f, "{:>ident$}+", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Sub(lhs, rhs) => {
                writeln!(f, "{:>ident$}-", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Mul(lhs, rhs) => {
                writeln!(f, "{:>ident$}*", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Div(lhs, rhs) => {
                writeln!(f, "{:>ident$}/", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::And(lhs, rhs) => {
                writeln!(f, "{:>ident$}and", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Or(lhs, rhs) => {
                writeln!(f, "{:>ident$}or", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Xor(lhs, rhs) => {
                writeln!(f, "{:>ident$}xor", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Negate(rhs) => {
                writeln!(f, "{:>ident$}-", "")?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + rhs_count)
            }
            Node::Not(rhs) => {
                writeln!(f, "{:>ident$}not", "")?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + rhs_count)
            }
            Node::Eq(lhs, rhs) => {
                writeln!(f, "{:>ident$}==", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Ne(lhs, rhs) => {
                writeln!(f, "{:>ident$}!=", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Lt(lhs, rhs) => {
                writeln!(f, "{:>ident$}<", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Lte(lhs, rhs) => {
                writeln!(f, "{:>ident$}<=", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Gt(lhs, rhs) => {
                writeln!(f, "{:>ident$}>", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Gte(lhs, rhs) => {
                writeln!(f, "{:>ident$}>=", "")?;
                let lhs_count = lhs.fmt_ident(f, ident + 2)?;
                let rhs_count = rhs.fmt_ident(f, ident + 2)?;
                Ok(1 + lhs_count + rhs_count)
            }
            Node::Assign(name, node) => {
                writeln!(f, "{:>ident$}{name}=", "")?;
                let node_count = node.fmt_ident(f, ident + 2)?;
                Ok(1 + node_count)
            }
            Node::Var(name) => {
                writeln!(f, "{:ident$}{name}", "")?;
                Ok(1)
            }
            Node::Block(node) => {
                writeln!(f, "{:>ident$}{{}}", "")?;
                let mut node_count = 0;
                for node in node {
                    node_count += node.fmt_ident(f, ident + 2)?;
                }
                Ok(1 + node_count)
            }
        }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_ident(f, 0)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(value) => write!(f, "{value}"),
            Value::Int(value) => write!(f, "{value}"),
            Value::Float(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl Prefix {
    fn binding_power(&self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 9,
            Self::Not => 12,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
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
            Infix::Assign => (1, 2),
            Infix::And | Infix::Or | Infix::Xor => (3, 4),
            Infix::Eq | Infix::Ne | Infix::Lt | Infix::Lte | Infix::Gt | Infix::Gte => (5, 6),
            Infix::Add | Infix::Sub => (7, 8),
            Infix::Mul | Infix::Div => (10, 11),
        }
    }
}

fn error(input: &str, token: &TokenLoc, message: impl ToString) -> String {
    source_error(input, token.start, token.end, message)
}

#[derive(Clone)]
pub struct Parser {
    pub root: Node,
}

impl Parser {
    pub fn new(root: Node) -> Self {
        Self { root }
    }

    pub fn parse(mut lexer: Lexer) -> anyhow::Result<Self> {
        let root = Self::expr(&mut lexer, 0)?;

        Ok(Self::new(root))
    }

    fn expr(lexer: &mut Lexer, min_bp: u8) -> anyhow::Result<Node> {
        let mut lhs = match lexer.next() {
            Token::Value(lexer::Value::Null) => Node::Value(Value::Null),
            Token::Value(lexer::Value::Bool(value)) => Node::Value(Value::Bool(*value)),
            Token::Value(lexer::Value::Int(value)) => Node::Value(Value::Int(*value)),
            Token::Value(lexer::Value::Float(value)) => Node::Value(Value::Float(*value)),
            Token::OpenParen => {
                let lhs = Self::expr(lexer, 0)?;
                let token = lexer.next();
                if token != &Token::CloseParen {
                    let token = &lexer.tokens[lexer.index - 1];
                    bail!(error(
                        &lexer.input,
                        token,
                        format!("Expected ')' got {}", token.token),
                    ));
                }
                lhs
            }
            Token::OpenBrace => {
                let mut lhs = Vec::new();
                loop {
                    let node = Self::expr(lexer, 0)?;
                    lhs.push(node);
                    match lexer.next() {
                        Token::CloseBrace => {
                            break;
                        }
                        Token::Op(Op::Semi) => {}
                        _ => {
                            let token = &lexer.tokens[lexer.index - 1];
                            bail!(error(
                                &lexer.input,
                                token,
                                format!("Expected '}}' got {}", token.token),
                            ));
                        }
                    }
                }
                Node::Block(lhs)
            }
            Token::Op(Op::Add) => Self::expr(lexer, Prefix::Plus.binding_power())?,
            Token::Op(Op::Sub) => {
                let rhs = Self::expr(lexer, Prefix::Minus.binding_power())?;
                Node::Negate(Box::new(rhs))
            }
            Token::Op(Op::Not) => {
                let rhs = Self::expr(lexer, Prefix::Not.binding_power())?;
                Node::Not(Box::new(rhs))
            }
            Token::Ident(name) => Node::Var(name.clone()),
            _ => {
                let token = &lexer.tokens[lexer.index - 1];
                bail!(error(
                    &lexer.input,
                    token,
                    format!("Expected integer got {}", token.token),
                ))
            }
        };

        loop {
            let infix = match lexer.peek() {
                Token::Eof | Token::Op(Op::Semi) | Token::CloseParen | Token::CloseBrace => break,
                Token::Op(Op::Add) => Infix::Add,
                Token::Op(Op::Sub) => Infix::Sub,
                Token::Op(Op::Mul) => Infix::Mul,
                Token::Op(Op::Div) => Infix::Div,
                Token::Op(Op::And) => Infix::And,
                Token::Op(Op::Or) => Infix::Or,
                Token::Op(Op::Xor) => Infix::Xor,
                Token::Op(Op::Eq) => Infix::Eq,
                Token::Op(Op::Ne) => Infix::Ne,
                Token::Op(Op::Lt) => Infix::Lt,
                Token::Op(Op::Lte) => Infix::Lte,
                Token::Op(Op::Gt) => Infix::Gt,
                Token::Op(Op::Gte) => Infix::Gte,
                Token::Op(Op::Assign) => Infix::Assign,
                kind => bail!(error(
                    &lexer.input,
                    &lexer.tokens[lexer.index],
                    format!("Expected operand got {kind}"),
                )),
            };

            let (l_bp, r_bp) = infix.binding_power();
            if l_bp < min_bp {
                break;
            }

            lexer.next();
            let rhs = Self::expr(lexer, r_bp)?;

            lhs = match infix {
                Infix::Add => Node::Add(Box::new(lhs), Box::new(rhs)),
                Infix::Sub => Node::Sub(Box::new(lhs), Box::new(rhs)),
                Infix::Mul => Node::Mul(Box::new(lhs), Box::new(rhs)),
                Infix::Div => Node::Div(Box::new(lhs), Box::new(rhs)),
                Infix::And => Node::And(Box::new(lhs), Box::new(rhs)),
                Infix::Or => Node::Or(Box::new(lhs), Box::new(rhs)),
                Infix::Xor => Node::Xor(Box::new(lhs), Box::new(rhs)),
                Infix::Eq => Node::Eq(Box::new(lhs), Box::new(rhs)),
                Infix::Ne => Node::Ne(Box::new(lhs), Box::new(rhs)),
                Infix::Lt => Node::Lt(Box::new(lhs), Box::new(rhs)),
                Infix::Lte => Node::Lte(Box::new(lhs), Box::new(rhs)),
                Infix::Gt => Node::Gt(Box::new(lhs), Box::new(rhs)),
                Infix::Gte => Node::Gte(Box::new(lhs), Box::new(rhs)),
                Infix::Assign => {
                    let name = match lhs {
                        Node::Var(name) => name,
                        _ => bail!("Expected variable"),
                    };
                    Node::Assign(name, Box::new(rhs))
                }
            };
        }

        Ok(lhs)
    }
}

impl fmt::Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "---")?;
        let node_count = self.root.fmt_ident(f, 0)?;
        writeln!(f, "---")?;
        write!(f, "{node_count} nodes")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arith_op() {
        let lexer = Lexer::lex("1 / 2 + 2 * 3").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
+
  /
    1
    2
  *
    2
    3
"#
            .trim()
        );
    }

    #[test]
    fn test_negate() {
        let lexer = Lexer::lex("-1 + -2").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
+
  -
    1
  -
    2
"#
            .trim()
        );
    }

    #[test]
    fn test_logic_op() {
        let lexer = Lexer::lex("not true and not false").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
and
  not
    true
  not
    false
    "#
            .trim()
        );
    }

    #[test]
    fn test_rel_op() {
        let lexer = Lexer::lex("1 < 2 and 2 >= 3").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
and
  <
    1
    2
  >=
    2
    3
"#
            .trim()
        );
    }

    #[test]
    fn test_paren() {
        let lexer = Lexer::lex("(1 + 2) * ((4 / 2))").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
*
  +
    1
    2
  /
    4
    2
"#
            .trim()
        );
    }

    #[test]
    fn test_assign() {
        let lexer = Lexer::lex("a = 1 + 2").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
a=
  +
    1
    2
"#
            .trim()
        );
    }

    #[test]
    fn test_block() {
        let lexer = Lexer::lex("2 * {1 + 2}").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        let result = format!("{:?}", parser.root);
        print!("{result}");
        assert_eq!(
            result.trim(),
            r#"
*
  2
  {}
    +
      1
      2
"#
            .trim()
        );
    }
}
