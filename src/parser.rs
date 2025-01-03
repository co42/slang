use std::fmt;

use anyhow::bail;

use crate::lexer::{Lexer, Op, Token};

#[derive(Clone, PartialEq)]
pub enum Node {
    Int(i64),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
}

impl Node {
    fn fmt_ident(&self, f: &mut fmt::Formatter<'_>, ident: usize) -> Result<usize, fmt::Error> {
        match self {
            Node::Int(value) => {
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
        }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_ident(f, 0)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
}

impl Infix {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Infix::Add | Infix::Sub => (1, 2),
            Infix::Mul | Infix::Div => (3, 4),
        }
    }
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
            Token::Int(value) => Node::Int(*value),
            Token::OpenParen => {
                let lhs = Self::expr(lexer, 0)?;
                let token = lexer.next();
                if token != &Token::CloseParen {
                    bail!("Expected ')' got {token}");
                }
                lhs
            }
            token => bail!("Expected integer got {token}"),
        };

        loop {
            let infix = match lexer.peek() {
                Token::Eof | Token::CloseParen => break,
                Token::Op(Op::Add) => Infix::Add,
                Token::Op(Op::Sub) => Infix::Sub,
                Token::Op(Op::Mul) => Infix::Mul,
                Token::Op(Op::Div) => Infix::Div,
                token => bail!("Expected operand got {token:?}"),
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
    fn test_op() {
        let lexer = Lexer::lex("1 + 2 * 3").unwrap();
        let parser = Parser::parse(lexer).unwrap();
        assert_eq!(
            format!("{:?}", parser.root).trim(),
            r#"
+
  1
  *
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
        assert_eq!(
            format!("{:?}", parser.root).trim(),
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
}
