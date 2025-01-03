use std::fmt;

use anyhow::bail;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Int(i64),
    Op(Op),
    OpenParen,
    CloseParen,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Int(value) => write!(f, "{value}"),
            Token::Op(op) => write!(f, "{op}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone)]
pub struct Lexer {
    pub tokens: Vec<Token>,
    pub index: usize,
}

impl Lexer {
    pub fn lex(input: &str) -> anyhow::Result<Self> {
        let mut input = input.chars().peekable();
        let mut tokens = Vec::new();
        while let Some(c) = input.next() {
            let token = match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '0'..='9' => {
                    let mut value = c as i64 - '0' as i64;
                    while let Some(&c) = input.peek() {
                        if c.is_ascii_digit() {
                            value = value * 10 + c as i64 - '0' as i64;
                            input.next();
                        } else {
                            break;
                        }
                    }
                    Token::Int(value)
                }
                '+' => Token::Op(Op::Add),
                '-' => Token::Op(Op::Sub),
                '*' => Token::Op(Op::Mul),
                '/' => Token::Op(Op::Div),
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                _ => bail!("Invalid character: {c}"),
            };
            tokens.push(token);
        }

        Ok(Self { tokens, index: 0 })
    }

    pub fn next(&mut self) -> &Token {
        match self.tokens.get(self.index) {
            Some(token) => {
                self.index += 1;
                token
            }
            None => &Token::Eof,
        }
    }

    pub fn peek(&self) -> &Token {
        match self.tokens.get(self.index) {
            Some(token) => token,
            None => &Token::Eof,
        }
    }
}

impl fmt::Debug for Lexer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "---")?;
        for token in &self.tokens {
            writeln!(f, "{token:?}")?;
        }
        writeln!(f, "---")?;
        write!(f, "{} tokens", self.tokens.len())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Op::*;
    use super::Token::*;
    use super::*;

    #[test]
    fn test_op() {
        let input = "40 + 2";
        let lexer = Lexer::lex(input).unwrap();
        assert_eq!(lexer.tokens, vec![Int(40), Op(Add), Int(2)]);
    }

    #[test]
    fn test_paren() {
        let input = "40 + (1*1)";
        let lexer = Lexer::lex(input).unwrap();
        assert_eq!(
            lexer.tokens,
            vec![
                Int(40),
                Op(Add),
                OpenParen,
                Int(1),
                Op(Mul),
                Int(1),
                CloseParen
            ],
        );
    }
}
