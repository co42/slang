use std::fmt;

use anyhow::bail;

use crate::error::source_error;

#[derive(Clone, Debug, PartialEq)]
pub struct TokenLoc {
    pub token: Token,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Value(Value),
    Ident(String),
    Op(Op),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Value(value) => write!(f, "{value}"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Op(op) => write!(f, "{op}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Eof => write!(f, "EOF"),
        }
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
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Assign,
    Dot,
    Arrow,
    Semi,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Xor => write!(f, "xor"),
            Op::Not => write!(f, "not"),
            Op::Eq => write!(f, "=="),
            Op::Ne => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Lte => write!(f, "<="),
            Op::Gt => write!(f, ">"),
            Op::Gte => write!(f, ">="),
            Op::Assign => write!(f, "="),
            Op::Dot => write!(f, "."),
            Op::Arrow => write!(f, "->"),
            Op::Semi => write!(f, ";"),
        }
    }
}

#[derive(Clone)]
struct Input {
    chars: Vec<char>,
    index: usize,
}

impl Input {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.chars.get(self.index).copied();
        self.index += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }
}

fn error(input_raw: &str, start: usize) -> String {
    source_error(input_raw, start, start, "Unexpected character")
}

#[derive(Clone)]
pub struct Lexer {
    pub input: String,
    pub tokens: Vec<TokenLoc>,
    pub index: usize,
}

impl Lexer {
    pub fn lex(input_raw: impl ToString) -> anyhow::Result<Self> {
        let input_raw = input_raw.to_string();
        let mut input = Input::new(&input_raw);
        let mut tokens = Vec::new();
        while let Some(c) = input.next() {
            let start = input.index - 1;
            let kind = match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                'a'..='z' | 'A'..='Z' => {
                    while let Some(c) = input.peek() {
                        if c.is_ascii_alphanumeric() || c == '_' {
                            input.next();
                        } else {
                            break;
                        }
                    }
                    match &input.chars[start..input.index] {
                        // Value
                        ['n', 'u', 'l', 'l'] => Token::Value(Value::Null),
                        ['f', 'a', 'l', 's', 'e'] => Token::Value(Value::Bool(false)),
                        ['t', 'r', 'u', 'e'] => Token::Value(Value::Bool(true)),
                        // Operator
                        ['a', 'd', 'd'] => Token::Op(Op::Add),
                        ['s', 'u', 'b'] => Token::Op(Op::Sub),
                        ['m', 'u', 'l'] => Token::Op(Op::Mul),
                        ['d', 'i', 'v'] => Token::Op(Op::Div),
                        ['a', 'n', 'd'] => Token::Op(Op::And),
                        ['o', 'r'] => Token::Op(Op::Or),
                        ['x', 'o', 'r'] => Token::Op(Op::Xor),
                        ['n', 'o', 't'] => Token::Op(Op::Not),
                        ['e', 'q'] => Token::Op(Op::Eq),
                        ['n', 'e'] => Token::Op(Op::Ne),
                        ['l', 't'] => Token::Op(Op::Lt),
                        ['l', 't', 'e'] => Token::Op(Op::Lte),
                        ['g', 't'] => Token::Op(Op::Gt),
                        ['g', 't', 'e'] => Token::Op(Op::Gte),
                        // Identifier
                        ident => Token::Ident(ident.iter().collect()),
                    }
                }
                '0'..='9' => {
                    let mut value = c as i64 - '0' as i64;
                    while let Some(c) = input.peek() {
                        if c.is_ascii_digit() {
                            value = value * 10 + c as i64 - '0' as i64;
                            input.next();
                        } else {
                            break;
                        }
                    }
                    if let Some('.') = input.peek() {
                        input.next();
                        let mut value = value as f64;
                        let mut fraction = 0.1;
                        while let Some(c) = input.peek() {
                            if c.is_ascii_digit() {
                                value += fraction * (c as i64 - '0' as i64) as f64;
                                fraction *= 0.1;
                                input.next();
                            } else {
                                break;
                            }
                        }
                        Token::Value(Value::Float(value))
                    } else {
                        Token::Value(Value::Int(value))
                    }
                }
                '+' => Token::Op(Op::Add),
                '-' => {
                    if input.peek() == Some('>') {
                        input.next();
                        Token::Op(Op::Arrow)
                    } else {
                        Token::Op(Op::Sub)
                    }
                }
                '*' => Token::Op(Op::Mul),
                '/' => Token::Op(Op::Div),
                '=' => {
                    if input.peek() == Some('=') {
                        input.next();
                        Token::Op(Op::Eq)
                    } else {
                        Token::Op(Op::Assign)
                    }
                }
                '!' => {
                    if input.next() != Some('=') {
                        bail!(error(&input_raw, start));
                    }
                    Token::Op(Op::Ne)
                }
                '<' => {
                    if input.peek() == Some('=') {
                        input.next();
                        Token::Op(Op::Lte)
                    } else {
                        Token::Op(Op::Lt)
                    }
                }
                '>' => {
                    if input.peek() == Some('=') {
                        input.next();
                        Token::Op(Op::Gte)
                    } else {
                        Token::Op(Op::Gt)
                    }
                }
                '.' => Token::Op(Op::Dot),
                ';' => Token::Op(Op::Semi),
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                _ => bail!(error(&input_raw, start)),
            };
            tokens.push(TokenLoc {
                token: kind,
                start,
                end: input.index,
            });
        }
        tokens.push(TokenLoc {
            token: Token::Eof,
            start: input.chars.len() - 1,
            end: input.chars.len(),
        });

        Ok(Self {
            input: input_raw,
            tokens,
            index: 0,
        })
    }

    pub fn next(&mut self) -> &Token {
        match self.tokens.get(self.index) {
            Some(token) => {
                self.index += 1;
                &token.token
            }
            None => &self.tokens.last().expect("Lexer past EOF").token,
        }
    }

    pub fn peek(&self) -> &Token {
        match self.tokens.get(self.index) {
            Some(token) => &token.token,
            None => &self.tokens.last().expect("Lexer past EOF").token,
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
    use super::Value::*;
    use super::*;

    #[test]
    fn test_error() {
        let input = "1 + &true";
        let err = Lexer::lex(input).unwrap_err();
        let err = err.to_string();
        println!("{err}");
        assert_eq!(
            err,
            r#"
Unexpected character line 1 column 5
1 + &true
    ^"#
            .trim()
        );
    }

    #[test]
    fn test_null() {
        test("null", &[Value(Null)]);
    }

    #[test]
    fn test_false() {
        test("false", &[Value(Bool(false))]);
    }

    #[test]
    fn test_true() {
        test("true", &[Value(Bool(true))]);
    }

    #[test]
    fn test_int() {
        test("42", &[Value(Int(42))]);
    }

    #[test]
    fn test_float() {
        test("42.42", &[Value(Float(42.42))]);
    }

    #[test]
    fn test_arith_op() {
        test("+ - * /", &[Op(Add), Op(Sub), Op(Mul), Op(Div)]);
    }

    #[test]
    fn test_logic_op() {
        test("and or xor not", &[Op(And), Op(Or), Op(Xor), Op(Not)]);
    }

    #[test]
    fn test_rel_op() {
        test(
            "== != < <= > >=",
            &[Op(Eq), Op(Ne), Op(Lt), Op(Lte), Op(Gt), Op(Gte)],
        );
    }

    #[test]
    fn test_assign() {
        test(
            "foo_42 = 1",
            &[Ident("foo_42".to_owned()), Op(Assign), Value(Int(1))],
        );
    }

    #[test]
    fn test_dot() {
        test(".", &[Op(Dot)]);
    }

    #[test]
    fn test_arrow() {
        test("->", &[Op(Arrow)]);
    }

    #[test]
    fn test_paren() {
        test(
            "40 + (1*1)",
            &[
                Value(Int(40)),
                Op(Add),
                OpenParen,
                Value(Int(1)),
                Op(Mul),
                Value(Int(1)),
                CloseParen,
            ],
        );
    }

    #[test]
    fn test_brace() {
        test("{1}", &[OpenBrace, Value(Int(1)), CloseBrace]);
    }

    #[test]
    fn test_bracket() {
        test("[1]", &[OpenBracket, Value(Int(1)), CloseBracket]);
    }

    fn test(input: &str, tokens_kind: &[Token]) {
        let mut tokens = tokens_kind.to_vec();
        tokens.push(Eof);
        let lexer = Lexer::lex(input).unwrap();
        assert_eq!(
            lexer
                .tokens
                .into_iter()
                .map(|t| t.token)
                .collect::<Vec<_>>(),
            tokens,
        );
    }
}
