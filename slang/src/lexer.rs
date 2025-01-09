use std::fmt;

use anyhow::bail;

use crate::error::source_error;

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
            Self::Null => write!(f, "null"),
            Self::Bool(value) => write!(f, "{}", value),
            Self::Int(value) => write!(f, "{}", value),
            Self::Float(value) => write!(f, "{}", value),
        }
    }
}

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
    Symbol(Symbol),
    Keyword(Keyword),
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{value}"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Symbol(symbol) => write!(f, "{symbol}"),
            Self::Keyword(keyword) => write!(f, "{keyword}"),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Symbol {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Assign,
    Comma,
    Semi,
    Dot,
    Pipe,
    Arrow,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            Self::OpenBrace => write!(f, "{{"),
            Self::CloseBrace => write!(f, "}}"),
            Self::OpenBracket => write!(f, "["),
            Self::CloseBracket => write!(f, "]"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Lte => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
            Self::Assign => write!(f, "="),
            Self::Comma => write!(f, ","),
            Self::Semi => write!(f, ";"),
            Self::Dot => write!(f, "."),
            Self::Pipe => write!(f, "|"),
            Self::Arrow => write!(f, "->"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    And,
    Or,
    Xor,
    Not,
    If,
    Else,
    While,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Not => write!(f, "not"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
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
                        // Keyword
                        ['a', 'n', 'd'] => Token::Keyword(Keyword::And),
                        ['o', 'r'] => Token::Keyword(Keyword::Or),
                        ['x', 'o', 'r'] => Token::Keyword(Keyword::Xor),
                        ['n', 'o', 't'] => Token::Keyword(Keyword::Not),
                        ['i', 'f'] => Token::Keyword(Keyword::If),
                        ['e', 'l', 's', 'e'] => Token::Keyword(Keyword::Else),
                        ['w', 'h', 'i', 'l', 'e'] => Token::Keyword(Keyword::While),
                        // Identifier
                        ident => Token::Ident(ident.iter().collect()),
                    }
                },
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
                },
                '(' => Token::Symbol(Symbol::OpenParen),
                ')' => Token::Symbol(Symbol::CloseParen),
                '{' => Token::Symbol(Symbol::OpenBrace),
                '}' => Token::Symbol(Symbol::CloseBrace),
                '[' => Token::Symbol(Symbol::OpenBracket),
                ']' => Token::Symbol(Symbol::CloseBracket),
                '+' => Token::Symbol(Symbol::Add),
                '-' => {
                    if input.peek() == Some('>') {
                        input.next();
                        Token::Symbol(Symbol::Arrow)
                    } else {
                        Token::Symbol(Symbol::Sub)
                    }
                },
                '*' => Token::Symbol(Symbol::Mul),
                '/' => Token::Symbol(Symbol::Div),
                '%' => Token::Symbol(Symbol::Mod),
                '=' => {
                    if input.peek() == Some('=') {
                        input.next();
                        Token::Symbol(Symbol::Eq)
                    } else {
                        Token::Symbol(Symbol::Assign)
                    }
                },
                '!' => {
                    if input.next() != Some('=') {
                        bail!(error(&input_raw, start));
                    }
                    Token::Symbol(Symbol::Ne)
                },
                '<' => {
                    if input.peek() == Some('=') {
                        input.next();
                        Token::Symbol(Symbol::Lte)
                    } else {
                        Token::Symbol(Symbol::Lt)
                    }
                },
                '>' => {
                    if input.peek() == Some('=') {
                        input.next();
                        Token::Symbol(Symbol::Gte)
                    } else {
                        Token::Symbol(Symbol::Gt)
                    }
                },
                ',' => Token::Symbol(Symbol::Comma),
                ';' => Token::Symbol(Symbol::Semi),
                '.' => Token::Symbol(Symbol::Dot),
                '|' => Token::Symbol(Symbol::Pipe),
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
            },
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
    use super::Keyword::*;
    use super::Symbol::*;
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
    fn test_arith_symbol() {
        test("+ - * / %", &[Symbol(Add), Symbol(Sub), Symbol(Mul), Symbol(Div), Symbol(Mod)]);
    }

    #[test]
    fn test_rel_symbol() {
        test("== != < <= > >=", &[Symbol(Eq), Symbol(Ne), Symbol(Lt), Symbol(Lte), Symbol(Gt), Symbol(Gte)]);
    }

    #[test]
    fn test_other_symbol() {
        test("; . | ->", &[Symbol(Semi), Symbol(Dot), Symbol(Pipe), Symbol(Arrow)]);
    }

    #[test]
    fn test_logic_op() {
        test("and or xor not", &[Keyword(And), Keyword(Or), Keyword(Xor), Keyword(Not)]);
    }

    #[test]
    fn test_assign() {
        test("foo_42 = 1", &[Ident("foo_42".to_owned()), Symbol(Assign), Value(Int(1))]);
    }

    #[test]
    fn test_paren() {
        test(
            "40 + (1*1)",
            &[
                Value(Int(40)),
                Symbol(Add),
                Symbol(OpenParen),
                Value(Int(1)),
                Symbol(Mul),
                Value(Int(1)),
                Symbol(CloseParen),
            ],
        );
    }

    #[test]
    fn test_brace() {
        test("{1}", &[Symbol(OpenBrace), Value(Int(1)), Symbol(CloseBrace)]);
    }

    #[test]
    fn test_bracket() {
        test("[1]", &[Symbol(OpenBracket), Value(Int(1)), Symbol(CloseBracket)]);
    }

    fn test(input: &str, tokens_kind: &[Token]) {
        let mut tokens = tokens_kind.to_vec();
        tokens.push(Eof);
        let lexer = Lexer::lex(input).unwrap();
        assert_eq!(lexer.tokens.into_iter().map(|t| t.token).collect::<Vec<_>>(), tokens,);
    }
}
