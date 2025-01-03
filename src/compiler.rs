use std::fmt;

use crate::parser::{self, Node, Parser};
use crate::vm::{Ins, Program, Value};

pub struct Compiler {
    pub program: Program,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        Self { program }
    }

    pub fn compile(parser: Parser) -> anyhow::Result<Self> {
        let mut program = Program::new();
        Self::expr(&mut program, &parser.root);
        program.ins.push(Ins::Exit);
        Ok(Self::new(program))
    }

    fn expr(program: &mut Program, node: &Node) {
        match node {
            Node::Value(parser::Value::Null) => {
                program.ins.push(Ins::Push(Value::Null));
            }
            Node::Value(parser::Value::Bool(value)) => {
                program.ins.push(Ins::Push(Value::Bool(*value)));
            }
            Node::Value(parser::Value::Int(value)) => {
                program.ins.push(Ins::Push(Value::Int(*value)));
            }
            Node::Value(parser::Value::Float(value)) => {
                program.ins.push(Ins::Push(Value::Float(*value)));
            }
            Node::Add(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Add);
            }
            Node::Sub(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Sub);
            }
            Node::Mul(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Mul);
            }
            Node::Div(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Div);
            }
            Node::And(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::And);
            }
            Node::Or(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Or);
            }
            Node::Xor(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Xor);
            }
            Node::Negate(value) => {
                Self::expr(program, value);
                program.ins.push(Ins::Negate);
            }
            Node::Not(value) => {
                Self::expr(program, value);
                program.ins.push(Ins::Not);
            }
            Node::Eq(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Eq);
            }
            Node::Ne(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Ne);
            }
            Node::Lt(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Lt);
            }
            Node::Lte(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Lte);
            }
            Node::Gt(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Gt);
            }
            Node::Gte(lhs, rhs) => {
                Self::expr(program, lhs);
                Self::expr(program, rhs);
                program.ins.push(Ins::Gte);
            }
        }
    }
}

impl fmt::Debug for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.program)?;
        Ok(())
    }
}
