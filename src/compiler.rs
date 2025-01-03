use std::fmt;

use crate::parser::{Node, Parser};
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
            Node::Int(value) => {
                program.ins.push(Ins::Push(Value::Int(*value)));
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
        }
    }
}

impl fmt::Debug for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.program)?;
        Ok(())
    }
}
