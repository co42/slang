use std::fmt;

use crate::parser::{self, Node, Parser};
use crate::vm::{Ins, Program, Value};

struct Context {
    variables: Vec<String>,
    frames: Vec<usize>,
}

impl Context {
    fn new() -> Self {
        Self {
            variables: Vec::new(),
            frames: vec![0],
        }
    }

    fn enter(&mut self) {
        self.frames.push(self.variables.len());
    }

    fn leave(&mut self) {
        let frame = self.frames.pop().unwrap();
        self.variables.truncate(frame);
    }

    fn decl(&mut self, ident: &str) -> u32 {
        self.variables.push(ident.to_string());
        self.variables.len() as u32
    }

    fn get(&self, ident: &str) -> Option<u32> {
        self.variables
            .iter()
            .position(|var| var == ident)
            .map(|index| index as u32)
    }
}

pub struct Compiler {
    pub program: Program,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        Self { program }
    }

    pub fn compile(parser: Parser) -> anyhow::Result<Self> {
        let mut program = Program::new();
        let mut context = Context::new();
        Self::expr(&mut program, &mut context, &parser.root);
        program.ins.push(Ins::Exit);
        Ok(Self::new(program))
    }

    fn expr(program: &mut Program, context: &mut Context, node: &Node) {
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
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Add);
            }
            Node::Sub(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Sub);
            }
            Node::Mul(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Mul);
            }
            Node::Div(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Div);
            }
            Node::And(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::And);
            }
            Node::Or(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Or);
            }
            Node::Xor(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Xor);
            }
            Node::Negate(value) => {
                Self::expr(program, context, value);
                program.ins.push(Ins::Negate);
            }
            Node::Not(value) => {
                Self::expr(program, context, value);
                program.ins.push(Ins::Not);
            }
            Node::Eq(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Eq);
            }
            Node::Ne(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Ne);
            }
            Node::Lt(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Lt);
            }
            Node::Lte(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Lte);
            }
            Node::Gt(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Gt);
            }
            Node::Gte(lhs, rhs) => {
                Self::expr(program, context, lhs);
                Self::expr(program, context, rhs);
                program.ins.push(Ins::Gte);
            }
            Node::Assign(ident, node) => {
                Self::expr(program, context, node);
                let addr = context.get(ident);
                if addr.is_none() {
                    context.decl(ident);
                }
                program.ins.push(Ins::Store(addr));
            }
            Node::Var(ident) => {
                let addr = context.get(ident).expect("Variable not found");
                program.ins.push(Ins::Load(addr));
            }
            Node::Block(nodes) => {
                context.enter();
                program.ins.push(Ins::Enter);
                for node in nodes {
                    Self::expr(program, context, node);
                }
                program.ins.push(Ins::Leave);
                context.leave();
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
