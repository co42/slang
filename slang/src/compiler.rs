use std::rc::Rc;

use anyhow::Context as _;
use derive_more::Deref;
use stackz::vm::{self, Ins, Program};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Func(Func),
    Gen(Gen),
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Null
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<Func> for Value {
    fn from(value: Func) -> Self {
        Value::Func(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    pub args: Vec<String>,
    pub body: Box<Expr>,
}

#[derive(Clone, Debug, Deref, PartialEq)]
pub struct Gen(Func);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Value(Value),
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Block {
        exprs: Vec<Expr>,
    },
    IfElse {
        cond: Box<Expr>,
        if_body: Box<Expr>,
        else_body: Box<Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Assign {
        ident: String,
        expr: Box<Expr>,
    },
    Var {
        ident: String,
    },
    Return(Box<Expr>),
    Yield(Box<Expr>),
    Next(Box<Expr>),
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

impl Expr {
    pub fn compile(&self, context: &mut Context, program: &mut Program) -> anyhow::Result<()> {
        match self {
            Self::Value(value) => {
                let vm_value = match value {
                    Value::Null => vm::Value::Null,
                    Value::Bool(value) => vm::Value::Bool(*value),
                    Value::Int(value) => vm::Value::Int(*value),
                    Value::Float(value) => vm::Value::Float(*value),
                    Value::Func(func) => {
                        let mut program = Program::new();
                        context.enter();
                        for arg in &func.args {
                            context.decl(arg);
                        }
                        func.body.compile(context, &mut program)?;
                        context.leave();
                        vm::Value::Func(vm::Func::new(func.args.len() as u32, Rc::new(program)))
                    },
                    Value::Gen(gen) => {
                        let mut program = Program::new();
                        context.enter();
                        for arg in &gen.args {
                            context.decl(arg);
                        }
                        gen.body.compile(context, &mut program)?;
                        context.leave();
                        vm::Value::Gen(vm::Gen(vm::Func::new(gen.args.len() as u32, Rc::new(program))))
                    },
                };
                program.push(Ins::Push(vm_value));
            },
            Self::UnaryOp { op, operand } => {
                operand.compile(context, program)?;
                program.push(match op {
                    UnaryOp::Negate => Ins::Negate,
                    UnaryOp::Not => Ins::Not,
                });
            },
            Self::BinaryOp { op, lhs, rhs } => {
                lhs.compile(context, program)?;
                rhs.compile(context, program)?;
                program.push(match op {
                    BinaryOp::Add => Ins::Add,
                    BinaryOp::Sub => Ins::Sub,
                    BinaryOp::Mul => Ins::Mul,
                    BinaryOp::Div => Ins::Div,
                    BinaryOp::Mod => Ins::Mod,
                    BinaryOp::And => Ins::And,
                    BinaryOp::Or => Ins::Or,
                    BinaryOp::Xor => Ins::Xor,
                    BinaryOp::Eq => Ins::Eq,
                    BinaryOp::Ne => Ins::Ne,
                    BinaryOp::Lt => Ins::Lt,
                    BinaryOp::Lte => Ins::Lte,
                    BinaryOp::Gt => Ins::Gt,
                    BinaryOp::Gte => Ins::Gte,
                });
            },
            Self::Block { exprs } => {
                context.enter();
                program.push(Ins::Enter);
                for expr in exprs {
                    expr.compile(context, program)?;
                }
                program.push(Ins::Leave);
                context.leave();
            },
            Self::IfElse {
                cond,
                if_body,
                else_body,
            } => {
                // Condition
                cond.compile(context, program)?;

                // Jump to else if condition is false
                program.push(Ins::Not);
                let jmp_else = program.len();
                program.push(Ins::JmpIf(0));

                // If
                if_body.compile(context, program)?;
                // Jump to end
                let jmp_end = program.len();
                program.push(Ins::JmpIf(0));

                // Else
                let else_index = program.len();
                else_body.compile(context, program)?;

                // Patch jumps
                program[jmp_else] = Ins::JmpIf(else_index as u32);
                program[jmp_end] = Ins::Jmp(program.len() as u32);
            },
            Self::While { cond, body } => {
                let start = program.len();
                // Condition
                cond.compile(context, program)?;

                // Jump to end if condition is false
                program.push(Ins::Not);
                let jmp_end = program.len();
                program.push(Ins::JmpIf(0));

                // Body
                body.compile(context, program)?;

                // Jump to start
                program.push(Ins::Jmp(start as u32));

                // Patch jump
                program[jmp_end] = Ins::JmpIf(program.len() as u32);
            },
            Self::Assign { ident, expr } => {
                expr.compile(context, program)?;
                if let Some(index) = context.get(ident) {
                    program.push(Ins::Store(Some(index)));
                } else {
                    context.decl(ident);
                    program.push(Ins::Store(None));
                }
            },
            Self::Var { ident } => {
                let index = context.get(ident).context("Variable not found")?;
                program.push(Ins::Load(index));
            },
            Self::Return(expr) => {
                expr.compile(context, program)?;
                program.push(Ins::Return);
            },
            Self::Yield(expr) => {
                expr.compile(context, program)?;
                program.push(Ins::Continue);
            },
            Self::Next(expr) => {
                expr.compile(context, program)?;
                program.push(Ins::Next);
            },
            Self::Call { func, args } => {
                for arg in args {
                    arg.compile(context, program)?;
                }
                func.compile(context, program)?;
                program.push(Ins::Call);
            },
        }
        Ok(())
    }
}

pub struct Context {
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
        println!("enter");
        self.frames.push(self.variables.len());
    }

    fn leave(&mut self) {
        println!("leave");
        let frame = self.frames.pop().unwrap();
        self.variables.truncate(frame);
    }

    fn decl(&mut self, ident: &str) {
        println!("decl {ident}");
        self.variables.push(ident.to_string());
    }

    fn get(&self, ident: &str) -> Option<u32> {
        println!("get {ident}");
        self.variables
            .iter()
            .rev()
            .position(|var| var == ident)
            .map(|index| index as u32)
    }
}

pub struct Compiler;

impl Compiler {
    pub fn compile(root: &Expr) -> anyhow::Result<vm::Func> {
        let mut context = Context::new();
        let mut program = Program::new();
        root.compile(&mut context, &mut program)?;
        // TODO: Fix this
        program.push(Ins::Return);
        Ok(vm::Func::new(0, Rc::new(program)))
    }
}

#[allow(unused)]
pub mod expr {
    use std::fmt::Display;

    use super::*;

    pub fn value(value: impl Into<Value>) -> Expr {
        Expr::Value(value.into())
    }

    pub fn func<const N: usize>(args: [impl Display; N], body: Expr) -> Expr {
        Expr::Value(Value::Func(Func {
            args: args.map(|arg| arg.to_string()).to_vec(),
            body: Box::new(body),
        }))
    }

    pub fn gen<const N: usize>(args: [impl Display; N], body: Expr) -> Expr {
        Expr::Value(Value::Gen(Gen(Func {
            args: args.map(|arg| arg.to_string()).to_vec(),
            body: Box::new(body),
        })))
    }

    pub fn uop(op: UnaryOp, operand: Expr) -> Expr {
        Expr::UnaryOp {
            op,
            operand: Box::new(operand),
        }
    }

    pub fn bop(op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn block<const N: usize>(exprs: [Expr; N]) -> Expr {
        Expr::Block { exprs: exprs.to_vec() }
    }

    pub fn if_else(cond: Expr, cond_if: Expr, cond_else: Expr) -> Expr {
        Expr::IfElse {
            cond: Box::new(cond),
            if_body: Box::new(cond_if),
            else_body: Box::new(cond_else),
        }
    }

    pub fn while_(cond: Expr, body: Expr) -> Expr {
        Expr::While {
            cond: Box::new(cond),
            body: Box::new(body),
        }
    }

    pub fn assign(ident: &str, expr: Expr) -> Expr {
        Expr::Assign {
            ident: ident.to_string(),
            expr: Box::new(expr),
        }
    }

    pub fn var(ident: &str) -> Expr {
        Expr::Var {
            ident: ident.to_string(),
        }
    }

    pub fn ret(expr: Expr) -> Expr {
        Expr::Return(Box::new(expr))
    }

    pub fn yield_(expr: Expr) -> Expr {
        Expr::Yield(Box::new(expr))
    }

    pub fn next(expr: Expr) -> Expr {
        Expr::Next(Box::new(expr))
    }

    pub fn call<const N: usize>(func: Expr, args: [Expr; N]) -> Expr {
        Expr::Call {
            func: Box::new(func),
            args: args.to_vec(),
        }
    }
}

#[cfg(test)]
mod tests {
    use stackz::vm::{Flow, Task};

    use super::expr::*;
    use super::BinaryOp::*;
    use super::UnaryOp::*;
    use super::*;

    fn test(root: Expr, expected: impl Into<vm::Value>) {
        let func = Compiler::compile(&root).expect("Compile error");
        println!("{:?}", func.program);
        let exit_value = Task::new(func).poll().expect("Task poll error");
        assert_eq!(exit_value, Flow::Break(expected.into()));
    }

    #[test]
    fn test_value() {
        test(value(42), 42);
    }

    #[test]
    fn test_bop() {
        test(bop(Add, value(40), value(2)), 42);
        test(bop(Sub, value(44), value(2)), 42);
        test(bop(Mul, value(21), value(2)), 42);
        test(bop(Div, value(84), value(2)), 42);
        test(bop(Mod, value(84), value(42)), 0);
        test(bop(And, value(true), value(true)), true);
        test(bop(Or, value(false), value(true)), true);
        test(bop(Xor, value(true), value(true)), false);
        test(bop(Eq, value(42), value(42)), true);
        test(bop(Ne, value(42), value(42)), false);
        test(bop(Lt, value(41), value(42)), true);
        test(bop(Lte, value(42), value(42)), true);
        test(bop(Gt, value(43), value(42)), true);
        test(bop(Gte, value(42), value(42)), true);
    }

    #[test]
    fn test_uop() {
        test(uop(Negate, value(-42)), 42);
        test(uop(Not, value(false)), true);
    }

    #[test]
    fn test_block() {
        test(block([value(42)]), 42);
        test(
            block([
                assign("x", value(20)),
                bop(Add, block([assign("y", value(2)), bop(Add, var("x"), var("y"))]), var("x")),
            ]),
            42,
        );
    }

    #[test]
    fn test_if_else() {
        test(if_else(value(true), value(42), value(0)), 42);
        test(if_else(value(false), value(42), value(0)), 0);
    }

    #[test]
    fn test_while() {
        test(
            block([
                assign("x", value(0)),
                assign("y", value(6)),
                assign("z", value(1)),
                while_(
                    bop(Lt, var("x"), var("y")),
                    block([
                        assign("z", bop(Mul, var("z"), value(2))),
                        assign("x", bop(Add, var("x"), value(1))),
                    ]),
                ),
                var("z"),
            ]),
            64,
        );
    }

    #[test]
    fn test_assign() {
        test(
            block([
                assign("x", value(40)),
                assign("y", value(2)),
                bop(Add, var("x"), var("y")),
            ]),
            42,
        );
    }

    #[test]
    fn test_call() {
        test(
            block([
                assign("add", func(["x", "y"], ret(bop(Add, var("x"), var("y"))))),
                call(var("add"), [value(40), value(2)]),
            ]),
            42,
        );
    }

    #[test]
    fn test_generator() {
        test(
            block([
                assign(
                    "range",
                    gen(
                        ["start", "end"],
                        block([while_(
                            bop(Lt, var("start"), var("end")),
                            block([yield_(var("start")), assign("start", bop(Add, var("start"), value(1)))]),
                        )]),
                    ),
                ),
                assign("r", call(var("range"), [value(2), value(10)])),
                bop(Mul, next(var("r")), next(var("r"))),
            ]),
            6,
        )
    }
}
