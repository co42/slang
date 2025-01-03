use core::fmt;

use anyhow::Context;

use crate::compiler::Compiler;

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

#[derive(Clone, Debug, PartialEq)]
pub enum Ins {
    // Exit
    Exit,
    // Stack
    Push(Value),
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone)]
pub struct Program {
    pub ins: Vec<Ins>,
}

impl Program {
    pub fn new() -> Self {
        Self { ins: Vec::new() }
    }

    #[allow(unused)]
    pub fn exit(mut self) -> Self {
        self.ins.push(Ins::Exit);
        self
    }

    #[allow(unused)]
    pub fn push(mut self, value: impl Into<Value>) -> Self {
        self.ins.push(Ins::Push(value.into()));
        self
    }

    #[allow(unused)]
    pub fn add(mut self) -> Self {
        self.ins.push(Ins::Add);
        self
    }

    #[allow(unused)]
    pub fn sub(mut self) -> Self {
        self.ins.push(Ins::Sub);
        self
    }

    #[allow(unused)]
    pub fn mul(mut self) -> Self {
        self.ins.push(Ins::Mul);
        self
    }

    #[allow(unused)]
    pub fn div(mut self) -> Self {
        self.ins.push(Ins::Div);
        self
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "---")?;
        for ins in &self.ins {
            writeln!(f, "{ins:?}")?;
        }
        writeln!(f, "---")?;
        write!(f, "{} instructions", self.ins.len())?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Vm {
    program: Program,
    stack: Vec<Value>,
    ip: usize,
}

impl Vm {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            program: compiler.program,
            stack: Vec::new(),
            ip: 0,
        }
    }

    pub fn interpret(&mut self) -> anyhow::Result<Value> {
        loop {
            let ins = self
                .program
                .ins
                .get(self.ip)
                .context("Invalid instruction pointer")?;
            match ins {
                Ins::Exit => {
                    let value = self.stack.pop().context("Invalid stack pop")?;
                    return Ok(value.clone());
                }
                Ins::Push(value) => {
                    self.stack.push(value.clone());
                }
                Ins::Add => {
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '+'")),
                    };
                    self.stack.push(result);
                }
                Ins::Sub => {
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '-'")),
                    };
                    self.stack.push(result);
                }
                Ins::Mul => {
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '*'")),
                    };
                    self.stack.push(result);
                }
                Ins::Div => {
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '/'")),
                    };
                    self.stack.push(result);
                }
            }
            self.ip += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value::*;
    use super::*;

    #[test]
    fn test_exit() {
        let program = Program::new().push(Int(42)).exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }

    #[test]
    fn test_add() {
        let program = Program::new().push(Int(40)).push(Int(2)).add().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }
}
