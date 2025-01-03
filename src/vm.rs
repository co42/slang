use core::fmt;

use anyhow::Context;

use crate::compiler::Compiler;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
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
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    // Logical operators
    And,
    Or,
    Xor,
    Not,
    // Relational operators
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
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

    #[allow(unused)]
    pub fn negate(mut self) -> Self {
        self.ins.push(Ins::Negate);
        self
    }

    #[allow(unused)]
    pub fn and(mut self) -> Self {
        self.ins.push(Ins::And);
        self
    }

    #[allow(unused)]
    pub fn or(mut self) -> Self {
        self.ins.push(Ins::Or);
        self
    }

    #[allow(unused)]
    pub fn xor(mut self) -> Self {
        self.ins.push(Ins::Xor);
        self
    }

    #[allow(unused)]
    pub fn not(mut self) -> Self {
        self.ins.push(Ins::Not);
        self
    }

    #[allow(unused)]
    pub fn eq(mut self) -> Self {
        self.ins.push(Ins::Eq);
        self
    }

    #[allow(unused)]
    pub fn ne(mut self) -> Self {
        self.ins.push(Ins::Ne);
        self
    }

    #[allow(unused)]
    pub fn lt(mut self) -> Self {
        self.ins.push(Ins::Lt);
        self
    }

    #[allow(unused)]
    pub fn lte(mut self) -> Self {
        self.ins.push(Ins::Lte);
        self
    }

    #[allow(unused)]
    pub fn gt(mut self) -> Self {
        self.ins.push(Ins::Gt);
        self
    }

    #[allow(unused)]
    pub fn gte(mut self) -> Self {
        self.ins.push(Ins::Gte);
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
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs + rhs as f64),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '+'")),
                    };
                    self.stack.push(result);
                }
                Ins::Sub => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 - rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs - rhs as f64),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '-'")),
                    };
                    self.stack.push(result);
                }
                Ins::Mul => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 * rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs * rhs as f64),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '*'")),
                    };
                    self.stack.push(result);
                }
                Ins::Div => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 / rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs / rhs as f64),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '/'")),
                    };
                    self.stack.push(result);
                }
                Ins::Negate => {
                    let value = self.stack.pop().context("Invalid stack pop")?;
                    let result = match value {
                        Value::Int(value) => Value::Int(-value),
                        Value::Float(value) => Value::Float(-value),
                        _ => return Err(anyhow::anyhow!("Invalid operands for '-'")),
                    };
                    self.stack.push(result);
                }
                Ins::And => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs && rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for 'and'")),
                    };
                    self.stack.push(result);
                }
                Ins::Or => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs || rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for 'or'")),
                    };
                    self.stack.push(result);
                }
                Ins::Xor => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs ^ rhs),
                        _ => return Err(anyhow::anyhow!("Invalid operands for 'xor'")),
                    };
                    self.stack.push(result);
                }
                Ins::Not => {
                    let value = self.stack.pop().context("Invalid stack pop")?;
                    let result = match value {
                        Value::Bool(value) => Value::Bool(!value),
                        _ => return Err(anyhow::anyhow!("Invalid operands for 'not'")),
                    };
                    self.stack.push(result);
                }
                Ins::Eq => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = Value::Bool(lhs == rhs);
                    self.stack.push(result);
                }
                Ins::Ne => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = Value::Bool(lhs != rhs);
                    self.stack.push(result);
                }
                Ins::Lt => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = Value::Bool(lhs < rhs);
                    self.stack.push(result);
                }
                Ins::Lte => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = Value::Bool(lhs <= rhs);
                    self.stack.push(result);
                }
                Ins::Gt => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = Value::Bool(lhs > rhs);
                    self.stack.push(result);
                }
                Ins::Gte => {
                    let rhs = self.stack.pop().context("Invalid stack pop")?;
                    let lhs = self.stack.pop().context("Invalid stack pop")?;
                    let result = Value::Bool(lhs >= rhs);
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
        let program = Program::new().push(42).exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }

    #[test]
    fn test_add() {
        let program = Program::new().push(40).push(2).add().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }

    #[test]
    fn test_sub() {
        let program = Program::new().push(44).push(2).sub().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }

    #[test]
    fn test_mul() {
        let program = Program::new().push(21).push(2).mul().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }

    #[test]
    fn test_div() {
        let program = Program::new().push(84).push(2).div().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(42));
    }

    #[test]
    fn test_negate() {
        let program = Program::new().push(42).negate().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Int(-42));
    }

    #[test]
    fn test_and() {
        let program = Program::new().push(true).push(true).and().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(true).push(false).and().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_or() {
        let program = Program::new().push(true).push(false).or().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(false).push(false).or().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_xor() {
        let program = Program::new().push(true).push(false).xor().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(true).push(true).xor().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_not() {
        let program = Program::new().push(true).not().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));

        let program = Program::new().push(false).not().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));
    }

    #[test]
    fn test_eq() {
        let program = Program::new().push(42).push(42).eq().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(42).push(43).eq().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_ne() {
        let program = Program::new().push(42).push(42).ne().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));

        let program = Program::new().push(42).push(43).ne().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));
    }

    #[test]
    fn test_lt() {
        let program = Program::new().push(41).push(42).lt().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(42).push(42).lt().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_lte() {
        let program = Program::new().push(41).push(42).lte().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(42).push(42).lte().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(43).push(42).lte().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_gt() {
        let program = Program::new().push(43).push(42).gt().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(42).push(42).gt().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }

    #[test]
    fn test_gte() {
        let program = Program::new().push(43).push(42).gte().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(42).push(42).gte().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(true));

        let program = Program::new().push(41).push(42).gte().exit();
        let exit_value = Vm::new(Compiler::new(program)).interpret();
        assert_eq!(exit_value.unwrap(), Bool(false));
    }
}
