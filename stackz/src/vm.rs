use std::cell::RefCell;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;

use derive_more::derive::{Deref, DerefMut};

use crate::{Error, ErrorExt, Result};

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Func(Func),
    Gen(Gen),
    Task(Rc<RefCell<Task>>),
    Array(Vec<Value>),
}

impl Value {
    pub fn as_null(&self) -> crate::Result<()> {
        match self {
            Value::Null => Ok(()),
            _ => Err(crate::Error::fatal(format!("Expected 'null' got '{self}'"))),
        }
    }

    pub fn as_bool(&self) -> crate::Result<bool> {
        match self {
            Value::Bool(value) => Ok(*value),
            _ => Err(crate::Error::fatal(format!("Expected 'Bool' got '{self}'"))),
        }
    }

    pub fn as_int(&self) -> crate::Result<i64> {
        match self {
            Value::Int(value) => Ok(*value),
            _ => Err(crate::Error::fatal(format!("Expected 'Integer' got '{self}'"))),
        }
    }

    pub fn as_float(&self) -> crate::Result<f64> {
        match self {
            Value::Float(value) => Ok(*value),
            _ => Err(crate::Error::fatal(format!("Expected 'Float' got '{self}'"))),
        }
    }

    pub fn as_str(self) -> crate::Result<String> {
        match self {
            Value::Str(value) => Ok(value),
            _ => Err(crate::Error::fatal(format!("Expected 'String' got '{self}'"))),
        }
    }

    pub fn as_function(self) -> crate::Result<Func> {
        match self {
            Value::Func(value) => Ok(value),
            _ => Err(crate::Error::fatal(format!("Expected 'Function' got '{self}'"))),
        }
    }

    pub fn as_generator(self) -> crate::Result<Gen> {
        match self {
            Value::Gen(value) => Ok(value),
            _ => Err(crate::Error::fatal(format!("Expected 'Generator' got '{self}'"))),
        }
    }

    pub fn as_task(self) -> crate::Result<Rc<RefCell<Task>>> {
        match self {
            Value::Task(value) => Ok(value),
            _ => Err(crate::Error::fatal(format!("Expected 'Task' got '{self}'"))),
        }
    }

    pub fn as_array(self) -> crate::Result<Vec<Value>> {
        match self {
            Value::Array(value) => Ok(value),
            _ => Err(crate::Error::fatal(format!("Expected 'Array' got '{self}'"))),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(value) => write!(f, "{value}"),
            Value::Int(value) => write!(f, "{value}"),
            Value::Float(value) => write!(f, "{value}"),
            Value::Str(value) => write!(f, "{value:?}"),
            Value::Func(value) => write!(f, "{value:?}"),
            Value::Gen(value) => write!(f, "{value:?}"),
            Value::Task(_) => write!(f, "Task"),
            Value::Array(value) => write!(f, "{value:?}"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Array(lhs), Value::Array(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Null, Value::Null) => Some(std::cmp::Ordering::Equal),
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int(lhs), Value::Int(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Str(lhs), Value::Str(rhs)) => lhs.partial_cmp(rhs),
            (Value::Array(lhs), Value::Array(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
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

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Str(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Str(value.to_owned())
    }
}

impl From<Func> for Value {
    fn from(value: Func) -> Self {
        Value::Func(value)
    }
}

impl From<Gen> for Value {
    fn from(value: Gen) -> Self {
        Value::Gen(value)
    }
}

impl From<Task> for Value {
    fn from(value: Task) -> Self {
        Value::Task(Rc::new(RefCell::new(value)))
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::Array(value)
    }
}

#[derive(Clone)]
pub struct Func {
    pub arity: u32,
    pub program: Rc<Program>,
}

impl Func {
    pub fn new(arity: u32, program: Rc<Program>) -> Self {
        Self { arity, program }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function {}", self.arity)
    }
}

#[derive(Clone, Deref)]
pub struct Gen(pub Func);

impl Debug for Gen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Generator {:?}", self.0.arity)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ins {
    // Stack
    Push(Value),
    Pop,
    Load(u32),
    Store(Option<u32>),
    // Array
    Array(u32),
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
    // Jump
    Jmp(u32),
    JmpIf(u32),
    // Frames
    Enter,
    Leave,
    // Functions
    Call,
    Return,
    // Generators
    Continue,
    Break,
    Next,
    // Builtin
    Builtin(u32),
}

impl Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Flow {
    Continue(Value),
    Break(Value),
}

#[derive(Deref, DerefMut)]
pub struct Program(Vec<Ins>);

impl Program {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get(&self, ip: u32) -> Result<&Ins> {
        self.0.get(ip as usize).fatal("Invalid instruction pointer")
    }
}

impl Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "---")?;
        for ins in self.iter() {
            writeln!(f, "{ins:?}")?;
        }
        writeln!(f, "---")?;
        write!(f, "{} instructions", self.len())?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ProgramBuilder {
    ins: Vec<Ins>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self { ins: Vec::new() }
    }

    pub fn program(self) -> Program {
        Program(self.ins)
    }

    pub fn func(self, arity: u32) -> Func {
        Func::new(arity, Rc::new(self.program()))
    }

    pub fn gen(self, arity: u32) -> Gen {
        Gen(self.func(arity))
    }

    pub fn push(mut self, value: impl Into<Value>) -> Self {
        self.ins.push(Ins::Push(value.into()));
        self
    }

    pub fn pop(mut self) -> Self {
        self.ins.push(Ins::Pop);
        self
    }

    pub fn load(mut self, index: u32) -> Self {
        self.ins.push(Ins::Load(index));
        self
    }

    pub fn store(mut self, index: impl Into<Option<u32>>) -> Self {
        self.ins.push(Ins::Store(index.into()));
        self
    }

    pub fn array(mut self, size: u32) -> Self {
        self.ins.push(Ins::Array(size));
        self
    }

    pub fn add(mut self) -> Self {
        self.ins.push(Ins::Add);
        self
    }

    pub fn sub(mut self) -> Self {
        self.ins.push(Ins::Sub);
        self
    }

    pub fn mul(mut self) -> Self {
        self.ins.push(Ins::Mul);
        self
    }

    pub fn div(mut self) -> Self {
        self.ins.push(Ins::Div);
        self
    }

    pub fn mod_(mut self) -> Self {
        self.ins.push(Ins::Mod);
        self
    }

    pub fn negate(mut self) -> Self {
        self.ins.push(Ins::Negate);
        self
    }

    pub fn and(mut self) -> Self {
        self.ins.push(Ins::And);
        self
    }

    pub fn or(mut self) -> Self {
        self.ins.push(Ins::Or);
        self
    }

    pub fn xor(mut self) -> Self {
        self.ins.push(Ins::Xor);
        self
    }

    pub fn not(mut self) -> Self {
        self.ins.push(Ins::Not);
        self
    }

    pub fn eq(mut self) -> Self {
        self.ins.push(Ins::Eq);
        self
    }

    pub fn ne(mut self) -> Self {
        self.ins.push(Ins::Ne);
        self
    }

    pub fn lt(mut self) -> Self {
        self.ins.push(Ins::Lt);
        self
    }

    pub fn lte(mut self) -> Self {
        self.ins.push(Ins::Lte);
        self
    }

    pub fn gt(mut self) -> Self {
        self.ins.push(Ins::Gt);
        self
    }

    pub fn gte(mut self) -> Self {
        self.ins.push(Ins::Gte);
        self
    }

    pub fn jmp(mut self, index: u32) -> Self {
        self.ins.push(Ins::Jmp(index));
        self
    }

    pub fn jmp_if(mut self, index: u32) -> Self {
        self.ins.push(Ins::JmpIf(index));
        self
    }

    pub fn enter(mut self) -> Self {
        self.ins.push(Ins::Enter);
        self
    }

    pub fn leave(mut self) -> Self {
        self.ins.push(Ins::Leave);
        self
    }

    pub fn call(mut self) -> Self {
        self.ins.push(Ins::Call);
        self
    }

    pub fn ret(mut self) -> Self {
        self.ins.push(Ins::Return);
        self
    }

    pub fn continue_(mut self) -> Self {
        self.ins.push(Ins::Continue);
        self
    }

    pub fn break_(mut self) -> Self {
        self.ins.push(Ins::Break);
        self
    }

    pub fn next(mut self) -> Self {
        self.ins.push(Ins::Next);
        self
    }

    pub fn builtin(mut self, index: u32) -> Self {
        self.ins.push(Ins::Builtin(index));
        self
    }
}

#[derive(Debug, Deref, DerefMut)]
pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn pop(&mut self) -> Result<T> {
        self.0.pop().fatal("Invalid pop on empty stack")
    }

    pub fn get(&self, index: usize) -> Result<&T> {
        self.0.get(index).fatal("Invalid stack index")
    }
}

pub struct Builtin {
    pub arity: u32,
    pub func: Box<dyn Fn(Vec<Value>) -> Value>,
}

impl Builtin {
    pub fn new(arity: u32, func: impl Fn(Vec<Value>) -> Value + 'static) -> Self {
        Self {
            arity,
            func: Box::new(func),
        }
    }
}

pub struct Context {
    builtins: Vec<Builtin>,
}

impl Context {
    pub fn new(builtins: Vec<Builtin>) -> Self {
        Self { builtins }
    }
}

#[derive(Debug)]
pub struct Task {
    func: Func,
    funcs: Stack<Func>,
    ip: u32,
    ips: Stack<u32>,
    stack: Stack<Value>,
    variables: Stack<Value>,
    frames: Stack<u32>,
}

impl Task {
    pub fn new(func: Func) -> Self {
        Self::with_variables(func, Stack::new())
    }

    pub fn with_variables(func: Func, variables: Stack<Value>) -> Self {
        Self {
            func,
            funcs: Stack::new(),
            ip: 0,
            ips: Stack::new(),
            stack: Stack::new(),
            variables,
            frames: Stack(vec![0]),
        }
    }

    pub fn poll(&mut self, context: &Context) -> Result<Flow> {
        loop {
            let ins = self.func.program.get(self.ip)?;
            // println!("{:p} {ins:?} ip={} frame={} stack={}", self, self.ip, self.frames.len(), self.stack.len());
            match ins {
                Ins::Push(value) => {
                    self.stack.push(value.clone());
                    self.ip += 1;
                },
                Ins::Pop => {
                    self.stack.pop().fatal(ins)?;
                    self.ip += 1;
                },
                Ins::Load(index) => {
                    let index = self.variables.len() - 1 - *index as usize;
                    let value = self.variables.get(index).fatal(ins)?.clone();
                    self.stack.push(value);
                    self.ip += 1;
                },
                Ins::Store(index) => {
                    let value = self.stack.pop().fatal(ins)?;
                    if let Some(index) = index {
                        let index = self.variables.len() - 1 - *index as usize;
                        self.variables[index] = value;
                    } else {
                        self.variables.push(value);
                    }
                    self.ip += 1;
                },
                Ins::Array(size) => {
                    let mut array = Vec::with_capacity(*size as usize);
                    for _ in 0..*size {
                        array.push(self.stack.pop().fatal(ins)?);
                    }
                    array.reverse();
                    self.stack.push(Value::Array(array));
                    self.ip += 1;
                },
                Ins::Add => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs + rhs as f64),
                        (Value::Str(lhs), Value::Str(rhs)) => Value::Str(lhs + &rhs),
                        (Value::Array(lhs), Value::Array(rhs)) => Value::Array([lhs, rhs].concat()),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Sub => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 - rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs - rhs as f64),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Mul => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 * rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs * rhs as f64),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Div => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 / rhs),
                        (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs / rhs as f64),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Mod => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs % rhs),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Negate => {
                    let value = self.stack.pop().fatal(ins)?;
                    let result = match value {
                        Value::Int(value) => Value::Int(-value),
                        Value::Float(value) => Value::Float(-value),
                        _ => return Err(Error::fatal(format!("{ins}: Invalid operand '{value}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::And => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs && rhs),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Or => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs || rhs),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Xor => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs ^ rhs),
                        (lhs, rhs) => return Err(Error::fatal(format!("{ins}: Invalid operands '{lhs}' and '{rhs}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Not => {
                    let value = self.stack.pop().fatal(ins)?;
                    let result = match value {
                        Value::Bool(value) => Value::Bool(!value),
                        value => return Err(Error::fatal(format!("{ins}: Invalid operand '{value}'"))),
                    };
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Eq => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = Value::Bool(lhs == rhs);
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Ne => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = Value::Bool(lhs != rhs);
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Lt => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = Value::Bool(lhs < rhs);
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Lte => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = Value::Bool(lhs <= rhs);
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Gt => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = Value::Bool(lhs > rhs);
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Gte => {
                    let rhs = self.stack.pop().fatal(ins)?;
                    let lhs = self.stack.pop().fatal(ins)?;
                    let result = Value::Bool(lhs >= rhs);
                    self.stack.push(result);
                    self.ip += 1;
                },
                Ins::Jmp(index) => {
                    self.ip = *index;
                },
                Ins::JmpIf(index) => {
                    let condition = self.stack.pop().fatal(ins)?.as_bool()?;
                    if condition {
                        self.ip = *index;
                    } else {
                        self.ip += 1;
                    }
                },
                Ins::Enter => {
                    self.frames.push(self.variables.len() as u32);
                    self.ip += 1;
                },
                Ins::Leave => {
                    let left_frame = self.frames.pop().fatal(ins)?;
                    self.variables.truncate(left_frame as usize);
                    self.ip += 1;
                },
                Ins::Call => match self.stack.pop().fatal(ins)? {
                    Value::Func(func) => {
                        self.frames.push(self.variables.len() as u32);
                        let args_index = self.stack.len() - func.arity as usize;
                        self.variables.extend(self.stack.split_off(args_index));
                        self.ip += 1;
                        self.ips.push(self.ip);
                        self.ip = 0;
                        self.funcs.push(self.func.clone());
                        self.func = func;
                    },
                    Value::Gen(gen) => {
                        let args_index = self.stack.len() - gen.arity as usize;
                        let gen_variables = Stack(self.stack.split_off(args_index));
                        let gen_task = Rc::new(RefCell::new(Task::with_variables(gen.0, gen_variables)));
                        self.stack.push(Value::Task(gen_task));
                        self.ip += 1;
                    },
                    value => {
                        return Err(Error::fatal(format!("{ins}: Expected 'Function' or 'Generator' got '{value}'")))
                    },
                },
                Ins::Return => {
                    let value = self.stack.pop().fatal(ins)?;
                    if self.funcs.is_empty() {
                        // Return from top level function
                        break Ok(Flow::Break(value));
                    }
                    self.func = self.funcs.pop().fatal("Return: Function stack")?;
                    self.ip = self.ips.pop().fatal("Return: Instruction pointer stack")?;
                    let left_frame = self.frames.pop().fatal("Return: Frame stack")?;
                    self.stack.truncate(left_frame as usize);
                    self.stack.push(value);
                },
                Ins::Continue => {
                    let value = self.stack.pop().fatal(ins)?;
                    self.ip += 1;
                    break Ok(Flow::Continue(value));
                },
                Ins::Break => {
                    let value = self.stack.pop().fatal(ins)?;
                    self.ip += 1;
                    break Ok(Flow::Break(value));
                },
                Ins::Next => {
                    let task = self.stack.pop()?.as_task()?;
                    let flow = task.borrow_mut().poll(context).fatal(ins)?;
                    match flow {
                        Flow::Continue(value) => {
                            self.stack.push(value);
                        },
                        Flow::Break(value) => {
                            self.stack.push(value);
                        },
                    }
                    self.ip += 1;
                },
                Ins::Builtin(index) => {
                    let builtin = context.builtins.get(*index as usize).fatal(ins)?;
                    let args_index = self.stack.len() - builtin.arity as usize;
                    let args = self.stack.split_off(args_index);
                    let result = (builtin.func)(args);
                    self.stack.push(result);
                    self.ip += 1;
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Flow::*;
    use super::Value::Null;
    use super::*;

    #[test]
    fn test_mem_size() {
        assert_eq!(std::mem::size_of::<Value>(), 32);
        assert_eq!(std::mem::size_of::<Ins>(), 32);
        assert_eq!(std::mem::size_of::<Task>(), 144);
    }

    fn test(prog: ProgramBuilder, expected: impl Into<Value>) {
        let context = Context::new(vec![]);
        test_with_context(context, prog, expected);
    }

    fn test_with_context(context: Context, prog: ProgramBuilder, expected: impl Into<Value>) {
        let main = prog.ret().func(0);
        let exit_value = Task::new(main).poll(&context).expect("Task poll error");
        assert_eq!(exit_value, Break(expected.into()));
    }

    fn prog() -> ProgramBuilder {
        ProgramBuilder::new()
    }

    #[test]
    fn test_push() {
        test(prog().push(42), 42);
    }

    #[test]
    fn test_pop() {
        test(prog().push(42).push(21).pop(), 42);
    }

    #[test]
    fn test_store_load() {
        test(prog().push(42).store(None).push(21).load(0), 42);
        test(prog().push(42).store(None).push(21).store(None).load(1), 42);
        test(prog().push(42).store(None).push(21).store(0).load(0), 21);
    }

    #[test]
    fn test_array() {
        test(prog().push(42).push(21).array(2), vec![42.into(), 21.into()]);
    }

    #[test]
    fn test_add() {
        test(prog().push(40).push(2).add(), 42);
        test(prog().push("hello").push(" world").add(), "hello world");
        test(
            prog().push(vec![1.into(), 2.into()]).push(vec![3.into(), 4.into()]).add(),
            vec![1.into(), 2.into(), 3.into(), 4.into()],
        );
    }

    #[test]
    fn test_sub() {
        test(prog().push(44).push(2).sub(), 42);
    }

    #[test]
    fn test_mul() {
        test(prog().push(21).push(2).mul(), 42);
    }

    #[test]
    fn test_div() {
        test(prog().push(84).push(2).div(), 42);
    }

    #[test]
    fn test_mod() {
        test(prog().push(84).push(42).mod_(), 0);
    }

    #[test]
    fn test_negate() {
        test(prog().push(-42).negate(), 42);
    }

    #[test]
    fn test_and() {
        test(prog().push(true).push(true).and(), true);
        test(prog().push(true).push(false).and(), false);
    }

    #[test]
    fn test_or() {
        test(prog().push(true).push(false).or(), true);
        test(prog().push(false).push(false).or(), false);
    }

    #[test]
    fn test_xor() {
        test(prog().push(true).push(false).xor(), true);
        test(prog().push(true).push(true).xor(), false);
    }

    #[test]
    fn test_not() {
        test(prog().push(true).not(), false);
        test(prog().push(false).not(), true);
    }

    #[test]
    fn test_eq() {
        test(prog().push(42).push(42).eq(), true);
        test(prog().push(42).push(43).eq(), false);
    }

    #[test]
    fn test_ne() {
        test(prog().push(42).push(42).ne(), false);
        test(prog().push(42).push(43).ne(), true);
    }

    #[test]
    fn test_lt() {
        test(prog().push(41).push(42).lt(), true);
        test(prog().push(42).push(42).lt(), false);
    }

    #[test]
    fn test_lte() {
        test(prog().push(41).push(42).lte(), true);
        test(prog().push(42).push(42).lte(), true);
        test(prog().push(43).push(42).lte(), false);
    }

    #[test]
    fn test_gt() {
        test(prog().push(43).push(42).gt(), true);
        test(prog().push(42).push(42).gt(), false);
    }

    #[test]
    fn test_gte() {
        test(prog().push(43).push(42).gte(), true);
        test(prog().push(42).push(42).gte(), true);
        test(prog().push(41).push(42).gte(), false);
    }

    #[test]
    fn test_jmp() {
        test(prog().push(42).jmp(4).push(21).ret().push(12).ret(), 12);
    }

    #[test]
    fn test_jmp_if() {
        test(
            prog()
                .push(3)
                .store(None)
                .load(0)
                .push(1)
                .sub()
                .store(0)
                .load(0)
                .push(0)
                .gt()
                .jmp_if(2)
                .load(0)
                .ret(),
            0,
        );
    }

    #[test]
    fn test_enter_leave() {
        test(prog().push(42).store(None).enter().push(21).store(None).leave().load(0), 42);
        test(prog().push(42).store(None).enter().push(21).store(None).load(0), 21);
        test(prog().push(42).store(None).enter().push(21).store(None).load(1), 42);
    }

    #[test]
    fn test_function() {
        let square = prog().load(0).load(0).mul().ret().func(1);
        test(prog().push(8).push(square).call(), 64);
    }

    #[test]
    fn test_generator() {
        let new_repeat3 = prog()
            .load(0)
            .continue_()
            .load(0)
            .continue_()
            .load(0)
            .continue_()
            .push(Null)
            .break_()
            .gen(1);
        test(
            prog()
                .push(42)
                .push(new_repeat3)
                .call()
                .store(None)
                .load(0)
                .next()
                .load(0)
                .next()
                .add(),
            84,
        );
    }

    #[test]
    fn test_builtin() {
        let builtins = vec![Builtin::new(2, |args| {
            (args[0].as_int().unwrap() + args[1].as_int().unwrap()).into()
        })];
        let context = Context::new(builtins);
        test_with_context(context, prog().push(40).push(2).builtin(0), 42);
    }
}
