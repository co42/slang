use std::io::Read;

use stackz::vm::{Builtin, Value};

pub fn builtins() -> Vec<(&'static str, Builtin)> {
    vec![
        ("input", Builtin::new(0, input)),
        ("print", Builtin::new(1, print)),
        ("int", Builtin::new(1, parse_int)),
        ("get", Builtin::new(2, get)),
        ("len", Builtin::new(1, len)),
        ("sorted", Builtin::new(1, sorted)),
        ("split", Builtin::new(2, split)),
    ]
}

pub fn input(_args: Vec<Value>) -> Value {
    let mut input = String::new();
    std::io::stdin()
        .read_to_string(&mut input)
        .expect("Builtin: input: Failed to read stdin");
    Value::Str(input.trim().to_string())
}

pub fn print(args: Vec<Value>) -> Value {
    match args.get(0) {
        Some(value) => print_inner(value),
        None => {
            panic!("Builtin: print: expected 1 argument");
        },
    };
    println!();
    Value::Null
}

pub fn parse_int(args: Vec<Value>) -> Value {
    let Some(Value::Str(value)) = args.get(0) else {
        panic!("Builtin: int: expected 1 string argument");
    };
    match value.parse::<i64>() {
        Ok(value) => Value::Int(value),
        Err(_) => Value::Null,
    }
}

fn print_inner(value: &Value) {
    match value {
        Value::Null => print!("null"),
        Value::Bool(value) => print!("{}", value),
        Value::Int(value) => print!("{}", value),
        Value::Float(value) => print!("{}", value),
        Value::Str(value) => print!("\"{}\"", value),
        Value::Array(array) => {
            print!("[");
            for (i, value) in array.iter().enumerate() {
                if i > 0 {
                    print!(", ");
                }
                print_inner(value);
            }
            print!("]");
        },
        Value::Func(_) => {
            print!("function");
        },
        Value::Gen(_) => {
            print!("generator");
        },
        Value::Task(_) => {
            print!("task");
        },
    }
}

pub fn get(args: Vec<Value>) -> Value {
    let Some(Value::Array(array)) = args.get(0) else {
        panic!("Builtin: array_get: expected 1 array argument");
    };
    let Some(Value::Int(index)) = args.get(1) else {
        panic!("Builtin: array_get: expected 2 integer argument");
    };
    array.get(*index as usize).cloned().unwrap_or(Value::Null)
}

pub fn len(args: Vec<Value>) -> Value {
    let Some(Value::Array(array)) = args.get(0) else {
        panic!("Builtin: len: expected 1 array argument");
    };
    Value::Int(array.len() as i64)
}

pub fn sorted(args: Vec<Value>) -> Value {
    let Some(Value::Array(array)) = args.get(0) else {
        panic!("Builtin: sorted: expected 1 array argument");
    };
    let mut array = array.clone();
    array.sort();
    Value::Array(array)
}

pub fn split(args: Vec<Value>) -> Value {
    let Some(Value::Str(value)) = args.get(0) else {
        panic!("Builtin: split: expected 1 string argument");
    };
    let Some(Value::Str(delimiter)) = args.get(1) else {
        panic!("Builtin: split: expected 2 string argument");
    };
    Value::Array(value.split(delimiter).map(|s| Value::Str(s.to_string())).collect())
}

#[cfg(test)]
mod tests {
    use stackz::vm::Value;

    fn test(input: &str, expected: Value) {
        let lexer = crate::lex(input).unwrap();
        let parser = crate::parse(lexer).unwrap();
        let main = crate::compile(parser).unwrap();
        let actual = crate::run(main).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_split() {
        test(
            r#"
            {
                s = "hello,world";
                split(s ",")
            }
            "#,
            Value::Array(vec![Value::Str("hello".to_string()), Value::Str("world".to_string())]),
        );
    }
}
