use stackz::vm::{Builtin, Value};

pub fn builtins() -> Vec<(&'static str, Builtin)> {
    vec![
        ("input", Builtin::new(0, input)),
        ("print", Builtin::new(1, print)),
        ("get", Builtin::new(2, get)),
        ("split", Builtin::new(2, split)),
    ]
}

pub fn input(_args: Vec<Value>) -> Value {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Builtin: input: Failed to read stdin");
    Value::Str(input.trim().to_string())
}

pub fn print(args: Vec<Value>) -> Value {
    let Some(Value::Str(value)) = args.get(0) else {
        panic!("Builtin: print: expected 1 string argument");
    };
    println!("{value}");
    Value::Null
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
