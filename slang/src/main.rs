use std::io::{stdin, Read};

use anyhow::Context;
use clap::Parser as _;
use stackz::vm::{Flow, Func, Task, Value};

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod compiler;
mod error;
mod lexer;
mod parser;

#[derive(clap::Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Print tokens output by the lexer
    Lex,
    /// Print AST output by the parser
    Parse,
    /// Print program output by the compiler
    Compile,
    /// Run program from stdin and print the returned value
    Run,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Command::Lex => {
            let lexer = lex(&input()?)?;
            println!("{lexer:?}");
        },

        Command::Parse => {
            let parser = parse(lex(&input()?)?)?;
            println!("{parser:?}");
        },

        Command::Compile => {
            let compiler = compile(parse(lex(&input()?)?)?)?;
            println!("{compiler:?}");
        },

        Command::Run => {
            let exit_value = run(compile(parse(lex(&input()?)?)?)?)?;
            println!("Exit: {exit_value}");
        },
    }

    Ok(())
}

fn input() -> anyhow::Result<String> {
    let mut input = String::new();
    stdin().read_to_string(&mut input).context("Read input error")?;
    Ok(input)
}

fn lex(input: &str) -> anyhow::Result<Lexer> {
    Lexer::lex(input).context("Lexer error")
}

fn parse(lexer: Lexer) -> anyhow::Result<Parser> {
    Parser::parse(lexer).context("Parser error")
}

fn compile(parser: Parser) -> anyhow::Result<Func> {
    Compiler::compile(&parser.root).context("Compiler error")
}

fn run(main: Func) -> anyhow::Result<Value> {
    match Task::new(main).poll().context("Interpreter error")? {
        cont @ Flow::Continue(_) => Err(anyhow::anyhow!("Unexpected continue in main: {cont:?}")),
        Flow::Break(value) => Ok(value),
    }
}

#[cfg(test)]
mod tests {
    use stackz::vm::Value;

    fn test(input: &str, expected: Value) {
        let lexer = super::lex(input).unwrap();
        let parser = super::parse(lexer).unwrap();
        let main = super::compile(parser).unwrap();
        let actual = super::run(main).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_pow() {
        test(
            r#"
            {
                pow = |x y| {
                    acc = 1;
                    while y > 0 {
                        acc = acc * x;
                        y = y - 1;
                    }
                    acc
                };
                pow(2 10)
            }
            "#,
            Value::Int(1024),
        );
    }
}