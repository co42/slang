use std::io::{stdin, Read};

use anyhow::Context;
use clap::Parser as _;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::{Value, Vm};

mod compiler;
mod error;
mod lexer;
mod parser;
mod vm;

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
    /// Interpret stdin and print the exit value
    Interpret,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Command::Lex => {
            let lexer = lex(&input()?)?;
            println!("{lexer:?}");
        }

        Command::Parse => {
            let parser = parse(lex(&input()?)?)?;
            println!("{parser:?}");
        }

        Command::Compile => {
            let compiler = compile(parse(lex(&input()?)?)?)?;
            println!("{compiler:?}");
        }

        Command::Interpret => {
            let exit_value = interpret(compile(parse(lex(&input()?)?)?)?)?;
            println!("Exit: {exit_value}");
        }
    }

    Ok(())
}

fn input() -> anyhow::Result<String> {
    let mut input = String::new();
    stdin()
        .read_to_string(&mut input)
        .context("Read input error")?;
    Ok(input)
}

fn lex(input: &str) -> anyhow::Result<Lexer> {
    Lexer::lex(input).context("Lexer error")
}

fn parse(lexer: Lexer) -> anyhow::Result<Parser> {
    Parser::parse(lexer).context("Parser error")
}

fn compile(parser: Parser) -> anyhow::Result<Compiler> {
    Compiler::compile(parser).context("Compiler error")
}

fn interpret(compiler: Compiler) -> anyhow::Result<Value> {
    Vm::new(compiler).interpret().context("Interpreter error")
}
