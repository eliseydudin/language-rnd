use core::fmt::Display;
use core::{error::Error, f64};
use parser::IntoParser;

use crate::environment::{Environment, Value};

mod environment;
mod lexer;
mod parser;
mod peek_iter;

fn display<T, E: Display>(t: Result<T, E>) -> Option<T> {
    match t {
        Ok(tok) => Some(tok),
        Err(e) => {
            println!("err: {e}");
            None
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILE: &str = "./test.cofy";

    let source = std::fs::read_to_string(FILE)?;
    let ast = lexer::Lexer::new(&source)
        .filter_map(display)
        .into_parser()
        .filter_map(display)
        .map(|a| a.inner);

    let mut env = Environment::new();
    env.push_function("sin", |mut args| {
        let num = args.remove(0).unwrap_number();
        Some(Value::Number(num.sin()))
    });
    env.push_function("cos", |mut args| {
        let num = args.remove(0).unwrap_number();
        Some(Value::Number(num.cos()))
    });
    env.push_function("round", |mut args| {
        let num = args.remove(0).unwrap_number();
        Some(Value::Number(num.round()))
    });
    env.push_constant("pi", Value::Number(f64::consts::PI));
    env.interpret_ast(ast);

    for (name, val) in env.constants {
        println!("const {name} = {val:?};");
    }

    Ok(())
}
