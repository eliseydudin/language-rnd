use core::error::Error;
use core::fmt::{Debug, Display};
use parser::IntoParser;
//use environment::{Environment, Value};

//mod environment;
mod lexer;
mod parser;
mod peek_iter;

fn display<T: Debug, E: Display>(t: Result<T, E>) -> Option<T> {
    match t {
        Ok(tok) => {
            //println!("some: {tok:?}");
            Some(tok)
        }
        Err(e) => {
            println!("err: {e}");
            None
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILE: &str = "./test.cofy";

    let source = std::fs::read_to_string(FILE)?;
    lexer::Lexer::new(&source)
        .filter_map(display)
        .into_parser()
        .filter_map(display)
        .for_each(|a| println!("{FILE}:{} {:#?}", a.pos, a.inner));

    // let mut env = Environment::new();
    // env.push_function("sin", |args| args.unwrap_number().sin().into());
    // env.push_function("cos", |args| args.unwrap_number().cos().into());
    // env.push_function("round", |args| args.unwrap_number().round().into());
    // env.push_function("sum", |args| {
    //     let args = args.unwrap_tuple();
    //     args.into_iter()
    //         .reduce(|a, b| (a.unwrap_number() + b.unwrap_number()).into())
    //         .expect("tuples are never unit")
    // });
    // env.push_constant("pi", Value::Number(f64::consts::PI));
    // env.interpret_ast(ast);

    // for (name, val) in env.constants {
    //     println!("const {name} = {val:?};");
    // }

    Ok(())
}
