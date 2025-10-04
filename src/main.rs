use core::error::Error;
use core::fmt::Debug;
use lexer::TokenRepr;
use parser::{Expr, IntoParser};

use crate::parser::AstRepr;

mod lexer;
mod parser;
mod peek_iter;

fn display<T, E: Debug>(t: Result<T, E>) -> Option<T> {
    match t {
        Ok(tok) => Some(tok),
        Err(e) => {
            println!("err: {e:?}");
            None
        }
    }
}

fn calculate_expr(expr: Expr) -> f64 {
    match expr {
        Expr::Number(num) => num
            .parse()
            .expect("numbers parsed by the lexer are guaranteed to be valid integers"),
        Expr::String(_) => todo!("cannot use strings in math expressions currently"),
        Expr::Unary(e) => -calculate_expr(*e),
        Expr::BinOp { left, op, right } => {
            let left = calculate_expr(*left);
            let right = calculate_expr(*right);
            match op {
                TokenRepr::Plus => left + right,
                TokenRepr::Minus => left - right,
                TokenRepr::Mult => left * right,
                TokenRepr::Div => left / right,
                _ => panic!("invalid operand"),
            }
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILE: &'static str = "./test.cofy";

    let source = std::fs::read_to_string(FILE)?;
    lexer::Lexer::new(&source)
        .filter_map(display)
        .into_parser()
        .filter_map(display)
        .for_each(|ast| {
            println!("{FILE}:{} {:#?}", ast.pos, ast.inner);
            let AstRepr::Const { name, value } = ast.inner;
            println!("const {name} = {}", calculate_expr(value))
        });

    Ok(())
}
