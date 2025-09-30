use core::error::Error;
use parser::{Expr, Operator};

use crate::parser::AstElement;

mod lexer;
mod parser;

fn calculate_expr(expr: Expr) -> f64 {
    match expr {
        Expr::BinOp {
            left,
            right,
            operator,
        } => {
            let left = calculate_expr(*left);
            let right = calculate_expr(*right);
            match operator {
                Operator::Plus => left + right,
                Operator::Minus => left - right,
                Operator::Div => left / right,
                Operator::Mult => left * right,
            }
        }
        Expr::Number(num) => num.parse().unwrap(),
        Expr::Unary { oper, inner } => {
            let val = calculate_expr(*inner);
            match oper {
                Operator::Minus => -val,
                Operator::Plus => val,
                _ => unreachable!(),
            }
        }
        _ => panic!(),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let source = std::fs::read_to_string("./test.cofy")?;
    let mut lexer = lexer::Lexer::new(&source);
    let mut tokens = vec![];

    println!("lexing...");
    for token in &mut lexer {
        match token {
            Ok(token) => {
                tokens.push(token);
                println!(
                    "TOKEN: {:?} at {} of type {:?}",
                    token.data, token.pos, token.repr
                )
            }
            Err(e) => println!("ERROR: {e}"),
        }
    }
    println!("parsing...");
    let mut parser = parser::Parser::new(&tokens);
    for ast in &mut parser {
        match ast {
            Ok(ast) => {
                println!("AST ELEMENT: {ast:#?}");
                let AstElement::Const { name, value } = ast;
                println!("const {name} = {}", calculate_expr(value))
            }
            Err(e) => {
                println!("ERROR: {e}");
            }
        }
    }

    Ok(())
}
