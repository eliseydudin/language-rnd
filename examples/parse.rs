use bumpalo::Bump;
use cofy::{Expr, ExprInner, Lexer, LexerError, Operator, Parser, parser::AstInner};
use core::error::Error;
use ptree::TreeBuilder;
use std::{
    io::{Error as IoErr, stdin},
    ops::Deref,
};

fn op_to_str(op: Operator) -> &'static str {
    match op {
        Operator::Div => "/",
        Operator::Minus => "-",
        Operator::Mult => "*",
        Operator::Plus => "+",
    }
}

fn print_expr(tree: &mut TreeBuilder, expr: &Expr) {
    match expr.inner() {
        ExprInner::Number(num) => tree.add_empty_child(format!("num {num}")),
        ExprInner::BinOp {
            left,
            operator,
            right,
        } => {
            tree.begin_child("binop".to_owned());
            print_expr(tree, left.deref());
            tree.add_empty_child(format!("op {}", op_to_str(*operator)));
            print_expr(tree, right.deref());
            tree.end_child();
            tree
        }
        ExprInner::Unary { operator, data } => {
            tree.begin_child("unary".to_owned());
            tree.add_empty_child(format!("{operator:?}"));
            print_expr(tree, data.deref());
            tree.end_child()
        }
    };
}

fn print_tree(ast: Parser) {
    let mut tree = TreeBuilder::new("stdin's abstract syntax tree".to_owned());
    for a in ast {
        match a {
            Ok(elem) => match elem.inner {
                AstInner::Const { name, value } => {
                    let refmut = tree.begin_child(format!("const `{name}`"));
                    print_expr(refmut, &value);
                    refmut.end_child();
                }
            },
            Err(e) => {
                tree.begin_child("error".to_owned())
                    .add_empty_child(e.to_string())
                    .end_child();
            }
        };
    }

    let tree = tree.build();
    ptree::print_tree(&tree).expect("cannot print the tree")
}

fn main() -> Result<(), Box<dyn Error>> {
    let bump = Bump::new();
    println!("write your code here");
    println!("print `end` to start the parser");
    println!();

    let input = stdin()
        .lines()
        .take_while(|a| match a.as_deref() {
            Ok("end") | Err(_) => false,
            _ => true,
        })
        .collect::<Result<Vec<String>, IoErr>>()?
        .join("\n");

    println!();

    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<_>, LexerError>>()?;
    print_tree(Parser::new(&bump, &tokens));
    println!("bytes allocated on the bump: {}", bump.allocated_bytes());

    Ok(())
}
