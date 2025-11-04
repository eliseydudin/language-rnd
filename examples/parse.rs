use bumpalo::Bump;
use cofy::{
    Expr, ExprInner, Lexer, LexerError, Operator, Parser,
    parser::{AstInner, Type},
};
use ptree::TreeBuilder;
use std::{
    error::Error,
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
            tree.add_empty_child(format!("op {}", op_to_str(*operator)));
            print_expr(tree, data.deref());
            tree.end_child()
        }
        ExprInner::Identifier(ident) => tree.add_empty_child(format!("ident {ident}")),
        ExprInner::Call { object, params } => {
            let mutref = tree
                .begin_child("call".to_owned())
                .begin_child("object".to_owned());
            print_expr(mutref, object.as_ref());
            mutref.end_child().begin_child("params".to_owned());
            for param in params {
                print_expr(mutref, param)
            }

            tree.end_child().end_child()
        }
        ExprInner::Access { object, property } => {
            let mutref = tree
                .begin_child("access".to_owned())
                .begin_child("object".to_owned());
            print_expr(mutref, object.as_ref());
            mutref
                .end_child()
                .add_empty_child(format!("property `{property}`"))
                .end_child()
        }
    };
}

fn print_type(tree: &mut TreeBuilder, ty: &Type) {
    match ty {
        Type::Plain(name) => {
            tree.add_empty_child(format!("plain `{name}`"));
        }
        Type::Function { params, returns } => {
            tree.begin_child("fn".to_owned())
                .begin_child("params".to_owned());
            for param in params {
                print_type(tree, param);
            }
            tree.end_child().begin_child("returns".to_owned());
            print_type(tree, returns.as_ref());
            tree.end_child().end_child();
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
                AstInner::FunctionPrototype {
                    name,
                    type_of,
                    type_parameters,
                } => {
                    let refmut = tree.begin_child(format!("fn proto `{name}`"));
                    print_type(refmut, &type_of);
                    refmut
                        .add_empty_child(format!("type params {type_parameters:?}"))
                        .end_child();
                }
                AstInner::Function {
                    name,
                    params,
                    body,
                    type_parameters,
                } => {
                    let refmut = tree
                        .begin_child(format!("fn `{name}`"))
                        .begin_child("params".to_owned());
                    for param in params.iter() {
                        print_expr(refmut, param);
                    }
                    refmut.end_child().begin_child("body".to_owned());
                    for pipe in body.iter() {
                        print_expr(refmut, pipe);
                    }
                    refmut
                        .end_child()
                        .add_empty_child(format!("type params {type_parameters:?}"))
                        .end_child();
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
