use bumpalo::Bump;
use cofy::{IntoParser, Lexer};
use std::io::{Error as IoError, Write, stdin, stdout};

pub fn input(prompt: &str) -> Result<String, IoError> {
    print!("{}", prompt);

    stdout().flush()?;
    let mut result = String::new();
    stdin().read_line(&mut result)?;
    Ok(result)
}

const RED_TEXT: &str = "\x1b[93m";
const GREEN_TEXT: &str = "\x1b[92m";
const RESET_TEXT: &str = "\x1b[0m";

fn main() {
    let source =
        input("enter an expression: ").expect("something went wrong while interacting with stdio");

    let bump = Bump::new();
    let mut lexer_errors = Vec::new();
    let mut parser_errors = Vec::new();

    let ast = Lexer::new(&source)
        .filter_map(|tok| match tok {
            Ok(data) => Some(data),
            Err(e) => {
                lexer_errors.push(e);
                None
            }
        })
        .into_parser(&bump)
        .filter_map(|a| match a {
            Ok(res) => Some(res),
            Err(e) => {
                parser_errors.push(e);
                None
            }
        })
        .collect::<Vec<_>>();

    for err in &lexer_errors {
        println!("{RED_TEXT}error:{RESET_TEXT} {err}");
    }

    for err in &parser_errors {
        println!("{RED_TEXT}error:{RESET_TEXT} {err}");
    }

    if lexer_errors.len() == 0 && parser_errors.len() == 0 {
        for a in ast {
            println!(
                "{GREEN_TEXT}ast element at {}:{RESET_TEXT} {:#?}",
                a.pos, a.inner
            )
        }
    }

    println!("allocated bytes: {}", bump.allocated_bytes());
}
