use bumpalo::Bump;
use cofy::{Lexer, LexerError, Parser};
use core::error::Error;
use std::io::{Error as IoErr, stdin};

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
    for ast in Parser::new(&bump, &tokens) {
        match ast {
            Ok(a) => println!("{a:#?}"),
            Err(e) => println!("an error occured: {e:#?}"),
        }
    }

    Ok(())
}
