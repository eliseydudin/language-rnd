use bumpalo::Bump;
use cofy::{Lexer, LexerError, Parser};
use core::error::Error;
use std::io::{Error as IoErr, ErrorKind, stdin};

fn main() -> Result<(), Box<dyn Error>> {
    let bump = Bump::new();
    let input = stdin()
        .lines()
        .next()
        .ok_or(IoErr::new(ErrorKind::Other, "no input"))??;
    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<_>, LexerError>>()?;
    let mut parser = Parser::new(&bump, &tokens);

    match parser.expression() {
        Ok(expr) => println!("found the following expression: {expr:#?}"),
        Err(e) => println!("found the following error: {e}"),
    }

    Ok(())
}
