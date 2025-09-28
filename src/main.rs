use core::error::Error;

mod lexer;
mod parser;

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
            Ok(ast) => println!("AST ELEMENT: {ast:#?}"),
            Err(e) => {
                println!("ERROR: {e}");
            }
        }
    }

    Ok(())
}
