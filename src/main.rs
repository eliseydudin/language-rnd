use core::error::Error;
use lexer::{LexerError, Token};
use peek_iter::IntoPeekIter;

mod lexer;
mod parser;
mod peek_iter;

fn display_lexer(t: Result<Token, LexerError>) -> Option<Token> {
    match t {
        Ok(tok) => Some(tok),
        Err(e) => {
            println!("err: {e}");
            None
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let source = std::fs::read_to_string("./test.cofy")?;
    let peek_iter = lexer::Lexer::new(&source)
        .filter_map(|a| display_lexer(a))
        .into_peek_iter();

    let parser = parser::Parser::new(peek_iter);

    for ast_element in parser {
        println!("{ast_element:?}")
    }

    Ok(())
}
