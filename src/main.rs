mod ast_rename;
mod cli;

use ast_rename::print_tree;
use bumpalo::Bump;
use clap::Parser as _;
use cli::Args;
use cofy::{Lexer, LexerError, Parser};
use std::{
    error::Error,
    fs::read_to_string,
    io::{Error as IoErr, stdin},
};

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let input = match (args.command, args.script) {
        (Some(_), Some(_)) => {
            return Err(Box::new(IoErr::other(
                "both script path and command can't be passed",
            )));
        }
        (Some(string), None) => string,
        (None, Some(path)) => read_to_string(path)?,
        (None, None) => {
            println!("write your code here");
            println!("print `end` to start the parser");
            println!();

            stdin()
                .lines()
                .take_while(|line| !matches!(line.as_deref(), Ok("end") | Err(_)))
                .collect::<Result<Vec<String>, IoErr>>()?
                .join("\n")
        }
    };

    let syntax_tree = args.syntax_tree;

    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<_>, LexerError>>()?;
    if syntax_tree {
        println!();
        let bump = Bump::new();
        print_tree(Parser::new(&bump, &tokens));
        println!("bytes allocated on the bump: {}", bump.allocated_bytes());
    }

    Ok(())
}
