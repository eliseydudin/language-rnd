//use environment::{Environment, Value};

//mod environment;
pub mod cow_str;
pub mod lexer;
pub mod parser;

pub use cow_str::CowStr;
pub use lexer::{Lexer, LexerError, SourcePosition, Token, TokenRepr, WithPos, WithPosOrEof};
pub use parser::{Expr, ExprInner, Operator, Parser, ParserError};
