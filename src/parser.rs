use crate::{
    lexer::{Token, TokenRepr, WithPos, WithPosOrEof},
    peek_iter::PeekIter,
};
use core::{error, fmt};

#[derive(Debug, PartialEq)]
pub enum ErrorRepr {
    Eof,
    Unexpected {
        found: TokenRepr,
        expected: TokenRepr,
    },
    UnexpectedMult {
        found: TokenRepr,
        expected: &'static [TokenRepr],
    },
    DoubleUnary,
}

impl fmt::Display for ErrorRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "expected a token, found eof"),
            Self::Unexpected { found, expected } => {
                write!(f, "expected {expected:?}, found {found:?}")
            }
            Self::UnexpectedMult { found, expected } => {
                write!(f, "expected {{ ")?;
                for (i, elem) in expected.iter().copied().enumerate() {
                    if i == expected.len() - 1 {
                        write!(f, "{elem:?} }}")?
                    } else {
                        write!(f, "{elem:?} | ")?
                    }
                }

                write!(f, " found {found:?}")
            }
            Self::DoubleUnary => write!(f, "an unary expression repeated 2 times is not allowed"),
        }
    }
}

impl error::Error for ErrorRepr {}

pub type ParserError = WithPosOrEof<ErrorRepr>;

pub const fn throw_eof_error() -> ParserError {
    WithPosOrEof::Eof(ErrorRepr::Eof)
}

pub const fn throw_unexpected_mult(found: Token, expected: &'static [TokenRepr]) -> ParserError {
    WithPosOrEof::Pos(WithPos::new(
        ErrorRepr::UnexpectedMult {
            found: found.repr,
            expected,
        },
        found.pos,
    ))
}

pub const fn throw_double_unary(tok: Token) -> ParserError {
    WithPosOrEof::Pos(WithPos::new(ErrorRepr::DoubleUnary, tok.pos))
}

pub fn binop_expr<'a>(left: Expr<'a>, op: TokenRepr, right: Expr<'a>) -> Expr<'a> {
    Expr::BinOp {
        left: Box::new(left),
        op,
        right: Box::new(right),
    }
}

pub fn unary_expr(expr: Expr<'_>) -> Expr<'_> {
    Expr::Unary(Box::new(expr))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'src> {
    Number(&'src str),
    String(&'src str),
    Identifier(&'src str),
    BinOp {
        left: Box<Self>,
        op: TokenRepr,
        right: Box<Self>,
    },
    Unary(Box<Self>),
    Access {
        object: Box<Self>,
        property: &'src str,
    },
    Call {
        object: Box<Self>,
        params: Box<Self>,
    },
    Tuple(Vec<Self>),
    Unit, // same as rust's ()
}

impl<'a> From<Vec<Expr<'a>>> for Expr<'a> {
    fn from(value: Vec<Expr<'a>>) -> Self {
        Self::Tuple(value)
    }
}

type FunctionParams<'src> = Option<Vec<Expr<'src>>>;
type FunctionBody<'src> = Vec<Expr<'src>>;

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum AstRepr<'src> {
    Const {
        name: &'src str,
        value: Expr<'src>,
    },
    Function {
        name: &'src str,
        params: FunctionParams<'src>,
        body: FunctionBody<'src>,
    },
    Comment(&'src str),
}

pub type Ast<'a> = WithPos<AstRepr<'a>>;

pub struct Parser<'src, I>
where
    I: Iterator<Item = Token<'src>>,
{
    iter: PeekIter<Token<'src>, I>,
}

pub type ParserResult<T> = Result<T, ParserError>;

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    pub const fn new(iter: PeekIter<Token<'src>, I>) -> Self {
        Self { iter }
    }

    /// Get the current token in the iterator, if the current token is [`None`],
    /// return an EOF error.
    pub fn current(&self) -> ParserResult<Token<'src>> {
        self.iter.current_copied().ok_or_else(throw_eof_error)
    }

    /// Get the current token in the iterator and advance it, if the current token is [`None`],
    /// return an EOF error.
    pub fn advance(&mut self) -> ParserResult<Token<'src>> {
        self.iter.advance().ok_or_else(throw_eof_error)
    }

    /// Get the next token in the iterator, if the token is [`None`],
    /// return an EOF error.
    pub fn peek(&self) -> ParserResult<Token<'src>> {
        self.iter.peek_copied().ok_or_else(throw_eof_error)
    }

    pub fn expect(&mut self, expect: TokenRepr) -> ParserResult<Token<'src>> {
        let curr = self.advance()?;

        if curr.repr == expect {
            Ok(curr)
        } else {
            Err(WithPosOrEof::Pos(WithPos::new(
                ErrorRepr::Unexpected {
                    found: curr.repr,
                    expected: expect,
                },
                curr.pos,
            )))
        }
    }

    pub fn parse_const(&mut self) -> ParserResult<Ast<'src>> {
        let c = self.expect(TokenRepr::Const)?;
        let name = self.expect(TokenRepr::Identifier)?;
        self.expect(TokenRepr::Set)?;
        let value = self.parse_expr(TokenRepr::Semicolon)?;

        Ok(Ast::new(
            AstRepr::Const {
                name: name.data,
                value,
            },
            c.pos,
        ))
    }

    pub fn parse_expr(&mut self, end: TokenRepr) -> ParserResult<Expr<'src>> {
        let curr = self.parse_value(true)?;
        self.cont_expr_or_end(curr, &[end]).map(|(_, e)| e)
    }

    pub fn parse_identifier_or_call(
        &mut self,
        start: Expr<'src>,
        allow_call: bool,
    ) -> ParserResult<Expr<'src>> {
        let next = self.current()?;

        if next.repr == TokenRepr::Dot {
            let property = self.peek()?;
            let access = Expr::Access {
                object: Box::new(start),
                property: property.data,
            };

            self.advance()?;
            self.advance()?;

            self.parse_identifier_or_call(access, allow_call)
        } else if next.repr == TokenRepr::LParen && allow_call {
            self.advance()?;

            //let params = self.parse_expr(TokenRepr::RParen)?;
            let function_call = Expr::Call {
                object: Box::new(start),
                params: Box::new(self.parse_tuple()?),
            };

            self.parse_identifier_or_call(function_call, allow_call)
        } else {
            Ok(start)
        }
    }

    /// Parse one value in the token tree
    pub fn parse_value(&mut self, allow_unary: bool) -> ParserResult<Expr<'src>> {
        const EXPECTED: &[TokenRepr] = &[
            TokenRepr::Number,
            TokenRepr::String,
            TokenRepr::Identifier,
            TokenRepr::LParen,
            TokenRepr::Minus,
        ];

        const EXPECTED_NO_UNARY: &[TokenRepr] = &[
            TokenRepr::Number,
            TokenRepr::String,
            TokenRepr::Identifier,
            TokenRepr::LParen,
        ];

        let current = self.advance()?;

        match current.repr {
            TokenRepr::Number => Ok(Expr::Number(current.data)),
            TokenRepr::String => Ok(Expr::String(current.data)),
            TokenRepr::Identifier => {
                let value = Expr::Identifier(current.data);
                self.parse_identifier_or_call(value, true)
            }
            TokenRepr::LParen => self.parse_tuple(),
            TokenRepr::Minus if allow_unary => Ok(unary_expr(self.parse_value(false)?)),
            TokenRepr::Minus if !allow_unary => Err(throw_double_unary(current)),
            _ if allow_unary => Err(throw_unexpected_mult(current, EXPECTED)),
            _ => Err(throw_unexpected_mult(current, EXPECTED_NO_UNARY)),
        }
    }

    pub fn parse_tuple(&mut self) -> ParserResult<Expr<'src>> {
        let mut result = vec![];

        loop {
            if self.current().map(|a| a.repr) == Ok(TokenRepr::RParen) && result.len() == 0 {
                self.advance()?;
                return Ok(Expr::Unit);
            }

            let val = self.parse_value(true)?;
            let (end, expr) = self.cont_expr_or_end(val, &[TokenRepr::Coma, TokenRepr::RParen])?;

            result.push(expr);
            if end == TokenRepr::RParen {
                break;
            }
        }

        if result.len() == 1 {
            Ok(result.remove(0))
        } else {
            Ok(Expr::Tuple(result))
        }
    }

    pub fn cont_expr_or_end(
        &mut self,
        start: Expr<'src>,
        end: &[TokenRepr],
    ) -> ParserResult<(TokenRepr, Expr<'src>)> {
        let current = self.advance()?;

        match current.repr {
            TokenRepr::Mult | TokenRepr::Div => {
                let value = self.parse_value(true)?;
                self.cont_expr_or_end(binop_expr(start, current.repr, value), end)
            }

            TokenRepr::Plus | TokenRepr::Minus => {
                let next = self.parse_value(true)?;
                let (end, next) = self.cont_expr_or_end(next, end)?;
                Ok((end, binop_expr(start, current.repr, next)))
            }

            e if end.contains(&e) => Ok((e, start)),

            _ => Err(throw_unexpected_mult(
                current,
                &[
                    TokenRepr::Mult,
                    TokenRepr::Div,
                    TokenRepr::Plus,
                    TokenRepr::Minus,
                ],
            )),
        }
    }

    fn parse_function_body_once(&mut self) {
        todo!()
    }

    pub fn parse_function_body(&mut self) -> ParserResult<FunctionBody<'src>> {
        let mut result = vec![];
        loop {
            let val = self.parse_value(true)?;
            let (end, expr) =
                self.cont_expr_or_end(val, &[TokenRepr::Arrow, TokenRepr::Semicolon])?;
            result.push(expr);

            match end {
                TokenRepr::Arrow => continue,
                TokenRepr::Semicolon => break,
                _ => unreachable!(),
            }
        }

        Ok(result)
    }

    pub fn parse_function_params(&mut self) -> ParserResult<FunctionParams<'src>> {
        macro_rules! insert {
            ($vec:ident, $data:expr) => {
                match &mut $vec {
                    None => {
                        let new = vec![$data];
                        $vec = Some(new);
                    }
                    Some(v) => v.push($data),
                }
            };
        }

        let mut result = None;
        loop {
            let next = self.current()?;
            match next.repr {
                TokenRepr::Set => return Ok(result),
                TokenRepr::Identifier => insert!(result, Expr::Identifier(next.data)),
                TokenRepr::Number => insert!(result, Expr::Number(next.data)),
                TokenRepr::String => insert!(result, Expr::String(next.data)),
                _ => {
                    return Err(throw_unexpected_mult(
                        next,
                        &[
                            TokenRepr::Set,
                            TokenRepr::Identifier,
                            TokenRepr::Number,
                            TokenRepr::String,
                        ],
                    ));
                }
            }
        }
    }

    pub fn parse_function(&mut self) -> ParserResult<Ast<'src>> {
        let start = self.expect(TokenRepr::Fn)?;
        let name = self.expect(TokenRepr::Identifier)?.data;
        let params = self.parse_function_params()?;
        self.expect(TokenRepr::Set)?;
        let body = self.parse_function_body()?;

        Ok(Ast::new(
            AstRepr::Function { name, params, body },
            start.pos,
        ))
    }
}

impl<'src, I: Iterator<Item = Token<'src>>> Iterator for Parser<'src, I> {
    type Item = ParserResult<Ast<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.iter.current_copied()?;
        let res = match current.repr {
            TokenRepr::Const => self.parse_const(),
            TokenRepr::Fn => self.parse_function(),
            TokenRepr::Comment => {
                let _ = self.advance();
                Ok(Ast::new(AstRepr::Comment(current.data), current.pos))
            }
            _ => todo!("{current:?}"),
        };

        Some(res)
    }
}

pub trait IntoParser<'s>: Iterator<Item = Token<'s>> + Sized {
    fn into_parser(self) -> Parser<'s, Self>;
}

impl<'s, I: Iterator<Item = Token<'s>>> IntoParser<'s> for I {
    fn into_parser(self) -> Parser<'s, Self> {
        Parser::new(PeekIter::new(self))
    }
}
