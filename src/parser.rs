use crate::{
    lexer::{Token, TokenRepr, WithPos, WithPosOrEof},
    peek_iter::PeekIter,
};

#[derive(Debug)]
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
    ExpectedExpression(TokenRepr),
    DoubleUnary,
}

pub type ParserError = WithPosOrEof<ErrorRepr>;

pub fn throw_eof_error() -> ParserError {
    WithPosOrEof::Eof(ErrorRepr::Eof)
}

pub fn throw_expected_expression(tok: Token) -> ParserError {
    WithPosOrEof::Pos(WithPos::new(
        ErrorRepr::ExpectedExpression(tok.repr),
        tok.pos,
    ))
}

pub fn throw_unexpected(found: Token, expected: TokenRepr) -> ParserError {
    WithPosOrEof::Pos(WithPos::new(
        ErrorRepr::Unexpected {
            found: found.repr,
            expected,
        },
        found.pos,
    ))
}

pub fn throw_unexpected_mult(found: Token, expected: &'static [TokenRepr]) -> ParserError {
    WithPosOrEof::Pos(WithPos::new(
        ErrorRepr::UnexpectedMult {
            found: found.repr,
            expected,
        },
        found.pos,
    ))
}

pub fn throw_double_unary(tok: Token) -> ParserError {
    WithPosOrEof::Pos(WithPos::new(ErrorRepr::DoubleUnary, tok.pos))
}

pub fn binop_expr<'a>(left: Expr<'a>, op: TokenRepr, right: Expr<'a>) -> Expr<'a> {
    Expr::BinOp {
        left: Box::new(left),
        op,
        right: Box::new(right),
    }
}

pub fn unary_expr<'a>(expr: Expr<'a>) -> Expr<'a> {
    Expr::Unary(Box::new(expr))
}

#[derive(Debug)]
pub enum Expr<'src> {
    Number(&'src str),
    String(&'src str),
    BinOp {
        left: Box<Self>,
        op: TokenRepr,
        right: Box<Self>,
    },
    Unary(Box<Self>),
}

#[derive(Debug)]
pub enum AstRepr<'src> {
    Const { name: &'src str, value: Expr<'src> },
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

    // /// Get the current token in the iterator, if the current token is [`None`],
    // /// return an EOF error.
    // pub fn current(&self) -> ParserResult<Token<'src>> {
    //     self.iter.current_copied().ok_or_else(throw_eof_error)
    // }

    /// Get the current token in the iterator and advance it, if the current token is [`None`],
    /// return an EOF error.
    pub fn advance(&mut self) -> ParserResult<Token<'src>> {
        self.iter.advance().ok_or_else(throw_eof_error)
    }

    // /// Get the next token in the iterator, if the token is [`None`],
    // /// return an EOF error.
    // pub fn peek(&self) -> ParserResult<Token<'src>> {
    //     self.iter.peek_copied().ok_or_else(throw_eof_error)
    // }

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
        self.cont_expr_or_end(curr, end)

        // match curr.repr {
        //     TokenRepr::LParen => {
        //         let parens = self.parse_expr(TokenRepr::RParen)?;
        //         self.cont_expr_or_end(parens, end)
        //     }
        //     e if e == end => Err(throw_expected_expression(curr)),
        //     _ => todo!(),
        // }
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
            TokenRepr::Identifier => todo!(),
            TokenRepr::LParen => self.parse_expr(TokenRepr::RParen),
            TokenRepr::Minus if allow_unary => Ok(unary_expr(self.parse_value(false)?)),
            TokenRepr::Minus if !allow_unary => Err(throw_double_unary(current)),
            _ if allow_unary => Err(throw_unexpected_mult(current, EXPECTED)),
            _ => Err(throw_unexpected_mult(current, EXPECTED_NO_UNARY)),
        }
    }

    pub fn cont_expr_or_end(
        &mut self,
        start: Expr<'src>,
        end: TokenRepr,
    ) -> ParserResult<Expr<'src>> {
        let current = self.advance()?;

        match current.repr {
            TokenRepr::Mult | TokenRepr::Div => {
                let value = self.parse_value(true)?;
                self.cont_expr_or_end(binop_expr(start, current.repr, value), end)
            }

            TokenRepr::Plus | TokenRepr::Minus => {
                let next = self.parse_value(true)?;
                let next = self.cont_expr_or_end(next, end)?;
                Ok(binop_expr(start, current.repr, next))
            }

            e if e == end => Ok(start),

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
}

impl<'src, I: Iterator<Item = Token<'src>>> Iterator for Parser<'src, I> {
    type Item = ParserResult<Ast<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.iter.current_copied()?;
        let res = match current.repr {
            TokenRepr::Const => self.parse_const(),
            _ => todo!(),
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
