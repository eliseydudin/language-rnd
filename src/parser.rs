use crate::{
    lexer::{SourcePosition, Token, TokenRepr, WithPos, WithPosOrEof},
    peek_iter::PeekIter,
};
use arrayvec::ArrayVec;
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
        expected: ArrayVec<TokenRepr, 16>,
    },
    DoubleUnary,
    UnexpectedParameterType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type<'src> {
    /// ()
    Unit,
    /// (T, B, number)
    Tuple(Vec<Type<'src>>),
    /// (<params>) => <returns>
    Function {
        params: Box<Type<'src>>,
        returns: Box<Type<'src>>,
    },
    // T
    Plain(&'src str),
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
            Self::UnexpectedParameterType => write!(
                f,
                "while parsing an expression of type <T, A, B...> found an expression that isnt an identifier"
            ),
        }
    }
}

impl error::Error for ErrorRepr {}

pub type ParserError = WithPosOrEof<ErrorRepr>;

pub const fn throw_eof_error() -> ParserError {
    WithPosOrEof::Eof(ErrorRepr::Eof)
}

pub fn throw_unexpected_mult(found: Token, expected_toks: impl AsRef<[TokenRepr]>) -> ParserError {
    let mut expected = ArrayVec::new();
    for tok in expected_toks.as_ref() {
        expected.push(*tok)
    }

    throw_unexpected_mult_array(found, expected)
}

pub const fn throw_unexpected_mult_array(
    found: Token,
    expected: ArrayVec<TokenRepr, 16>,
) -> ParserError {
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
        type_params: Option<Type<'src>>,
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

#[derive(Debug, Clone)]
#[expect(dead_code, reason = "ast repr isnt used currently")]
pub enum AstRepr<'src> {
    Const {
        name: &'src str,
        value: Expr<'src>,
    },
    Function {
        name: &'src str,
        type_params: Option<Type<'src>>,
        params: FunctionParams<'src>,
        body: FunctionBody<'src>,
    },
    FunctionPrototype {
        name: &'src str,
        type_params: Option<Type<'src>>,
        type_of: Type<'src>,
    },
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
        while let Some(tok) = self.iter.advance() {
            match tok.repr {
                TokenRepr::Comment => continue,
                _ => return Ok(tok),
            }
        }

        Err(throw_eof_error())
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
        type_params: Option<Type<'src>>,
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

            self.parse_identifier_or_call(access, allow_call, type_params)
        } else if next.repr == TokenRepr::LParen && allow_call {
            self.advance()?;

            //let params = self.parse_expr(TokenRepr::RParen)?;
            let function_call = Expr::Call {
                object: Box::new(start),
                params: Box::new(self.parse_tuple()?),
                type_params,
            };

            Ok(function_call)
        } else if next.repr == TokenRepr::LAngle && type_params.is_none() {
            let type_params = self.try_parse_type_params()?;
            self.parse_identifier_or_call(start, allow_call, type_params)
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
                self.parse_identifier_or_call(value, true, None)
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

    pub fn parse_tuple_with<T, F>(&mut self, func: F, endings: &[TokenRepr]) -> ParserResult<Vec<T>>
    where
        F: Fn(Expr<'src>, SourcePosition) -> ParserResult<T>,
    {
        if self
            .current()
            .map(|a| a.repr == TokenRepr::LParen)
            .unwrap_or(false)
        {
            self.advance()?;
        }

        let mut result = vec![];
        let mut cont_expr_ends: ArrayVec<TokenRepr, 16> =
            ArrayVec::from_iter(endings.into_iter().map(|a| *a));
        cont_expr_ends.push(TokenRepr::Coma);

        loop {
            if self.current().map(|a| endings.contains(&a.repr)) == Ok(true) {
                self.advance()?;
                return Ok(result);
            }

            let val = self.parse_value(true)?;

            let (e, expr) = self.cont_expr_or_end(val, &cont_expr_ends)?;
            let pos = self.current()?.pos;

            result.push(func(expr, pos)?);

            if endings.contains(&e) {
                break;
            }
        }

        Ok(result)
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

            _ => {
                let mut res = ArrayVec::new();
                res.push(TokenRepr::Mult);
                res.push(TokenRepr::Div);
                res.push(TokenRepr::Plus);
                res.push(TokenRepr::Minus);

                for r in end {
                    res.push(*r);
                }

                Err(throw_unexpected_mult_array(current, res))
            }
        }
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
            let next = self.advance()?;
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

    fn try_parse_type_tuple(&mut self, end: &[TokenRepr]) -> ParserResult<Vec<Type<'src>>> {
        self.parse_tuple_with(
            |expr, pos| {
                if let Expr::Identifier(name) = expr {
                    Ok(Type::Plain(name))
                } else {
                    Err(ParserError::Pos(WithPos::new(
                        ErrorRepr::UnexpectedParameterType,
                        pos,
                    )))
                }
            },
            end,
        )
    }

    pub fn try_parse_type_params(&mut self) -> ParserResult<Option<Type<'src>>> {
        let curr = self.current()?;
        if curr.repr != TokenRepr::LAngle {
            return Ok(None);
        } else {
            self.advance()?;
        }

        let result = self.try_parse_type_tuple(&[TokenRepr::RAngle])?;
        Ok(Some(Type::Tuple(result)))
    }

    fn try_parse_function_return(&mut self) -> ParserResult<Type<'src>> {
        if self.current()?.repr == TokenRepr::LParen {
            Ok(Type::Tuple(self.try_parse_type_tuple(&[
                TokenRepr::RParen,
                TokenRepr::Semicolon,
            ])?))
        } else {
            let ty = self.expect(TokenRepr::Identifier)?.data;
            Ok(Type::Plain(ty))
        }
    }

    pub fn try_parse_function_type(&mut self) -> ParserResult<Type<'src>> {
        self.advance()?;
        let params = self.try_parse_type_tuple(&[TokenRepr::RParen])?;
        let params = Box::new(if params.len() == 0 {
            Type::Unit
        } else {
            Type::Tuple(params)
        });

        self.expect(TokenRepr::Arrow)?;
        let returns = Box::new(self.try_parse_function_return()?);
        self.expect(TokenRepr::Semicolon)?;

        Ok(Type::Function { params, returns })
    }

    pub fn parse_function(&mut self) -> ParserResult<Ast<'src>> {
        let start = self.expect(TokenRepr::Fn)?;
        let name = self.expect(TokenRepr::Identifier)?.data;
        let type_params = self.try_parse_type_params()?;

        let curr = self.current()?;
        if curr.repr == TokenRepr::LParen {
            let type_of = self.try_parse_function_type()?;
            Ok(Ast::new(
                AstRepr::FunctionPrototype {
                    name,
                    type_params,
                    type_of,
                },
                start.pos,
            ))
        } else {
            let params = self.parse_function_params()?;
            // if we returned from `self.parse_function_params`
            // it means that a `Set` was already found
            let body = self.parse_function_body()?;

            Ok(Ast::new(
                AstRepr::Function {
                    name,
                    params,
                    body,
                    type_params,
                },
                start.pos,
            ))
        }
    }
}

impl<'src, I: Iterator<Item = Token<'src>>> Iterator for Parser<'src, I> {
    type Item = ParserResult<Ast<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = loop {
            let current = self.iter.current_copied()?;
            match current.repr {
                TokenRepr::Const => break self.parse_const(),
                TokenRepr::Fn => break self.parse_function(),
                TokenRepr::Comment => {
                    let _ = self.advance();
                    continue;
                }
                _ => todo!("{current:?}"),
            };
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
