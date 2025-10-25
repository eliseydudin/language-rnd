use crate::{PeekIter, SourcePosition, Token, TokenRepr, WithPos, WithPosOrEof};
use bumpalo::{Bump, boxed::Box, collections::Vec, vec};
use core::{error, fmt};

#[derive(Debug, PartialEq)]
pub enum ErrorRepr<'b> {
    Eof,
    Unexpected {
        found: TokenRepr,
        expected: TokenRepr,
    },
    UnexpectedMult {
        found: TokenRepr,
        expected: Vec<'b, TokenRepr>,
    },
    DoubleUnary,
    UnexpectedParameterType,
}

#[derive(Debug, PartialEq)]
pub enum Type<'src, 'b> {
    /// ()
    Unit,
    /// (T, B, number)
    Tuple(Vec<'b, Self>),
    /// (<params>) => <returns>
    Function {
        params: Box<'b, Self>,
        returns: Box<'b, Self>,
    },
    // T
    Plain(&'src str),
}

impl fmt::Display for ErrorRepr<'_> {
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

impl error::Error for ErrorRepr<'_> {}

pub type ParserError<'b> = WithPosOrEof<ErrorRepr<'b>>;

pub const fn throw_eof_error<'b>() -> ParserError<'b> {
    WithPosOrEof::Eof(ErrorRepr::Eof)
}

pub const fn throw_unexpected_mult<'b>(
    found: Token,
    expected: Vec<'b, TokenRepr>,
) -> ParserError<'b> {
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

pub fn binop_expr<'a, 'b>(
    bump: &'b Bump,
    left: Expr<'a, 'b>,
    op: TokenRepr,
    right: Expr<'a, 'b>,
) -> Expr<'a, 'b> {
    Expr::BinOp {
        left: Box::new_in(left, bump),
        op,
        right: Box::new_in(right, bump),
    }
}

pub fn unary_expr<'a, 'b>(bump: &'b Bump, expr: Expr<'a, 'b>) -> Expr<'a, 'b> {
    Expr::Unary(Box::new_in(expr, bump))
}

#[derive(Debug, PartialEq)]
pub enum Expr<'src, 'b> {
    Number(&'src str),
    String(&'src str),
    Identifier(&'src str),
    BinOp {
        left: Box<'b, Self>,
        op: TokenRepr,
        right: Box<'b, Self>,
    },
    Unary(Box<'b, Self>),
    Access {
        object: Box<'b, Self>,
        property: &'src str,
    },
    Call {
        object: Box<'b, Self>,
        params: Box<'b, Self>,
        type_params: Option<Type<'src, 'b>>,
    },
    Tuple(Vec<'b, Self>),
    Unit, // same as rust's ()
}

impl<'a, 'b> From<Vec<'b, Expr<'a, 'b>>> for Expr<'a, 'b> {
    fn from(value: Vec<'b, Expr<'a, 'b>>) -> Self {
        Self::Tuple(value)
    }
}

type FunctionParams<'src, 'b> = Option<Vec<'b, Expr<'src, 'b>>>;
type FunctionBody<'src, 'b> = Vec<'b, Expr<'src, 'b>>;

#[derive(Debug, PartialEq)]
pub enum AstRepr<'src, 'b> {
    Const {
        name: &'src str,
        value: Expr<'src, 'b>,
    },
    Function {
        name: &'src str,
        type_params: Type<'src, 'b>,
        params: FunctionParams<'src, 'b>,
        body: FunctionBody<'src, 'b>,
    },
    FunctionPrototype {
        name: &'src str,
        type_params: Type<'src, 'b>,
        type_of: Type<'src, 'b>,
    },
}

pub type Ast<'a, 'b> = WithPos<AstRepr<'a, 'b>>;

pub struct Parser<'src, 'bump, I>
where
    I: Iterator<Item = Token<'src>>,
{
    iter: PeekIter<Token<'src>, I>,
    bump: &'bump Bump,
}

pub type ParserResult<'b, T> = Result<T, ParserError<'b>>;

impl<'src: 'bump, 'bump, I: Iterator<Item = Token<'src>>> Parser<'src, 'bump, I> {
    pub const fn new(iter: PeekIter<Token<'src>, I>, bump: &'bump Bump) -> Self {
        Self { iter, bump }
    }

    /// Get the current token in the iterator, if the current token is [`None`],
    /// return an EOF error.
    pub fn current(&self) -> ParserResult<'bump, Token<'src>> {
        self.iter.current_copied().ok_or_else(throw_eof_error)
    }

    /// Get the current token in the iterator and advance it, if the current token is [`None`],
    /// return an EOF error.
    pub fn advance(&mut self) -> ParserResult<'bump, Token<'src>> {
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
    pub fn peek(&self) -> ParserResult<'bump, Token<'src>> {
        self.iter.peek_copied().ok_or_else(throw_eof_error)
    }

    pub fn expect(&mut self, expect: TokenRepr) -> ParserResult<'bump, Token<'src>> {
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

    pub fn parse_const(&mut self) -> ParserResult<'bump, Ast<'src, 'bump>> {
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

    pub fn parse_expr(&mut self, end: TokenRepr) -> ParserResult<'bump, Expr<'src, 'bump>> {
        let curr = self.parse_value(true)?;
        self.cont_expr_or_end(curr, &[end]).map(|(_, e)| e)
    }

    pub fn parse_identifier_or_call(
        &mut self,
        start: Expr<'src, 'bump>,
        allow_call: bool,
        type_params: Option<Type<'src, 'bump>>,
    ) -> ParserResult<'bump, Expr<'src, 'bump>> {
        let next = self.current()?;

        if next.repr == TokenRepr::Dot {
            let property = self.peek()?;
            let access = Expr::Access {
                object: Box::new_in(start, self.bump),
                property: property.data,
            };

            self.advance()?;
            self.advance()?;

            self.parse_identifier_or_call(access, allow_call, type_params)
        } else if next.repr == TokenRepr::LParen && allow_call {
            self.advance()?;

            //let params = self.parse_expr(TokenRepr::RParen)?;
            let function_call = Expr::Call {
                object: Box::new_in(start, self.bump),
                params: Box::new_in(self.parse_tuple()?, self.bump),
                type_params,
            };

            Ok(function_call)
        } else if next.repr == TokenRepr::LAngle && type_params.is_none() {
            let type_params = self.parse_type_params()?;
            self.parse_identifier_or_call(start, allow_call, Some(type_params))
        } else {
            Ok(start)
        }
    }

    /// Parse one value in the token tree
    pub fn parse_value(&mut self, allow_unary: bool) -> ParserResult<'bump, Expr<'src, 'bump>> {
        let current = self.advance()?;

        match current.repr {
            TokenRepr::Number => Ok(Expr::Number(current.data)),
            TokenRepr::String => Ok(Expr::String(current.data)),
            TokenRepr::Identifier => {
                self.parse_identifier_or_call(Expr::Identifier(current.data), true, None)
            }
            TokenRepr::LParen => self.parse_tuple(),
            TokenRepr::Minus if allow_unary => Ok(unary_expr(self.bump, self.parse_value(false)?)),
            TokenRepr::Minus if !allow_unary => Err(throw_double_unary(current)),
            _ if allow_unary => Err(throw_unexpected_mult(
                current,
                vec![
                    in &self.bump;
                    TokenRepr::Number,
                    TokenRepr::String,
                    TokenRepr::Identifier,
                    TokenRepr::LParen,
                    TokenRepr::Minus
                ],
            )),
            _ => Err(throw_unexpected_mult(
                current,
                vec![
                    in &self.bump;
                    TokenRepr::Number,
                    TokenRepr::String,
                    TokenRepr::Identifier,
                    TokenRepr::LParen
                ],
            )),
        }
    }

    pub fn parse_tuple(&mut self) -> ParserResult<'bump, Expr<'src, 'bump>> {
        let mut result = Vec::new_in(self.bump);

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

    pub fn parse_tuple_with<T, F>(
        &mut self,
        func: F,
        endings: &[TokenRepr],
    ) -> ParserResult<'bump, Vec<'bump, T>>
    where
        F: Fn(Expr<'src, 'bump>, SourcePosition) -> ParserResult<'bump, T>,
    {
        if self
            .current()
            .map(|a| a.repr == TokenRepr::LParen)
            .unwrap_or(false)
        {
            self.advance()?;
        }

        let mut result = Vec::new_in(self.bump);
        let mut cont_expr_ends = Vec::from_iter_in(endings.into_iter().copied(), self.bump);
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
        start: Expr<'src, 'bump>,
        end: &[TokenRepr],
    ) -> ParserResult<'bump, (TokenRepr, Expr<'src, 'bump>)> {
        let current = self.advance()?;

        match current.repr {
            // TokenRepr::Mult | TokenRepr::Div => {
            //     let next = self.parse_value(true)?;
            //     let (end, next) = self.cont_expr_or_end(next, end)?;
            //     Ok((end, binop_expr(self.bump, start, current.repr, next)))
            // }
            TokenRepr::Plus | TokenRepr::Minus | TokenRepr::Mult | TokenRepr::Div => {
                let value = self.parse_value(true)?;
                self.cont_expr_or_end(binop_expr(self.bump, start, current.repr, value), end)
            }

            e if end.contains(&e) => Ok((e, start)),

            _ => {
                let res = vec![in &self.bump; TokenRepr::Mult, TokenRepr::Div, TokenRepr::Plus, TokenRepr::Minus];
                Err(throw_unexpected_mult(current, res))
            }
        }
    }

    pub fn parse_function_body(&mut self) -> ParserResult<'bump, FunctionBody<'src, 'bump>> {
        let mut result = Vec::new_in(self.bump);
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

    pub fn parse_function_params(&mut self) -> ParserResult<'bump, FunctionParams<'src, 'bump>> {
        macro_rules! insert {
            ($vec:ident, $data:expr) => {
                match &mut $vec {
                    None => {
                        let mut new = Vec::new_in(self.bump);
                        new.push($data);
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
                        vec![
                            in &self.bump;
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

    fn try_parse_type_tuple(
        &mut self,
        end: &[TokenRepr],
    ) -> ParserResult<'bump, Vec<'bump, Type<'src, 'bump>>> {
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

    pub fn parse_type_params(&mut self) -> ParserResult<'bump, Type<'src, 'bump>> {
        let curr = self.current()?;
        if curr.repr != TokenRepr::LAngle {
            return Ok(Type::Unit);
        } else {
            self.advance()?;
        }

        let result = self.try_parse_type_tuple(&[TokenRepr::RAngle])?;
        Ok(Type::Tuple(result))
    }

    fn parse_function_return(&mut self) -> ParserResult<'bump, Type<'src, 'bump>> {
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

    pub fn parse_function_type(&mut self) -> ParserResult<'bump, Type<'src, 'bump>> {
        self.advance()?;
        let params = self.try_parse_type_tuple(&[TokenRepr::RParen])?;
        let params = Box::new_in(
            if params.len() == 0 {
                Type::Unit
            } else {
                Type::Tuple(params)
            },
            self.bump,
        );

        self.expect(TokenRepr::Arrow)?;
        let returns = Box::new_in(self.parse_function_return()?, self.bump);
        self.expect(TokenRepr::Semicolon)?;

        Ok(Type::Function { params, returns })
    }

    pub fn parse_function(&mut self) -> ParserResult<'bump, Ast<'src, 'bump>> {
        let start = self.expect(TokenRepr::Fn)?;
        let name = self.expect(TokenRepr::Identifier)?.data;
        let type_params = self.parse_type_params()?;

        let curr = self.current()?;
        if curr.repr == TokenRepr::LParen {
            let type_of = self.parse_function_type()?;
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

impl<'src: 'bump, 'bump, I: Iterator<Item = Token<'src>>> Iterator for Parser<'src, 'bump, I> {
    type Item = ParserResult<'bump, Ast<'src, 'bump>>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = loop {
            let current = self.iter.current_copied()?;
            match current.repr {
                TokenRepr::Const => break self.parse_const(),
                TokenRepr::Fn => break self.parse_function(),
                TokenRepr::Comment => {
                    self.advance().ok()?;
                    continue;
                }
                _ => todo!("{current:?}"),
            };
        };

        Some(res)
    }
}

pub trait IntoParser<'s, 'b>: Iterator<Item = Token<'s>> + Sized {
    fn into_parser(self, bump: &'b Bump) -> Parser<'s, 'b, Self>;
}

impl<'s: 'b, 'b, I: Iterator<Item = Token<'s>>> IntoParser<'s, 'b> for I {
    fn into_parser(self, bump: &'b Bump) -> Parser<'s, 'b, Self> {
        Parser::new(PeekIter::new(self), bump)
    }
}

#[cfg(test)]
mod tests {
    use crate::{AstRepr, Expr, IntoParser, Type};
    use bumpalo::{Bump, boxed::Box, vec};

    macro_rules! parse_file {
        ($file:literal, $bump:ident) => {
            $crate::Lexer::new(include_str!($file))
                .map(|tok| tok.expect("invalid token"))
                .into_parser(&$bump)
                .map(|a| a.expect("invalid ast").inner)
        };
    }

    #[test]
    fn fib_test() {
        let bump = Bump::new();
        let mut ast = parse_file!("../test/fib.cofy", bump);

        let prototype = ast.next().expect("shouldnt fail");
        let impl_0 = ast.next().expect("shouldnt fail");
        let impl_1 = ast.next().expect("shouldnt fail");
        let generic = ast.next().expect("shouldnt fail");

        assert!(matches!(prototype, AstRepr::FunctionPrototype { .. }));
        assert!(matches!(impl_0, AstRepr::Function { name: "fib", .. }));
        assert!(matches!(impl_1, AstRepr::Function { name: "fib", .. }));
        assert!(matches!(generic, AstRepr::Function { name: "fib", .. }));
    }

    #[test]
    fn templates_test() {
        let bump = Bump::new();
        let mut ast = parse_file!("../test/template.cofy", bump);
        let proto = ast.next().expect("shouldnt fail");
        let generic = ast.next().expect("shouldnt fail");

        assert_eq!(
            proto,
            AstRepr::FunctionPrototype {
                name: "variant",
                type_params: Type::Tuple(vec![in &bump; Type::Plain("T")]),
                type_of: Type::Function {
                    params: Box::new_in(Type::Tuple(vec![in &bump; Type::Plain("T")]), &bump),
                    returns: Box::new_in(Type::Plain("T"), &bump)
                }
            }
        );

        assert_eq!(
            generic,
            AstRepr::Function {
                name: "variant",
                type_params: Type::Tuple(vec![in &bump; Type::Plain("T")]),
                params: Some(vec![in &bump; Expr::Identifier("t")]),
                body: vec![in &bump; Expr::Identifier("t")]
            }
        );
    }
}
