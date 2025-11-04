use crate::{CowStr, SourcePosition, Token, TokenRepr, cow_str};
use bumpalo::{Bump, boxed::Box, collections::Vec, format, vec};
use core::{error, fmt};

pub struct Parser<'src, 'bump> {
    bump: &'bump Bump,
    token_stream: &'bump [Token<'src>],
    current: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Mult,
    Div,
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum Type<'src, 'bump> {
    Plain(&'src str),
    Function {
        params: Vec<'bump, Self>,
        returns: Box<'bump, Self>,
    },
}

impl From<TokenRepr> for Operator {
    fn from(value: TokenRepr) -> Self {
        match value {
            TokenRepr::Mult => Operator::Mult,
            TokenRepr::Div => Operator::Div,
            TokenRepr::Plus => Operator::Plus,
            TokenRepr::Minus => Operator::Minus,
            _ => panic!("invalid operator"),
        }
    }
}

#[derive(Debug)]
pub enum ExprInner<'src, 'bump> {
    Number(&'src str),
    Identifier(&'src str),
    BinOp {
        left: Box<'bump, Expr<'src, 'bump>>,
        operator: Operator,
        right: Box<'bump, Expr<'src, 'bump>>,
    },
    Unary {
        operator: Operator,
        data: Box<'bump, Expr<'src, 'bump>>,
    },
    Access {
        object: Box<'bump, Expr<'src, 'bump>>,
        property: &'src str,
    },
    Call {
        object: Box<'bump, Expr<'src, 'bump>>,
        params: Vec<'bump, Expr<'src, 'bump>>,
    },
}

#[derive(Debug)]
pub struct Expr<'src, 'bump> {
    inner: ExprInner<'src, 'bump>,
    pos: SourcePosition,
}

impl<'src, 'bump> Expr<'src, 'bump> {
    pub fn binop(
        at: SourcePosition,
        bump: &'bump Bump,
        left: Self,
        operator: Operator,
        right: Self,
    ) -> Self {
        Self {
            pos: at,
            inner: ExprInner::BinOp {
                left: Box::new_in(left, bump),
                operator,
                right: Box::new_in(right, bump),
            },
        }
    }

    pub fn unary(
        pos: SourcePosition,
        bump: &'bump Bump,
        operator: Operator,
        data: Expr<'src, 'bump>,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::Unary {
                operator,
                data: Box::new_in(data, bump),
            },
        }
    }

    pub fn number(pos: SourcePosition, data: &'src str) -> Self {
        Self {
            pos,
            inner: ExprInner::Number(data),
        }
    }

    pub fn identifier(pos: SourcePosition, data: &'src str) -> Self {
        Self {
            pos,
            inner: ExprInner::Identifier(data),
        }
    }

    pub fn access(
        pos: SourcePosition,
        bump: &'bump Bump,
        object: Self,
        property: &'src str,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::Access {
                object: Box::new_in(object, bump),
                property,
            },
        }
    }

    pub fn call(
        pos: SourcePosition,
        bump: &'bump Bump,
        object: Self,
        params: Vec<'bump, Self>,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::Call {
                object: Box::new_in(object, bump),
                params,
            },
        }
    }

    pub fn into_inner(self) -> ExprInner<'src, 'bump> {
        self.inner
    }

    pub const fn inner(&self) -> &ExprInner<'src, 'bump> {
        &self.inner
    }
}

#[derive(Debug, Clone)]
pub struct ParserError<'src> {
    message: CowStr<'src, 'src>,
    position: SourcePosition,
}

impl fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at {}: {}", self.position, self.message)
    }
}

impl error::Error for ParserError<'_> {}

pub type ParserResult<'s, T> = Result<T, ParserError<'s>>;

macro_rules! error {
    ($where:expr, $data:expr) => {
        $crate::parser::ParserError {
            message: $data.into(),
            position: $where.pos,
        }
    };
}

impl<'src, 'bump: 'src> Parser<'src, 'bump> {
    pub fn new(bump: &'bump Bump, token_stream: &'bump [Token<'src>]) -> Self {
        Self {
            bump,
            token_stream,
            current: 0,
        }
    }

    pub fn expression(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        self.equality()
    }

    pub fn equality(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.comparison()?;

        while self.match_next(&[TokenRepr::Equal]) {
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.comparison()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn previous(&self) -> Option<Token<'src>> {
        self.token_stream.get(self.current - 1).copied()
    }

    pub fn peek(&self) -> Option<Token<'src>> {
        self.token_stream.get(self.current).copied()
    }

    pub fn eof(&self) -> bool {
        self.peek().is_none()
    }

    pub fn match_next(&mut self, reprs: &[TokenRepr]) -> bool {
        for r in reprs {
            if self.check(*r) {
                self.advance();
                return true;
            }
        }

        false
    }

    pub fn check(&self, repr: TokenRepr) -> bool {
        match self.peek() {
            Some(n) if n.repr == repr => true,
            _ => false,
        }
    }

    pub fn advance(&mut self) -> Option<Token<'src>> {
        if !self.eof() {
            self.current += 1;
        }

        self.previous()
    }

    pub fn comparison(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.term()?;

        while self.match_next(&[
            TokenRepr::Gt,
            TokenRepr::RAngle,
            TokenRepr::Lt,
            TokenRepr::LAngle,
        ]) {
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.term()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn term(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.factor()?;

        while self.match_next(&[TokenRepr::Minus, TokenRepr::Plus]) {
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.factor()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn factor(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.unary()?;

        while self.match_next(&[TokenRepr::Div, TokenRepr::Mult]) {
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.unary()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        if self.match_next(&[TokenRepr::Minus]) {
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.unary()?;
            return Ok(Expr::unary(
                operator.pos,
                self.bump,
                operator.repr.into(),
                right,
            ));
        }

        self.primary(true)
    }

    pub fn consume(&mut self, tok: TokenRepr) -> ParserResult<'src, Token<'src>> {
        if self.check(tok) {
            Ok(self
                .advance()
                .expect("shouldn't be None since self.check is true"))
        } else {
            let last = self.previous().unwrap();
            Err(error!(
                last,
                format!(in self.bump, "expected a {:?}, found a {:?}", tok, last.repr)
            ))
        }
    }

    pub fn eof_error(&self) -> ParserError<'src> {
        error!(
            self.previous().unwrap(),
            cow_str!(in self.bump; "expected a token, found eof")
        )
    }

    pub fn primary(&mut self, complex_ident: bool) -> ParserResult<'src, Expr<'src, 'bump>> {
        let tok = self.peek().ok_or_else(|| self.eof_error())?;

        match tok.repr {
            TokenRepr::Number => {
                self.advance().ok_or_else(|| self.eof_error())?;
                Ok(Expr::number(tok.pos, tok.data))
            }
            TokenRepr::LParen => {
                self.advance().ok_or_else(|| self.eof_error())?;
                let exp = self.expression()?;
                self.consume(TokenRepr::RParen)?;
                Ok(exp)
            }
            TokenRepr::Identifier => {
                let start = Expr::identifier(tok.pos, tok.data);
                if complex_ident {
                    self.parse_identifier_or_call(start, None)
                } else {
                    self.advance().ok_or_else(|| self.eof_error())?;
                    Ok(start)
                }
            }
            _ => todo!("on {:?}", tok.repr),
        }
    }

    pub fn parse_const(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
        let begin = self.consume(TokenRepr::Const)?;
        let name = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::Set)?;
        let value = self.expression()?;
        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            inner: AstInner::Const {
                name: name.data,
                value,
            },
            pos: begin.pos,
        })
    }

    fn parse_type(&mut self) -> ParserResult<'src, Type<'src, 'bump>> {
        // TODO, add support for more complex types
        let res = self.consume(TokenRepr::Identifier)?;
        Ok(Type::Plain(res.data))
    }

    fn parse_type_tuple(&mut self) -> ParserResult<'src, Vec<'bump, Type<'src, 'bump>>> {
        let mut result = Vec::new_in(self.bump);
        self.consume(TokenRepr::LParen)?;
        let res = loop {
            let tp = self.parse_type()?;
            result.push(tp);

            let curr = self.peek().ok_or_else(|| self.eof_error())?;
            match curr.repr {
                TokenRepr::RParen => break Ok(result),
                TokenRepr::Coma => {
                    self.advance();
                    continue;
                }
                _ => {
                    return Err(error!(
                        curr,
                        cow_str!(owned format!(in self.bump, "unexpected token {:?}", curr))
                    ));
                }
            }
        };

        self.consume(TokenRepr::RParen)?;
        res
    }

    fn parse_function_type(&mut self) -> ParserResult<'src, Type<'src, 'bump>> {
        let params = self.parse_type_tuple()?;
        self.consume(TokenRepr::Arrow)?;
        let returns = self.parse_type()?;

        Ok(Type::Function {
            params,
            returns: Box::new_in(returns, self.bump),
        })
    }

    fn parse_function_params(&mut self) -> ParserResult<'src, Vec<'bump, Expr<'src, 'bump>>> {
        let mut result = vec![in self.bump];

        while self.peek().ok_or_else(|| self.eof_error())?.repr != TokenRepr::Set {
            result.push(self.primary(false)?)
        }

        Ok(result)
    }

    fn parse_function_body(&mut self) -> ParserResult<'src, Vec<'bump, Expr<'src, 'bump>>> {
        let mut result = vec![in self.bump];

        loop {
            let expr = self.expression()?;
            result.push(expr);

            let next = self.advance().ok_or_else(|| self.eof_error())?;
            match next.repr {
                TokenRepr::Arrow => continue,
                TokenRepr::Semicolon => break,
                _ => {
                    return Err(error!(
                        next,
                        cow_str!(owned format!(in self.bump, "unexpected token of type {:?}", next.repr))
                    ));
                }
            }
        }

        Ok(result)
    }

    fn parse_tuple(&mut self) -> ParserResult<'src, Vec<'bump, Expr<'src, 'bump>>> {
        self.consume(TokenRepr::LParen)?;
        let mut result = vec![in self.bump];

        loop {
            let expr = self.expression()?;
            result.push(expr);

            let next = self.advance().ok_or_else(|| self.eof_error())?;
            match next.repr {
                TokenRepr::Coma => continue,
                TokenRepr::RParen => break,
                _ => {
                    return Err(error!(
                        next,
                        cow_str!(owned format!(in self.bump, "unexpected token of type {:?}", next.repr))
                    ));
                }
            }
        }

        Ok(result)
    }

    fn parse_identifier_or_call(
        &mut self,
        start: Expr<'src, 'bump>,
        type_params: Option<&'bump [Type<'src, 'bump>]>,
    ) -> ParserResult<'src, Expr<'src, 'bump>> {
        self.advance().ok_or_else(|| self.eof_error())?;
        let next = self
            .peek()
            .expect("if advance doesn't fail then peek() doesn't return a None");

        if next.repr == TokenRepr::Dot {
            let property = self.peek().ok_or_else(|| self.eof_error())?;
            let access = Expr::access(start.pos, self.bump, start, property.data);

            self.advance().ok_or_else(|| self.eof_error())?;
            self.advance().ok_or_else(|| self.eof_error())?;

            self.parse_identifier_or_call(access, type_params)
        } else if next.repr == TokenRepr::LParen {
            let params = self.parse_tuple()?;

            let function_call = Expr::call(start.pos, self.bump, start, params);
            Ok(function_call)
        } else if next.repr == TokenRepr::LAngle && type_params.is_none() {
            let type_params = self.parse_type_params()?;
            self.parse_identifier_or_call(start, Some(type_params))
        } else {
            Ok(start)
        }
    }

    fn parse_type_params(&mut self) -> ParserResult<'src, &'bump [Type<'src, 'bump>]> {
        todo!()
    }

    pub fn parse_function(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
        let begin = self.consume(TokenRepr::Fn)?;
        let name = self.consume(TokenRepr::Identifier)?;

        let type_parameters: &[Type<'src, 'bump>] = if self.check(TokenRepr::LAngle) {
            todo!()
        } else {
            &[]
        };

        if self.check(TokenRepr::LParen) {
            let type_of = self.parse_function_type()?;
            self.consume(TokenRepr::Semicolon)?;
            Ok(Ast {
                inner: AstInner::FunctionPrototype {
                    name: name.data,
                    type_of,
                    type_parameters,
                },
                pos: begin.pos,
            })
        } else {
            let params = self.parse_function_params()?;
            self.consume(TokenRepr::Set)?;
            let body = self.parse_function_body()?;

            Ok(Ast {
                inner: AstInner::Function {
                    name: name.data,
                    params,
                    type_parameters,
                    body,
                },
                pos: begin.pos,
            })
        }
    }

    pub fn synchronize(&mut self) {
        const STATEMENT_STARTS: &[TokenRepr] = &[TokenRepr::Const, TokenRepr::Fn];

        loop {
            match self.peek() {
                Some(tok) if STATEMENT_STARTS.contains(&tok.repr) => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum AstInner<'src, 'bump> {
    Const {
        name: &'src str,
        value: Expr<'src, 'bump>,
    },
    FunctionPrototype {
        name: &'src str,
        type_of: Type<'src, 'bump>,
        type_parameters: &'bump [Type<'src, 'bump>],
    },
    Function {
        name: &'src str,
        params: Vec<'bump, Expr<'src, 'bump>>,
        body: Vec<'bump, Expr<'src, 'bump>>,
        type_parameters: &'bump [Type<'src, 'bump>],
    },
}

#[derive(Debug)]
pub struct Ast<'src, 'bump> {
    pub inner: AstInner<'src, 'bump>,
    pub pos: SourcePosition,
}

impl<'src, 'bump: 'src> Iterator for Parser<'src, 'bump> {
    type Item = ParserResult<'src, Ast<'src, 'bump>>;

    fn next(&mut self) -> Option<Self::Item> {
        let prev = self.peek()?;
        match prev.repr {
            TokenRepr::Const => Some(self.parse_const()),
            TokenRepr::Fn => Some(self.parse_function()),
            _ => {
                let error = error!(
                    prev,
                    cow_str!(owned format!(in self.bump, "unrecognized statement start {:?}", prev))
                );

                self.synchronize();
                Some(Err(error))
            }
        }
    }
}
