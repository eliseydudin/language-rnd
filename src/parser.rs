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
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
}

#[derive(Debug)]
pub enum Type<'src, 'bump> {
    // T
    Plain(&'src str),
    // [T]
    Iter(Box<'bump, Self>),
    // (params) => returns
    Function {
        params: Vec<'bump, Self>,
        returns: Box<'bump, Self>,
    },
    // (0, 1, 2)
    Tuple(Vec<'bump, Self>),
    // Ty<A, B, C>
    WithTypeParams(&'src str, &'bump [Type<'src, 'bump>]),
}

impl From<TokenRepr> for Operator {
    fn from(value: TokenRepr) -> Self {
        match value {
            TokenRepr::Mult => Operator::Mult,
            TokenRepr::Div => Operator::Div,
            TokenRepr::Plus => Operator::Plus,
            TokenRepr::Minus => Operator::Minus,
            TokenRepr::LAngle => Operator::Lt,
            TokenRepr::RAngle => Operator::Gt,
            TokenRepr::Le => Operator::Le,
            TokenRepr::Ge => Operator::Ge,
            TokenRepr::Equal => Operator::Eq,
            _ => panic!("invalid operator"),
        }
    }
}

#[derive(Debug)]
pub enum ExprInner<'src, 'bump> {
    Number(&'src str),
    Identifier(&'src str),
    String(&'src str),
    Pipe,
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
    If {
        condition: Box<'bump, Expr<'src, 'bump>>,
        main_body: Box<'bump, Expr<'src, 'bump>>,
        else_body: Option<Box<'bump, Expr<'src, 'bump>>>,
    },
    For {
        var: &'src str,
        container: Box<'bump, Expr<'src, 'bump>>,
        action: Box<'bump, Expr<'src, 'bump>>,
    },
    Tuple(Vec<'bump, Expr<'src, 'bump>>),
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

    pub fn string(pos: SourcePosition, data: &'src str) -> Self {
        Self {
            pos,
            inner: ExprInner::String(data),
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

    pub fn pipe(pos: SourcePosition) -> Self {
        Self {
            pos,
            inner: ExprInner::Pipe,
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

    pub fn if_expr(
        pos: SourcePosition,
        bump: &'bump Bump,
        condition: Self,
        main_body: Self,
        else_body: Option<Self>,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::If {
                condition: Box::new_in(condition, bump),
                main_body: Box::new_in(main_body, bump),
                else_body: else_body.map(|e| Box::new_in(e, bump)),
            },
        }
    }

    pub fn for_expr(
        pos: SourcePosition,
        bump: &'bump Bump,
        var: &'src str,
        container: Self,
        action: Self,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::For {
                var,
                container: Box::new_in(container, bump),
                action: Box::new_in(action, bump),
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
            self.current += 1;
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
            TokenRepr::Ge,
            TokenRepr::RAngle,
            TokenRepr::Le,
            TokenRepr::LAngle,
        ]) {
            self.current += 1;
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
            self.advance();
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
            self.advance();
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
            self.advance();
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
        self.advance().ok_or_else(|| self.eof_error())?;

        match tok.repr {
            TokenRepr::Number => Ok(Expr::number(tok.pos, tok.data)),
            TokenRepr::String => Ok(Expr::string(tok.pos, tok.data)),
            TokenRepr::Pipe => Ok(Expr::pipe(tok.pos)),
            TokenRepr::LParen => {
                self.current -= 1;
                let mut exp = self.parse_tuple()?;
                if exp.len() == 1 {
                    Ok(exp.remove(0))
                } else {
                    Ok(Expr {
                        pos: tok.pos,
                        inner: ExprInner::Tuple(exp),
                    })
                }
            }
            TokenRepr::Identifier => {
                let start = Expr::identifier(tok.pos, tok.data);
                if complex_ident {
                    self.current -= 1;
                    self.parse_identifier_or_call(start, None)
                } else {
                    Ok(start)
                }
            }
            TokenRepr::If => {
                self.current -= 1;
                self.parse_if_expr()
            }
            TokenRepr::For => {
                self.current -= 1;
                self.parse_for_expr()
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
        let tok = self.advance().ok_or_else(|| self.eof_error())?;
        match tok.repr {
            TokenRepr::Identifier => {
                if self.check(TokenRepr::LAngle) {
                    let type_params = self.parse_type_params()?;
                    Ok(Type::WithTypeParams(tok.data, type_params))
                } else {
                    Ok(Type::Plain(tok.data))
                }
            }
            TokenRepr::LParen => {
                self.current -= 1;
                let params = self.parse_type_tuple()?;

                if self.check(TokenRepr::Arrow) {
                    self.consume(TokenRepr::Arrow)?;
                    let returns = Box::new_in(self.parse_type()?, self.bump);
                    Ok(Type::Function { params, returns })
                } else {
                    Ok(Type::Tuple(params))
                }
            }
            TokenRepr::LBracket => {
                let inner = self.parse_type()?;
                self.consume(TokenRepr::RBracket)?;
                Ok(Type::Iter(Box::new_in(inner, self.bump)))
            }
            _ => Err(error!(tok, cow_str!(in self.bump; "unexpected token"))),
        }
    }

    fn parse_type_tuple(&mut self) -> ParserResult<'src, Vec<'bump, Type<'src, 'bump>>> {
        self.parse_tuple_with(
            TokenRepr::LParen,
            Self::parse_type,
            TokenRepr::Coma,
            TokenRepr::RParen,
        )
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
        self.parse_tuple_with(
            TokenRepr::LParen,
            Self::expression,
            TokenRepr::Coma,
            TokenRepr::RParen,
        )
    }

    fn parse_identifier_or_call(
        &mut self,
        start: Expr<'src, 'bump>,
        type_params: Option<&'bump [Type<'src, 'bump>]>,
    ) -> ParserResult<'src, Expr<'src, 'bump>> {
        self.advance().ok_or_else(|| self.eof_error())?;
        let next = self.peek().ok_or_else(|| self.eof_error())?;

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
            // very bad code but idk how to parse it in any other way
            let save = self.current;
            match self.parse_type_params() {
                Ok(data) => self.parse_identifier_or_call(start, Some(data)),
                Err(_) => {
                    self.current = save;
                    Ok(start)
                }
            }
        } else {
            Ok(start)
        }
    }

    fn parse_type_params(&mut self) -> ParserResult<'src, &'bump [Type<'src, 'bump>]> {
        self.parse_tuple_with(
            TokenRepr::LAngle,
            Self::parse_type,
            TokenRepr::Coma,
            TokenRepr::RAngle,
        )
        .map(|a| a.into_bump_slice())
    }

    fn parse_tuple_with<T, F>(
        &mut self,
        start: TokenRepr,
        mut elem_fn: F,
        delimeter: TokenRepr,
        end: TokenRepr,
    ) -> ParserResult<'src, Vec<'bump, T>>
    where
        F: FnMut(&mut Self) -> ParserResult<'src, T>,
    {
        let mut result = vec![in self.bump];
        self.consume(start)?;

        loop {
            if self.check(end) {
                self.consume(end)?;
                break;
            }

            let expr = elem_fn(self)?;
            result.push(expr);

            let next = self.advance().ok_or_else(|| self.eof_error())?;

            if next.repr == delimeter {
                continue;
            } else if next.repr == end {
                break;
            } else {
                return Err(error!(
                    next,
                    cow_str!(owned format!(in self.bump, "unexpected token of type {:?}", next.repr))
                ));
            }
        }

        Ok(result)
    }

    pub fn parse_function(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
        let begin = self.consume(TokenRepr::Fn)?;
        let name = self.consume(TokenRepr::Identifier)?;

        let type_parameters: &[Type<'src, 'bump>] = if self.check(TokenRepr::LAngle) {
            self.parse_type_params()?
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

            let with_type = if self.check(TokenRepr::With) {
                self.consume(TokenRepr::With)?;
                let with = Some(self.parse_type()?);
                self.consume(TokenRepr::Arrow)?;
                with
            } else {
                None
            };

            let body = self.parse_function_body()?;

            Ok(Ast {
                inner: AstInner::Function {
                    name: name.data,
                    params,
                    type_parameters,
                    body,
                    with_type,
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
                None => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn parse_if_expr(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let start = self.consume(TokenRepr::If)?;
        let condition = self.equality()?;

        self.consume(TokenRepr::Then)?;
        let main_body = self.expression()?;

        let else_body = if self.check(TokenRepr::Else) {
            self.consume(TokenRepr::Else)?;
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Expr::if_expr(
            start.pos, self.bump, condition, main_body, else_body,
        ))
    }

    fn parse_for_expr(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let start = self.consume(TokenRepr::For)?;
        let var = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::In)?;
        let container = self.expression()?;
        self.consume(TokenRepr::Do)?;
        let action = self.expression()?;

        Ok(Expr::for_expr(
            start.pos, self.bump, var.data, container, action,
        ))
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
        with_type: Option<Type<'src, 'bump>>,
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
