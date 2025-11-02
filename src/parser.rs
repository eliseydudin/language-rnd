use crate::{CowStr, SourcePosition, Token, TokenRepr, cow_str};
use bumpalo::{Bump, boxed::Box, format};
use core::{error, fmt};

pub struct Parser<'src, 'bump> {
    bump: &'bump Bump,
    token_stream: &'bump [Token<'src>],
    current: usize,
}

#[derive(Debug)]
pub enum Operator {
    Mult,
    Div,
    Plus,
    Minus,
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
    BinOp {
        left: Box<'bump, Expr<'src, 'bump>>,
        operator: Operator,
        right: Box<'bump, Expr<'src, 'bump>>,
    },
    Unary {
        operator: Operator,
        data: Box<'bump, Expr<'src, 'bump>>,
    },
}

#[expect(unused)]
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

        self.primary()
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

    pub fn primary(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let tok = self.peek().ok_or_else(|| self.eof_error())?;
        self.advance().ok_or_else(|| self.eof_error())?;

        match tok.repr {
            TokenRepr::Number => Ok(Expr::number(tok.pos, tok.data)),
            TokenRepr::LParen => {
                let exp = self.expression()?;
                self.consume(TokenRepr::RParen)?;
                Ok(exp)
            }
            _ => todo!(),
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
}

#[derive(Debug)]
pub enum AstInner<'src, 'bump> {
    Const {
        name: &'src str,
        value: Expr<'src, 'bump>,
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
            _ => None,
        }
    }
}
