use crate::lexer::{SourcePosition, Token, TokenRepr};
use core::error::Error;
use core::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Div,
    Mult,
}

impl Into<Operator> for TokenRepr {
    fn into(self) -> Operator {
        match self {
            Self::Plus => Operator::Plus,
            Self::Minus => Operator::Minus,
            Self::Div => Operator::Div,
            Self::Mult => Operator::Mult,
            _ => panic!("invalid token repr, cannot be converted to an operator"),
        }
    }
}

#[derive(Debug)]
pub enum ErrorRepr {
    UnexpectedToken {
        expected: TokenRepr,
        found: TokenRepr,
    },
    UnexpectedMultiple {
        allowed: &'static [TokenRepr],
        found: TokenRepr,
    },
    ExpectedExpression {
        found: TokenRepr,
    },
    StrayToken {
        found: TokenRepr,
    },
    Eof,
}

#[derive(Debug)]
pub struct ParserError {
    repr: ErrorRepr,
    /// Is [`None`] when at EOF
    source_pos: Option<SourcePosition>,
}

impl ParserError {
    pub const fn unexpected_token(expected: TokenRepr, found: Token<'_>) -> Self {
        Self {
            repr: ErrorRepr::UnexpectedToken {
                expected: expected,
                found: found.repr,
            },
            source_pos: Some(found.pos),
        }
    }

    pub const fn eof() -> Self {
        Self {
            repr: ErrorRepr::Eof,
            source_pos: None,
        }
    }

    pub const fn unexpected_multiple(allowed: &'static [TokenRepr], found: Token<'_>) -> Self {
        Self {
            source_pos: Some(found.pos),
            repr: ErrorRepr::UnexpectedMultiple {
                allowed,
                found: found.repr,
            },
        }
    }

    pub const fn expected_expression(found: Token<'_>) -> Self {
        Self {
            source_pos: Some(found.pos),
            repr: ErrorRepr::ExpectedExpression { found: found.repr },
        }
    }

    pub const fn stray_token(found: Token<'_>) -> Self {
        Self {
            source_pos: Some(found.pos),
            repr: ErrorRepr::StrayToken { found: found.repr },
        }
    }
}

impl fmt::Display for ErrorRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "expected a token, found eof"),
            Self::UnexpectedToken { expected, found } => {
                write!(f, "expected {expected:?}, found {found:?}")
            }
            Self::UnexpectedMultiple { allowed, found } => {
                write!(f, "expected {{ ")?;
                for (i, r) in (*allowed).iter().enumerate() {
                    if i + 1 != allowed.len() {
                        write!(f, "{r:?} | ")?
                    } else {
                        write!(f, "{r:?} ")?
                    }
                }
                write!(f, "}} found {found:?}")
            }
            Self::ExpectedExpression { found } => {
                write!(f, "expected an expression, found {found:?}")
            }
            Self::StrayToken { found } => write!(f, "expected a statement, found {found:?}"),
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.source_pos {
            Some(s) => write!(f, "at {s} {}", self.repr),
            None => write!(f, "at EOF {}", self.repr),
        }
    }
}

impl Error for ParserError {}

#[derive(Debug, Clone)]
#[expect(dead_code)]
pub enum Expr<'src> {
    Number(&'src str),
    String(&'src str),
    BinOp {
        left: Box<Self>,
        right: Box<Self>,
        operator: Operator,
    },
    Unary {
        oper: Operator,
        inner: Box<Self>,
    },
}

impl<'src> Expr<'src> {
    pub fn from_token(token: Token<'src>) -> Self {
        match token.repr {
            TokenRepr::String => Self::String(token.data),
            TokenRepr::Number => Self::Number(token.data),
            _ => todo!(),
        }
    }

    pub fn binop(left: Self, operator: Operator, right: Self) -> Self {
        Self::BinOp {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstElement<'src> {
    Const { name: &'src str, value: Expr<'src> },
}

pub struct Parser<'src> {
    tokens: &'src [Token<'src>],
    position: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src [Token<'src>]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn advance(&mut self) -> Option<Token<'src>> {
        let n = self.tokens.get(self.position).copied();
        self.position += 1;
        n
    }

    fn advance_or_eof(&mut self) -> Result<Token<'src>, ParserError> {
        self.advance().ok_or_else(ParserError::eof)
    }

    fn current(&self) -> Option<Token<'src>> {
        self.tokens.get(self.position).copied()
    }

    fn current_or_eof(&self) -> Result<Token<'src>, ParserError> {
        self.current().ok_or_else(ParserError::eof)
    }

    // fn rewind(&mut self) {
    //     if self.position > 0 {
    //         self.position -= 1;
    //     }
    // }

    fn matches(&mut self, check: &[TokenRepr]) -> Result<&'src [Token<'src>], ParserError> {
        let start = self.position;
        for c in check {
            self.expect(*c)?;
        }

        Ok(&self.tokens[start..self.position])
    }

    fn expect(&mut self, ty: TokenRepr) -> Result<Token<'src>, ParserError> {
        let next = self.advance_or_eof()?;
        if next.repr == ty {
            Ok(next)
        } else {
            Err(ParserError::unexpected_token(ty, next))
        }
    }

    pub fn try_parse_mult_expression(
        &mut self,
        left: Expr<'src>,
        oper: Operator,
        ending: TokenRepr,
    ) -> Result<Expr<'src>, ParserError> {
        let next = self.advance_or_eof()?;
        match next.repr {
            TokenRepr::String | TokenRepr::Number => {
                let left = Expr::binop(left, oper, Expr::from_token(next));
                let new_next = self.current_or_eof()?;

                match new_next.repr {
                    TokenRepr::Mult | TokenRepr::Div => {
                        self.position += 1;
                        self.try_parse_mult_expression(left, new_next.repr.into(), ending)
                    }
                    TokenRepr::Plus | TokenRepr::Minus => {
                        self.position += 1;
                        Ok(Expr::binop(
                            left,
                            new_next.repr.into(),
                            self.try_parse_expr(ending)?,
                        ))
                    }
                    // found the end of the expression
                    e if e == ending => Ok(left),
                    _ => Err(ParserError::unexpected_multiple(
                        &[
                            TokenRepr::Mult,
                            TokenRepr::Div,
                            TokenRepr::Plus,
                            TokenRepr::Minus,
                        ],
                        new_next,
                    )),
                }
            }

            TokenRepr::LParen => {
                let expr = Expr::binop(left, oper, self.try_parse_paren_expr(next.repr)?);
                self.try_parse_chain(expr, ending)
            }

            _ => Err(ParserError::unexpected_multiple(
                &[TokenRepr::String, TokenRepr::Number],
                next,
            )),
        }
    }

    pub fn try_parse_paren_expr(&mut self, start: TokenRepr) -> Result<Expr<'src>, ParserError> {
        let end = match start {
            TokenRepr::LParen => TokenRepr::RParen,
            TokenRepr::LBracket => TokenRepr::RBracket,
            TokenRepr::LAngle => TokenRepr::RAngle,
            TokenRepr::LFigure => TokenRepr::RFigure,
            _ => panic!(),
        };

        let res = self.try_parse_expr(end);
        self.position += 1;
        res
    }

    fn try_parse_chain(
        &mut self,
        start: Expr<'src>,
        ending: TokenRepr,
    ) -> Result<Expr<'src>, ParserError> {
        let next = self.current_or_eof()?;

        match next.repr {
            TokenRepr::Plus | TokenRepr::Minus => {
                self.position += 1;
                Ok(Expr::binop(
                    start,
                    next.repr.into(),
                    self.try_parse_expr(ending)?,
                ))
            }

            TokenRepr::Div | TokenRepr::Mult => {
                self.position += 1;
                self.try_parse_mult_expression(start, next.repr.into(), ending)
            }

            e if e == ending => Ok(start),
            _ => Err(ParserError::unexpected_multiple(
                &[
                    TokenRepr::Plus,
                    TokenRepr::Minus,
                    TokenRepr::Div,
                    TokenRepr::Mult,
                ],
                next,
            )),
        }
    }

    /// Parse a single value: number, string, identifier, function call, tuple,
    /// an expression in parenthesis.
    /// Doesn't parse unary expressions, use `try_parse_expr` for that
    pub fn try_parse_value(&mut self) -> Result<Expr<'src>, ParserError> {
        let curr = self.advance_or_eof()?;
        match curr.repr {
            TokenRepr::Number => Ok(Expr::Number(curr.data)),
            TokenRepr::String => Ok(Expr::String(curr.data)),
            TokenRepr::LParen => self.try_parse_paren_expr(curr.repr),
            _ => todo!(),
        }
    }

    /// `ending` is the [`TokenRepr`] you wanna stop at, e.g. if you are parsing
    /// a `const` it should be `;`, if you are parsing an object it should be `,`
    pub fn try_parse_expr(&mut self, ending: TokenRepr) -> Result<Expr<'src>, ParserError> {
        let tok = self.advance_or_eof()?;

        match tok.repr {
            TokenRepr::String | TokenRepr::Number => {
                let tok = Expr::from_token(tok);
                self.try_parse_chain(tok, ending)
            }

            TokenRepr::LParen => {
                let res = self.try_parse_paren_expr(tok.repr)?;
                self.try_parse_chain(res, ending)
            }

            // try an unary expression
            TokenRepr::Minus | TokenRepr::Plus => {
                let val = Expr::Unary {
                    oper: tok.repr.into(),
                    inner: Box::new(self.try_parse_value()?),
                };

                self.try_parse_chain(val, ending)
            }

            e if e == ending => {
                // if we get an ending then it means that an expression is expected
                Err(ParserError::expected_expression(tok))
            }
            _ => todo!("{tok:?}"),
        }

        //Ok(Expr::Number(tok.data))
    }

    pub fn try_parse_const(&mut self) -> Result<AstElement<'src>, ParserError> {
        use TokenRepr::{Const, Identifier, Semicolon, Set};

        let tokens = self.matches(&[Const, Identifier, Set])?;
        let value = self.try_parse_expr(Semicolon)?;
        self.expect(Semicolon)?;

        Ok(AstElement::Const {
            name: tokens[1].data,
            value,
        })
    }
}

impl<'src> Iterator for Parser<'src> {
    type Item = Result<AstElement<'src>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.current()?;
        let res = match curr.repr {
            TokenRepr::Const => self.try_parse_const(),
            _ => Err(ParserError::stray_token(curr)),
        };

        if res.is_err() {
            self.position += 1;
        }

        Some(res)
    }
}
