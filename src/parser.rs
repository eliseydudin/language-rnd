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
    Other(&'static str),
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

    pub const fn not_implemented(tok: Token<'_>) -> Self {
        Self {
            source_pos: Some(tok.pos),
            repr: ErrorRepr::Other("not yet implemented"),
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
            Self::Other(s) => write!(f, "{s}"),
            Self::UnexpectedMultiple { allowed, found } => {
                write!(f, "expected ")?;
                for (i, r) in (*allowed).iter().enumerate() {
                    if i + 1 != allowed.len() {
                        write!(f, "{r:?} | ")?
                    } else {
                        write!(f, "{r:?} ")?
                    }
                }
                write!(f, "found {found:?}")
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
pub enum Expr<'src> {
    Number(&'src str),
    String(&'src str),
    BinOp {
        left: Box<Self>,
        right: Box<Self>,
        operator: Operator,
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

    fn expect_sev(&mut self, tys: &'static [TokenRepr]) -> Result<Token<'src>, ParserError> {
        let next = self.advance_or_eof()?;
        for ty in tys {
            if next.repr == *ty {
                return Ok(next);
            }
        }

        Err(ParserError::unexpected_multiple(tys, next))
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
            _ => Err(ParserError::unexpected_multiple(
                &[TokenRepr::String, TokenRepr::Number],
                next,
            )),
        }
    }

    /// `ending` is the [`TokenRepr`] you wanna stop at, e.g. if you are parsing
    /// a `const` it should be `;`, if you are parsing an object it should be `,`
    pub fn try_parse_expr(&mut self, ending: TokenRepr) -> Result<Expr<'src>, ParserError> {
        let tok = self.advance_or_eof()?;

        match tok.repr {
            TokenRepr::String | TokenRepr::Number => {
                let tok = Expr::from_token(tok);
                let next = self.current_or_eof()?;

                return match next.repr {
                    TokenRepr::Plus | TokenRepr::Minus => {
                        self.position += 1;
                        Ok(Expr::binop(
                            tok,
                            next.repr.into(),
                            self.try_parse_expr(ending)?,
                        ))
                    }
                    TokenRepr::Div | TokenRepr::Mult => {
                        self.position += 1;
                        self.try_parse_mult_expression(tok, next.repr.into(), ending)
                    }

                    e if e == ending => Ok(tok),
                    _ => Err(ParserError::unexpected_multiple(
                        &[
                            TokenRepr::Plus,
                            TokenRepr::Minus,
                            TokenRepr::Div,
                            TokenRepr::Mult,
                        ],
                        next,
                    )),
                };
            }

            e if e == ending => {
                // if we get an ending then it means that an expression is expected
                return Err(ParserError::expected_expression(tok));
            }
            _ => todo!("{tok:?}"),
        };

        //Ok(Expr::Number(tok.data))
    }

    pub fn try_parse_const(&mut self) -> Result<AstElement<'src>, ParserError> {
        use TokenRepr::{Const, Identifier, Semicolon, Set};

        let tokens = self.matches(&[Const, Identifier, Set])?;
        let name = tokens[1].data;
        let value = self.try_parse_expr(TokenRepr::Semicolon)?;
        self.expect(Semicolon)?;

        Ok(AstElement::Const { name, value })
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
