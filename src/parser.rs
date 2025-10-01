use crate::{
    lexer::{SourcePosition, Token, TokenRepr},
    peek_iter::PeekIter,
};

#[derive(Debug)]
pub enum ErrorRepr {
    Eof,
    Unexpected {
        found: TokenRepr,
        expected: TokenRepr,
    },
}

#[derive(Debug)]
pub struct ParserError {
    repr: ErrorRepr,
    pos: Option<SourcePosition>,
}

impl ParserError {
    pub const fn eof() -> Self {
        Self {
            repr: ErrorRepr::Eof,
            pos: None,
        }
    }

    pub const fn unexpected(found: Token, expected: TokenRepr) -> Self {
        Self {
            pos: Some(found.pos),
            repr: ErrorRepr::Unexpected {
                found: found.repr,
                expected,
            },
        }
    }
}

#[derive(Debug)]
pub enum Expr<'src> {
    Number(&'src str),
    String(&'src str),
}

#[derive(Debug)]
pub enum AstRepr<'src> {
    Const { name: &'src str, value: Expr<'src> },
}

#[derive(Debug)]
pub struct AstElement<'src> {
    repr: AstRepr<'src>,
    pos: SourcePosition,
}

impl<'src> AstElement<'src> {
    pub fn new_const(start: SourcePosition, name: &'src str, value: Expr<'src>) -> Self {
        Self {
            repr: AstRepr::Const { name, value },
            pos: start,
        }
    }
}

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
        self.iter.current_copied().ok_or_else(ParserError::eof)
    }

    /// Get the current token in the iterator and advance it, if the current token is [`None`],
    /// return an EOF error.
    pub fn advance(&mut self) -> ParserResult<Token<'src>> {
        self.iter.advance().ok_or_else(ParserError::eof)
    }

    /// Get the next token in the iterator, if the token is [`None`],
    /// return an EOF error.
    pub fn peek(&self) -> ParserResult<Token<'src>> {
        self.iter.peek_copied().ok_or_else(ParserError::eof)
    }

    pub fn expect(
        &mut self,
        expected: &'static [TokenRepr],
        value_at: usize,
    ) -> ParserResult<Token<'src>> {
        assert!(value_at < expected.len());
        let mut result = self.current()?;

        for (i, expect) in expected.iter().enumerate() {
            let tok = self.advance()?;

            if tok.repr != *expect {
                return Err(ParserError::unexpected(tok, *expect));
            } else if value_at == i {
                result = tok;
            }
        }

        Ok(result)
    }

    // --ACTUAL PARSER FUNCTIONS HERE--

    /// Parse an [`AstRepr::Const`], assumes that the current token
    /// is of type [`TokenRepr::Const`]
    pub fn parse_const(&mut self) -> ParserResult<AstElement<'src>> {
        let name = self.expect(
            &[TokenRepr::Const, TokenRepr::Identifier, TokenRepr::Set],
            1,
        )?;
        self.expect(&[TokenRepr::Set], 0)?;
        let expr = self.parse_expr()?;

        Ok(AstElement::new_const(name.pos, name.data, expr))
    }

    pub fn parse_expr(&mut self) -> ParserResult<Expr<'src>> {
        todo!()
    }
}

impl<'src, I: Iterator<Item = Token<'src>>> Iterator for Parser<'src, I> {
    type Item = ParserResult<AstElement<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.iter.current_copied()?;
        let res = match current.repr {
            TokenRepr::Const => self.parse_const(),
            _ => todo!(),
        };

        Some(res)
    }
}
