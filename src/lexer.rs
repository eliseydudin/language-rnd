use core::cmp;
use core::error::Error;
use core::fmt;
use core::ops;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorRepr {
    Eof,
    UnknownCharacter(u8),
    RepeatedDotInNumber,
}

impl fmt::Display for ErrorRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "reached an unexpected eof"),
            Self::UnknownCharacter(c) => write!(f, "found an unknown character '{c}'"),
            Self::RepeatedDotInNumber => write!(f, "dot repeated inside a number declaration"),
        }
    }
}

pub type LexerError = WithPos<ErrorRepr>;

impl Error for LexerError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: usize,
    pub symbol: usize,
}

pub struct WithPos<T> {
    pub inner: T,
    pub pos: SourcePosition,
}

impl<T> WithPos<T> {
    pub const fn new(inner: T, pos: SourcePosition) -> Self {
        Self { inner, pos }
    }
}

impl<T> ops::Deref for WithPos<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> ops::DerefMut for WithPos<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug)]
pub enum WithPosOrEof<T> {
    Pos(WithPos<T>),
    Eof(T),
}

impl<T: cmp::PartialEq> cmp::PartialEq for WithPosOrEof<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pos(s), Self::Pos(o)) => s.inner == o.inner,
            (Self::Eof(s), Self::Eof(o)) => s == o,
            _ => false,
        }
    }
}

impl<T: Display> fmt::Display for WithPosOrEof<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pos(p) => write!(f, "{p}"),
            Self::Eof(t) => write!(f, "<EOF> {t}"),
        }
    }
}

impl<T: 'static + Error> Error for WithPosOrEof<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Pos(p) => Some(p),
            Self::Eof(t) => Some(t),
        }
    }
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.symbol + 1)
    }
}

impl<T: fmt::Display> fmt::Display for WithPos<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.pos, self.inner)
    }
}

impl<T: fmt::Debug> fmt::Debug for WithPos<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WithPos")
            .field("inner", &self.inner)
            .field("pos", &self.pos)
            .finish()
    }
}

impl<T: Error> Error for WithPos<T> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer<'src> {
    source: &'src str,
    position: usize,
    source_pos: SourcePosition,
    last_source_pos: Option<SourcePosition>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenRepr {
    Number,
    String,
    Identifier,

    LParen,
    RParen,
    LFigure,
    RFigure,
    LBracket,
    RBracket,
    LAngle,
    RAngle,

    Dot,
    Coma,

    Lt,
    Gt,
    Set,
    Equal,

    Colon,
    Semicolon,

    Arrow,

    Plus,
    Minus,
    Mult,
    Div,

    Const,
    Fn,

    Comment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub data: &'a str,
    pub repr: TokenRepr,
    pub pos: SourcePosition,
}

impl<'a> Token<'a> {
    pub const fn new(data: &'a str, repr: TokenRepr, pos: SourcePosition) -> Self {
        let pos = SourcePosition {
            line: pos.line,
            symbol: pos.symbol - data.len(),
        };

        Self { data, repr, pos }
    }

    pub fn try_convert_to_keyword(self) -> Self {
        // TODO
        let repr = match self.data {
            "const" => TokenRepr::Const,
            "fn" => TokenRepr::Fn,
            _ => self.repr,
        };

        Self {
            data: self.data,
            repr,
            pos: self.pos,
        }
    }
}

impl<'a> Lexer<'a> {
    pub const fn new(source: &'a str) -> Self {
        Self {
            source,
            source_pos: SourcePosition { line: 0, symbol: 0 },
            position: 0,
            last_source_pos: None,
        }
    }

    /// Get the next symbol in the source string, advance `position` and `source_position`.
    /// Returns [`None`] if reached Eof.
    pub fn advance(&mut self) -> Option<u8> {
        self.source
            .as_bytes()
            .get(self.position)
            .copied()
            .inspect(|byte| {
                self.last_source_pos = Some(self.source_pos);

                if *byte == b'\n' {
                    self.source_pos.line += 1;
                    self.source_pos.symbol = 0;
                } else {
                    self.source_pos.symbol += 1;
                }

                self.position += 1;
            })
    }

    // attempts to rewind 1 step
    pub const fn rewind(&mut self) {
        if self.position > 0 {
            self.position -= 1;
            self.source_pos = self.last_source_pos.unwrap();
            self.last_source_pos = None;
        }
    }

    /// Skips whitespace between tokens. Returns the next byte and a bool that checks if any whitespace was actually skipped
    pub fn skip_whitespace(&mut self) -> Option<u8> {
        loop {
            let next = self.advance()?;

            if next.is_ascii_whitespace() {
                continue;
            }
            return Some(next);
        }
    }

    pub fn lex_identifier(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.position - 1;

        while let Some(next) = self.advance() {
            if next.is_ascii_alphanumeric() || next == b'_' {
                continue;
            }
            self.rewind();
            break;
        }

        Ok(Token::new(
            &self.source[start..self.position],
            TokenRepr::Identifier,
            self.source_pos,
        ))
    }

    pub fn lex_number(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.position - 1;

        let mut found_dot = false;

        while let Some(next) = self.advance() {
            if next == b'.' {
                if found_dot {
                    return Err(LexerError::new(
                        ErrorRepr::RepeatedDotInNumber,
                        self.source_pos,
                    ));
                } else {
                    found_dot = true;
                    continue;
                }
            } else if next.is_ascii_digit() {
                continue;
            }

            self.rewind();
            break;
        }

        Ok(Token::new(
            &self.source[start..self.position],
            TokenRepr::Number,
            self.source_pos,
        ))
    }

    pub fn lex_string(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.position;
        loop {
            let Some(next) = self.advance() else {
                return Err(WithPos::new(ErrorRepr::Eof, self.source_pos));
            };

            if next == b'"' {
                break;
            }
        }

        Ok(Token::new(
            &self.source[start..self.position - 1],
            TokenRepr::String,
            self.source_pos,
        ))
    }

    pub fn current(&self) -> Option<u8> {
        self.source.as_bytes().get(self.position).copied()
    }

    pub fn lex_comment(&mut self) -> Token<'a> {
        let start = self.position;
        loop {
            self.advance();
            let last_pos = self
                .last_source_pos
                .expect("after calling advance last_pos is always Some");

            if last_pos.line != self.source_pos.line {
                // advanced to a new line
                return Token::new(
                    &self.source[start..self.position - 1],
                    TokenRepr::Comment,
                    self.last_source_pos.unwrap(),
                );
            }
        }
    }

    pub fn lex_fallback(&mut self, start: u8) -> Result<Token<'a>, LexerError> {
        let result = match start {
            b'(' => self.small_token(TokenRepr::LParen, 1),
            b')' => self.small_token(TokenRepr::RParen, 1),
            b'.' => self.small_token(TokenRepr::Dot, 1),
            b',' => self.small_token(TokenRepr::Coma, 1),
            b'{' => self.small_token(TokenRepr::LFigure, 1),
            b'}' => self.small_token(TokenRepr::RFigure, 1),
            b'[' => self.small_token(TokenRepr::LBracket, 1),
            b']' => self.small_token(TokenRepr::RBracket, 1),
            b'<' => self.small_token_or(TokenRepr::LAngle, b'=', TokenRepr::Lt),
            b'>' => self.small_token_or(TokenRepr::RAngle, b'=', TokenRepr::Gt),
            b'=' => self.small_token_or_several(
                TokenRepr::Set,
                b"=>",
                &[TokenRepr::Equal, TokenRepr::Arrow],
            ),
            b':' => self.small_token(TokenRepr::Colon, 1),
            b';' => self.small_token(TokenRepr::Semicolon, 1),
            b'+' => self.small_token(TokenRepr::Plus, 1),
            b'-' => self.small_token(TokenRepr::Minus, 1),
            b'*' => self.small_token(TokenRepr::Mult, 1),
            b'/' => {
                if self.current() == Some(b'/') {
                    self.advance();
                    self.lex_comment()
                } else {
                    self.small_token(TokenRepr::Div, 1)
                }
            }

            other => {
                return Err(WithPos::new(
                    ErrorRepr::UnknownCharacter(other),
                    self.source_pos,
                ));
            }
        };

        Ok(result)
    }

    fn small_token(&self, ty: TokenRepr, offset: usize) -> Token<'a> {
        Token::new(
            &self.source[self.position - offset..self.position],
            ty,
            self.source_pos,
        )
    }

    fn small_token_or(&mut self, ty: TokenRepr, expect: u8, then: TokenRepr) -> Token<'a> {
        if self.current() == Some(expect) {
            self.advance();
            self.small_token(then, 2)
        } else {
            self.small_token(ty, 1)
        }
    }

    fn small_token_or_several(
        &mut self,
        ty: TokenRepr,
        expect: &[u8],
        then: &[TokenRepr],
    ) -> Token<'a> {
        assert_eq!(expect.len(), then.len());
        for (i, elem) in expect.iter().zip(then) {
            if self.current() == Some(*i) {
                self.advance();
                return self.small_token(*elem, 2);
            }
        }

        self.small_token(ty, 1)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.skip_whitespace()?;
        Some(
            match next {
                b'a'..=b'z' | b'A'..=b'Z' => self.lex_identifier(),
                b'0'..=b'9' => self.lex_number(),
                b'"' => self.lex_string(),
                f => self.lex_fallback(f),
            }
            .map(Token::try_convert_to_keyword),
        )
    }
}
