use core::error::Error;
use core::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorRepr {
    Eof,
    UnknownCharacter(u8),
}

impl fmt::Display for ErrorRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "reached an unexpected eof"),
            Self::UnknownCharacter(c) => write!(f, "found an unknown character '{c}'"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexerError {
    repr: ErrorRepr,
    source_pos: SourcePosition,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "an error occurred at {} - {}",
            self.source_pos, self.repr
        )
    }
}

impl Error for LexerError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: usize,
    pub symbol: usize,
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.symbol + 1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    pub fn new(source: &'a str) -> Self {
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
                    self.source_pos.symbol = 0
                } else {
                    self.source_pos.symbol += 1;
                }

                self.position += 1;
            })
    }

    // attempts to rewind 1 step
    pub fn rewind(&mut self) {
        if self.position > 0 {
            self.position -= 1;
            self.source_pos = self.last_source_pos.unwrap();
            self.last_source_pos = None;
        }
    }

    /// Skips whitespace between tokens. Returns the next byte and a bool that checks if any whitespace was actually skipped
    pub fn skip_whitespace(&mut self) -> Option<(u8, bool)> {
        let mut found_whitespace = false;
        loop {
            let next = self.advance()?;

            if next.is_ascii_whitespace() {
                found_whitespace = true;
                continue;
            } else {
                return Some((next, found_whitespace));
            }
        }
    }

    pub fn lex_identifier(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.position - 1;

        while let Some(next) = self.advance() {
            if next.is_ascii_alphanumeric() || next == b'_' {
                continue;
            } else {
                self.rewind();
                break;
            }
        }

        Ok(Token::new(
            &self.source[start..self.position],
            TokenRepr::Identifier,
            self.source_pos,
        ))
    }

    pub fn lex_number(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.position - 1;
        while let Some(next) = self.advance() {
            if next.is_ascii_digit() {
                continue;
            } else {
                self.rewind();
                break;
            }
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
            let next = match self.advance() {
                Some(n) => n,
                None => {
                    return Err(LexerError {
                        repr: ErrorRepr::Eof,
                        source_pos: self.source_pos,
                    });
                }
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

    pub fn lex_fallback(&mut self, start: u8) -> Result<Token<'a>, LexerError> {
        match start {
            b'(' => Ok(self.small_token(TokenRepr::LParen, 1)),
            b')' => Ok(self.small_token(TokenRepr::RParen, 1)),
            b'.' => Ok(self.small_token(TokenRepr::Dot, 1)),
            b',' => Ok(self.small_token(TokenRepr::Coma, 1)),
            b'{' => Ok(self.small_token(TokenRepr::LFigure, 1)),
            b'}' => Ok(self.small_token(TokenRepr::RFigure, 1)),
            b'[' => Ok(self.small_token(TokenRepr::LBracket, 1)),
            b']' => Ok(self.small_token(TokenRepr::RBracket, 1)),
            b'<' => Ok(self.small_token_or(TokenRepr::LAngle, b'=', TokenRepr::Lt)),
            b'>' => Ok(self.small_token_or(TokenRepr::RAngle, b'=', TokenRepr::Gt)),
            b'=' => Ok(self.small_token_or_several(
                TokenRepr::Set,
                b"=>",
                &[TokenRepr::Equal, TokenRepr::Arrow],
            )),
            b':' => Ok(self.small_token(TokenRepr::Colon, 1)),
            b';' => Ok(self.small_token(TokenRepr::Semicolon, 1)),
            b'+' => Ok(self.small_token(TokenRepr::Plus, 1)),
            b'-' => Ok(self.small_token(TokenRepr::Minus, 1)),
            b'*' => Ok(self.small_token(TokenRepr::Mult, 1)),
            b'/' => Ok(self.small_token(TokenRepr::Div, 1)),

            other => Err(LexerError {
                repr: ErrorRepr::UnknownCharacter(other),
                source_pos: self.source_pos,
            }),
        }
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
        let (next, _) = self.skip_whitespace()?;
        Some(
            match next {
                b'a'..=b'z' | b'A'..=b'Z' => self.lex_identifier(),
                b'0'..=b'9' => self.lex_number(),
                b'"' => self.lex_string(),
                f => self.lex_fallback(f),
            }
            .map(|t| t.try_convert_to_keyword()),
        )
    }
}
