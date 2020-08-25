use crate::span::{BytePos, Pos, Span, Spanned};
use crate::token::Token;

pub struct Lexer<'a> {
    source: &'a str,
    cursor: Cursor<'a, u8>,
    current: BytePos,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: Cursor::new(source.as_bytes()),
            current: BytePos(0),
        }
    }

    fn next_token(&mut self) -> Spanned<Token> {
        // consume `meaningless` chars
        self.consume_while(Self::is_whitespace);

        let start = self.current;

        let next = match self.consume() {
            Some(b) => b,
            None => return self.token(self.current, Token::Eof),
        };

        match (next, self.peek()) {
            // TODO(extra): Nested multi-line comments.
            // comment line
            (b'/', Some(b'/')) => {
                self.consume_while(|b| b != b'\n');
                self.next_token()
            }

            (b'(', ..) => self.token(start, Token::LeftParen),
            (b')', ..) => self.token(start, Token::RightParen),
            (b'{', ..) => self.token(start, Token::LeftBrace),
            (b'}', ..) => self.token(start, Token::RightBrace),
            (b',', ..) => self.token(start, Token::Comma),
            (b'.', ..) => self.token(start, Token::Dot),
            (b'-', ..) => self.token(start, Token::Minus),
            (b'+', ..) => self.token(start, Token::Plus),
            (b';', ..) => self.token(start, Token::Semicolon),
            (b'/', ..) => self.token(start, Token::Slash), // we checked above for comment lines
            (b'*', ..) => self.token(start, Token::Star),

            (b'!', ..) => self.token_or(start, Token::BangEqual, Token::Bang, |b| b == b'='),
            (b'=', ..) => self.token_or(start, Token::EqualEqual, Token::Equal, |b| b == b'='),
            (b'<', ..) => self.token_or(start, Token::LessEqual, Token::Less, |b| b == b'='),
            (b'>', ..) => self.token_or(start, Token::GreaterEqual, Token::Greater, |b| b == b'='),

            (x, ..) if Self::is_digit(x) => self.number(start),
            (x, ..) if Self::is_alpha(x) => self.identifier(start),
            (b'"', ..) => self.string(start),

            (x, ..) => self.token(start, Token::Invalid(x)),
        }
    }

    fn string(&mut self, start: BytePos) -> Spanned<Token> {
        self.consume_while(|b| b != b'"');
        // TODO: handle unclosed string
        // consume closing `"`.
        self.consume();
        self.token(start, Token::String)
    }

    fn number(&mut self, start: BytePos) -> Spanned<Token> {
        self.consume_while(Self::is_digit);
        if matches!(
                (self.peek(), self.peek_next()),
                (Some(b'.'), Some(x)) if Self::is_digit(x)
        ) {
            // consume dot
            self.consume().expect("decimal dot");
            self.consume_while(Self::is_digit);
        }

        self.token(start, Token::Number)
    }

    fn identifier(&mut self, start: BytePos) -> Spanned<Token> {
        const KEYWORDS: &[(&str, Token)] = &[
            ("and", Token::And),
            ("class", Token::Class),
            ("else", Token::Else),
            ("false", Token::False),
            ("fun", Token::Fun),
            ("for", Token::For),
            ("if", Token::If),
            ("nil", Token::Nil),
            ("or", Token::Or),
            ("print", Token::Print),
            ("return", Token::Return),
            ("super", Token::Super),
            ("this", Token::This),
            ("true", Token::True),
            ("var", Token::Var),
            ("while", Token::While),
        ];
        const KEYWORD_MAX_LEN: usize = 6;

        self.consume_while(Self::is_alpha_numeric);
        // As we work with byte positions a raw index into the string is valid.
        let ident = &self.source[start.to_usize()..self.current.to_usize()];

        if ident.len() > KEYWORD_MAX_LEN {
            return self.token(start, Token::Identifier);
        }

        match KEYWORDS.iter().find(|&&(w, _)| w == ident).map(|&(_, t)| t) {
            Some(token) => self.token(start, token),
            None => self.token(start, Token::Identifier),
        }
    }

    fn token(&self, start: BytePos, token: Token) -> Spanned<Token> {
        Spanned {
            span: Span {
                low: start,
                high: self.current,
            },
            item: token,
        }
    }

    fn token_or<F>(&mut self, start: BytePos, left: Token, right: Token, cmp: F) -> Spanned<Token>
    where
        F: Fn(u8) -> bool,
    {
        let next = self.peek();
        match next {
            Some(b) if cmp(b) => {
                // Unwrap is save we checked with peek == Some if there are
                // more chars available.
                self.consume().expect("consume to return a valid byte");
                self.token(start, left)
            }

            _ => self.token(start, right),
        }
    }

    fn peek(&mut self) -> Option<u8> {
        self.cursor.first()
    }

    fn peek_next(&mut self) -> Option<u8> {
        self.cursor.second()
    }

    fn consume(&mut self) -> Option<u8> {
        let b = self.cursor.next()?;
        self.current += 1;
        Some(b)
    }

    fn consume_while<F>(&mut self, mut func: F)
    where
        F: FnMut(u8) -> bool,
    {
        while let Some(b) = self.cursor.first() {
            if !func(b) {
                break;
            }
            self.consume().expect("consume to return a valid byte");
        }
    }

    fn is_whitespace(b: u8) -> bool {
        b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' || b == 0xc // form feed
    }

    fn is_digit(b: u8) -> bool {
        b >= b'0' && b <= b'9'
    }

    fn is_alpha(b: u8) -> bool {
        b >= b'a' && b <= b'z' || b >= b'A' && b <= b'Z' || b == b'_'
    }

    fn is_alpha_numeric(b: u8) -> bool {
        Self::is_alpha(b) || Self::is_digit(b)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Spanned { item, .. } if item == Token::Eof => None,
            token => Some(token),
        }
    }
}

pub(crate) struct Cursor<'a, T> {
    source: &'a [T],
    offset: usize,
}

impl<'a, T: Copy> Cursor<'a, T> {
    pub(crate) fn new(source: &'a [T]) -> Self {
        Self { source, offset: 0 }
    }

    pub(crate) fn first(&self) -> Option<T> {
        self.peek_nth(0)
    }

    pub(crate) fn second(&self) -> Option<T> {
        self.peek_nth(1)
    }

    pub(crate) fn next(&mut self) -> Option<T> {
        let next = self.get(self.offset)?;
        self.offset += 1;
        Some(next)
    }

    fn peek_nth(&self, nth: usize) -> Option<T> {
        self.get(self.offset + nth)
    }

    fn in_bounds(&self, idx: usize) -> bool {
        self.source.len() > idx
    }

    fn get(&self, idx: usize) -> Option<T> {
        if self.in_bounds(idx) {
            Some(self.source[idx])
        } else {
            None
        }
    }
}
