use std::iter::Peekable;
use std::str::Chars;

use crate::span::{Span, Pos};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    // Single character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,      // !
    BangEqual, // !=
    Equal,
    EqualEqual,
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Invalid(char),

    Eof,
}

pub struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    current: Pos,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            current: Pos { col: 0, line: 1 },
        }
    }

    fn next_token(&mut self) -> Span<Token> {
        use Token::*;

        // consume `meaningless` chars
        self.consume_while(char::is_whitespace);

        let start = self.current;

        let next = match self.consume() {
            Some(c) => c,
            None => return self.token(self.current, Eof),
        };

        match (next, self.peek()) {
            // TODO(extra): Nested multi-line comments.
            // comment line
            ('/', Some('/')) => {
                self.consume_while(|c| c != '\n');
                self.next_token()
            }

            ('(', ..) => self.token(start, LeftParen),
            (')', ..) => self.token(start, RightParen),
            ('{', ..) => self.token(start, LeftBrace),
            ('}', ..) => self.token(start, RightBrace),
            (',', ..) => self.token(start, Comma),
            ('.', ..) => self.token(start, Dot),
            ('-', ..) => self.token(start, Minus),
            ('+', ..) => self.token(start, Plus),
            (';', ..) => self.token(start, Semicolon),
            ('/', ..) => self.token(start, Slash), // we checked above for comment lines
            ('*', ..) => self.token(start, Star),

            ('!', ..) => self.token_or(start, BangEqual, Bang, |c| c == '='),
            ('=', ..) => self.token_or(start, EqualEqual, Equal, |c| c == '='),
            ('<', ..) => self.token_or(start, LessEqual, Less, |c| c == '='),
            ('>', ..) => self.token_or(start, GreaterEqual, Greater, |c| c == '='),

            (x, ..) if Self::is_digit(x) => self.number(start),
            (x, ..) if Self::is_alpha(x) => self.identifier(start, x),
            ('"', ..) => self.string(start),

            (x, ..) => self.token(start, Invalid(x)),
        }
    }

    fn string(&mut self, start: Pos) -> Span<Token> {
        self.consume_while(|c| c != '"');
        // consume closing `"`.
        // TODO: handle unclosed string
        self.consume();
        self.token(start, Token::String)
    }

    fn is_digit(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn number(&mut self, start: Pos) -> Span<Token> {
        self.consume_while(Self::is_digit);
        if matches!(
                (self.peek(), self.peek_next()),
                (Some('.'), Some(x)) if Self::is_digit(x)
        ) {
            // consume dot
            self.consume().expect("decimal dot");
            self.consume_while(Self::is_digit);
        }

        self.token(start, Token::Number)
    }

    fn is_alpha(c: char) -> bool {
        c >= 'a' && c <= 'z'
        || c >= 'A' && c <= 'Z'
        || c == '_'
    }

    fn is_alpha_numeric(c: char) -> bool {
        Self::is_alpha(c) || Self::is_digit(c)
    }

    fn identifier(&mut self, start: Pos, first: char) -> Span<Token> {
        use Token::*;

        const KEYWORDS: &[(&str, Token)] = &[
            ("and", And),
            ("class", Class),
            ("else", Else),
            ("false", False),
            ("fun", Fun),
            ("for", For),
            ("if", If),
            ("nil", Nil),
            ("or", Or),
            ("print", Print),
            ("return", Return),
            ("super", Super),
            ("this", This),
            ("true", True),
            ("var", Var),
            ("while", While),
        ];
        const KEYWORD_MAX_LEN: usize = 6;

        // Scan KEYWORD_MAX_LEN + 1 so that if we would had `returns` we would
        // also see the `s` and declare it an identifier instead of the keyword
        // `Token::Return`.
        let scan_len = KEYWORD_MAX_LEN + 1;
        let mut ident = std::string::String::with_capacity(scan_len);
        ident.push(first);

        // We cant do a raw index like self.source[x..x] as it would fail if we
        // had unicode chars before the range.
        self.consume_while(|c| {
            let accept = Self::is_alpha_numeric(c);
            if accept && ident.len() < scan_len {
                ident.push(c);
            }
            accept
        });

        match KEYWORDS.iter().find(|&&(w, _)| w == ident).map(|&(_, t)| t) {
            Some(token) => self.token(start, token),
            None => self.token(start, Identifier),
        }
    }

    fn token(&self, start: Pos, token: Token) -> Span<Token> {
        Span {
            start,
            end: self.current,
            item: token,
        }
    }

    fn token_or<F>(&mut self, start: Pos, left: Token, right: Token, cmp: F) -> Span<Token>
    where
        F: Fn(char) -> bool,
    {
        let next = self.peek();
        match next {
            Some(c) if cmp(c) => {
                // save we checked with peek == Some if there are more chars available.
                self.consume().unwrap();
                self.token(start, left)
            }

            _ => self.token(start, right),
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn peek_next(&mut self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next()?;
        chars.next()
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        match c {
            '\n' => {
                self.current.col = 0;
                self.current.line += 1;
            }
            _ => self.current.col += 1,
        };

        Some(c)
    }

    fn consume_while<F>(&mut self, mut func: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(c) = self.peek() {
            if !func(c) {
                break;
            }
            if self.consume().is_none() {
                break;
            }
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Span<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Span { item, .. } if item == Token::Eof => None,
            token => Some(token),
        }
    }
}
