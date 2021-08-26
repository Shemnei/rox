use crate::span::{BytePos, Pos, Span};
use crate::token::{Token, TokenKind};

pub(crate) const KEYWORDS: &[(&str, TokenKind)] = &[
	("and", TokenKind::And),
	("class", TokenKind::Class),
	("else", TokenKind::Else),
	("false", TokenKind::False),
	("fun", TokenKind::Fun),
	("for", TokenKind::For),
	("if", TokenKind::If),
	("nil", TokenKind::Nil),
	("or", TokenKind::Or),
	("print", TokenKind::Print),
	("return", TokenKind::Return),
	("super", TokenKind::Super),
	("this", TokenKind::This),
	("true", TokenKind::True),
	("var", TokenKind::Var),
	("while", TokenKind::While),
];

const KEYWORD_MAX_LEN: usize = 6;

#[derive(Debug)]
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

	fn next_token(&mut self) -> Token {
		// consume `meaningless` chars
		self.consume_while(Self::is_whitespace);

		let start = self.current;

		let next = match self.consume() {
			Some(b) => b,
			None => return self.emit(self.current, TokenKind::Eof),
		};

		match (next, self.peek()) {
			// TODO(extra): Nested multi-line comments.
			// comment line
			(b'/', Some(b'/')) => {
				self.consume_while(|b| b != b'\n');
				self.next_token()
			}

			(b'(', ..) => self.emit(start, TokenKind::LeftParen),
			(b')', ..) => self.emit(start, TokenKind::RightParen),
			(b'{', ..) => self.emit(start, TokenKind::LeftBrace),
			(b'}', ..) => self.emit(start, TokenKind::RightBrace),
			(b',', ..) => self.emit(start, TokenKind::Comma),
			(b'.', ..) => self.emit(start, TokenKind::Dot),
			(b'-', ..) => self.emit(start, TokenKind::Minus),
			(b'+', ..) => self.emit(start, TokenKind::Plus),
			(b';', ..) => self.emit(start, TokenKind::Semicolon),
			(b'/', ..) => self.emit(start, TokenKind::Slash), // we checked above for comment lines
			(b'*', ..) => self.emit(start, TokenKind::Star),

			(b'!', ..) => self.emit_or(
				start,
				TokenKind::BangEqual,
				TokenKind::Bang,
				|b| b == b'=',
			),
			(b'=', ..) => self.emit_or(
				start,
				TokenKind::EqualEqual,
				TokenKind::Equal,
				|b| b == b'=',
			),
			(b'<', ..) => self.emit_or(
				start,
				TokenKind::LessEqual,
				TokenKind::Less,
				|b| b == b'=',
			),
			(b'>', ..) => self.emit_or(
				start,
				TokenKind::GreaterEqual,
				TokenKind::Greater,
				|b| b == b'=',
			),

			(x, ..) if Self::is_digit(x) => self.number(start),
			(x, ..) if Self::is_alpha(x) => self.identifier(start),
			(b'"', ..) => self.string(start),

			(x, ..) => self.emit(start, TokenKind::Invalid(x)),
		}
	}

	fn string(&mut self, start: BytePos) -> Token {
		self.consume_while(|b| b != b'"');
		// TODO: handle unclosed string
		// consume closing `"`.
		self.consume();
		self.emit(start, TokenKind::String)
	}

	fn number(&mut self, start: BytePos) -> Token {
		self.consume_while(Self::is_digit);
		if matches!(
				(self.peek(), self.peek_next()),
				(Some(b'.'), Some(x)) if Self::is_digit(x)
		) {
			// consume dot
			self.consume().expect("decimal dot");
			self.consume_while(Self::is_digit);
		}

		self.emit(start, TokenKind::Number)
	}

	fn identifier(&mut self, start: BytePos) -> Token {
		self.consume_while(Self::is_alpha_numeric);
		// As we work with byte positions a raw index into the string is valid.
		let ident = &self.source[start.to_usize()..self.current.to_usize()];

		if ident.len() > KEYWORD_MAX_LEN {
			return self.emit(start, TokenKind::Identifier);
		}

		match KEYWORDS.iter().find(|&&(w, _)| w == ident).map(|&(_, t)| t) {
			Some(token) => self.emit(start, token),
			None => self.emit(start, TokenKind::Identifier),
		}
	}

	const fn emit(&self, start: BytePos, kind: TokenKind) -> Token {
		Token { span: Span { low: start, high: self.current }, kind }
	}

	fn emit_or<F>(
		&mut self,
		start: BytePos,
		left: TokenKind,
		right: TokenKind,
		cmp: F,
	) -> Token
	where
		F: Fn(u8) -> bool,
	{
		let next = self.peek();
		match next {
			Some(b) if cmp(b) => {
				// Unwrap is save we checked with peek == Some if there are
				// more chars available.
				self.consume().expect("consume to return a valid byte");
				self.emit(start, left)
			}

			_ => self.emit(start, right),
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

	const fn is_whitespace(b: u8) -> bool {
		b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' || b == 0xc // form feed
	}

	const fn is_digit(b: u8) -> bool {
		b >= b'0' && b <= b'9'
	}

	const fn is_alpha(b: u8) -> bool {
		b >= b'a' && b <= b'z' || b >= b'A' && b <= b'Z' || b == b'_'
	}

	const fn is_alpha_numeric(b: u8) -> bool {
		Self::is_alpha(b) || Self::is_digit(b)
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		match self.next_token() {
			Token { kind, .. } if kind == TokenKind::Eof => None,
			token => Some(token),
		}
	}
}

#[derive(Debug)]
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
