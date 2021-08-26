use std::fmt;

use crate::expr::{Expr, ExprKind};
use crate::lex::Cursor;
use crate::rox::Session;
use crate::span::*;
use crate::stmt::{Stmt, StmtKind};
use crate::token::{Token, TokenKind};

const MAX_ARGS_COUNT: usize = 255;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, Copy)]
pub enum ParseError {
	UnexpectedToken {
		expected: TokenKind,
		got: Token,
	},

	UnexpectedExpression {
		span: Span,
		expected: &'static str,
		got: &'static str,
	},

	ExpectedExpression {
		got: Token,
	},

	ArgumentCountExceeded {
		span: Span,
	},
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::UnexpectedToken {
				expected,
				got: Token { kind: TokenKind::Eof, .. },
			} => write!(
				f,
				"[EOF]: Expected token {} at the end",
				expected.name()
			),

			Self::UnexpectedToken { expected, got } => write!(
				f,
				"[{}]: Expected token {} but got {} instead",
				got.span,
				expected.name(),
				got.name()
			),

			Self::UnexpectedExpression { span, expected, got } => write!(
				f,
				"[{}]: Expected expression {} but got {} instead",
				span, expected, got
			),

			Self::ExpectedExpression {
				got: Token { kind: TokenKind::Eof, .. },
			} => write!(f, "[EOF]: Expected an expression "),

			Self::ExpectedExpression { got } => write!(
				f,
				"[{}]: Expected an expression but got {} instead",
				got.span,
				got.name()
			),

			Self::ArgumentCountExceeded { span } => write!(
				f,
				"[{}]: Argument count of {} was exceeded",
				span, MAX_ARGS_COUNT
			),
		}
	}
}

impl std::error::Error for ParseError {}

// TODO: look in stdlib if there is a better way to do this.
macro_rules! unwrap {
	($or:expr) => {
		match $or? {
			Ok(value) => value,
			Err(err) => return Some(Err(err)),
		}
	};
}

macro_rules! expect_some {
	($or:expr) => {
		match $or {
			Some(res) => match res {
				Ok(value) => value,
				Err(err) => return Some(Err(err)),
			},
			None => {
				return Some(Err(ParseError::ExpectedExpression {
					got: Token::new(TokenKind::Eof, Span::DUMMY),
				}))
			}
		}
	};
}

// https://doc.rust-lang.org/src/core/macros/mod.rs.html#254-261
macro_rules! some_matches {
    ($option:expr, $($pattern:pat)|+) => {
        match $option {
            None => false,
            Some(token) => matches!(token.kind, $($pattern)|+),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
	sess: &'a mut Session,
	cursor: Cursor<'a, Token>,
	current: usize,
}

// FIXME: replace all unwrap! with errors where the stmt/expr is required
impl<'a> Parser<'a> {
	pub fn new(session: &'a mut Session, tokens: &'a [Token]) -> Self {
		Self { sess: session, cursor: Cursor::new(tokens), current: 0 }
	}

	pub fn next_stmt(&mut self) -> Option<Result<Stmt>> {
		self.declaration()
	}

	#[allow(dead_code)]
	fn synchronize(&mut self) -> Option<()> {
		// TODO: cleanup
		if matches!(
			self.peek()?.kind,
			TokenKind::Class
				| TokenKind::Fun | TokenKind::Var
				| TokenKind::For | TokenKind::If
				| TokenKind::While
				| TokenKind::Print
				| TokenKind::Return
		) {
			return Some(());
		}

		let prev = self.consume()?;
		loop {
			if prev.kind == TokenKind::Semicolon {
				return Some(());
			}

			if matches!(
				self.peek()?.kind,
				TokenKind::Class
					| TokenKind::Fun | TokenKind::Var
					| TokenKind::For | TokenKind::If
					| TokenKind::While | TokenKind::Print
					| TokenKind::Return
			) {
				return Some(());
			}
		}
	}

	fn declaration(&mut self) -> Option<Result<Stmt>> {
		let stmt = if some_matches!(self.peek(), TokenKind::Fun) {
			self.function("function")
		} else if some_matches!(self.peek(), TokenKind::Var) {
			self.var_declaration()
		} else {
			self.statement()
		}?;

		match stmt {
			Ok(stmt) => Some(Ok(stmt)),
			Err(err) => {
				eprintln!("ParserSync: {}", err);
				// TODO: maybe return SynchronizationError and handle it in next_stmt ??
				self.synchronize();
				self.declaration()
			}
		}
	}

	fn function(&mut self, _kind: &'static str) -> Option<Result<Stmt>> {
		let start = unwrap!(self.expect(TokenKind::Fun));
		let name = unwrap!(self.expect(TokenKind::Identifier));

		let symbol = match self.sess.intern(name.span) {
			Some(symbol) => symbol,
			None => panic!(
				"Could not find span {} for {} in source",
				name.span,
				name.name()
			),
		};

		let _ = unwrap!(self.expect(TokenKind::LeftParen));

		let mut params: Vec<Token> = Vec::new();

		if !matches!(
			self.peek().expect("valid parameter token").kind,
			TokenKind::RightParen
		) {
			loop {
				if params.len() >= MAX_ARGS_COUNT {
					let span = start.span.union(
						params.last().expect("last valid parameter").span,
					);
					// TODO: just report error and continue
					return Some(Err(ParseError::ArgumentCountExceeded {
						span,
					}));
				}

				params.push(unwrap!(self.expect(TokenKind::Identifier)));

				if !matches!(
					self.peek().expect("valid parameter token").kind,
					TokenKind::Comma
				) {
					break;
				}

				let _ = unwrap!(self.expect(TokenKind::Comma));
			}
		}

		let _ = unwrap!(self.expect(TokenKind::RightParen));

		let (end_span, body) = expect_some!(self.block());

		let kind = StmtKind::Function { name, symbol, params, body };

		Some(Ok(Stmt { span: start.span.union(end_span), kind }))
	}

	fn var_declaration(&mut self) -> Option<Result<Stmt>> {
		let _ = unwrap!(self.expect(TokenKind::Var));
		let name = unwrap!(self.expect(TokenKind::Identifier));
		let symbol = match self.sess.intern(name.span) {
			Some(symbol) => symbol,
			None => panic!(
				"Could not find span {} for {} in source",
				name.span,
				name.name()
			),
		};

		let initializer: Option<Box<Expr>> =
			if some_matches!(self.peek(), TokenKind::Equal) {
				let _ = unwrap!(self.expect(TokenKind::Equal));
				Some(unwrap!(self.expression()).into())
			} else {
				None
			};

		let _ = unwrap!(self.expect(TokenKind::Semicolon));

		let mut span: Span = match initializer {
			Some(ref expr) => name.span.union(expr.span),
			None => name.span,
		};

		span.high += BytePos(1);
		let kind = StmtKind::Var { name, symbol, initializer };
		Some(Ok(Stmt { span, kind }))
	}

	fn statement(&mut self) -> Option<Result<Stmt>> {
		if some_matches!(self.peek(), TokenKind::For) {
			self.for_statement()
		} else if some_matches!(self.peek(), TokenKind::If) {
			self.if_statement()
		} else if some_matches!(self.peek(), TokenKind::Print) {
			self.print_statement()
		} else if some_matches!(self.peek(), TokenKind::Return) {
			self.return_statement()
		} else if some_matches!(self.peek(), TokenKind::While) {
			self.while_statement()
		} else if some_matches!(self.peek(), TokenKind::LeftBrace) {
			let (span, statments) = unwrap!(self.block());
			Some(Ok(Stmt { span, kind: StmtKind::Block { statments } }))
		} else {
			self.expression_statement()
		}
	}

	fn for_statement(&mut self) -> Option<Result<Stmt>> {
		let start = unwrap!(self.expect(TokenKind::For));
		let _ = unwrap!(self.expect(TokenKind::LeftParen));

		let next = self.peek().expect("valid token");
		// All branches remove the semicolon.
		let initializer = match next.kind {
			TokenKind::Semicolon => {
				unwrap!(self.expect(TokenKind::Semicolon));
				None
			}
			TokenKind::Var => Some(unwrap!(self.var_declaration())),
			_ => Some(unwrap!(self.expression_statement())),
		};

		let next = self.peek().expect("valid token");
		let condition = match next.kind {
			TokenKind::Semicolon => None,
			_ => Some(unwrap!(self.expression())),
		};
		unwrap!(self.expect(TokenKind::Semicolon));

		let next = self.peek().expect("valid token");
		let increment = match next.kind {
			TokenKind::RightParen => None,
			_ => Some(unwrap!(self.expression())),
		};

		let _ = unwrap!(self.expect(TokenKind::RightParen));

		let mut body = unwrap!(self.statement());

		// unroll
		let span = start.span.union(body.span);

		// increment
		if let Some(increment) = increment {
			let inc_span = increment.span;
			let inc_kind =
				StmtKind::Expression { expression: increment.into() };

			let kind = StmtKind::Block {
				statments: vec![
					body.into(),
					Stmt { span: inc_span, kind: inc_kind }.into(),
				],
			};

			body = Stmt { span, kind };
		}

		// condition
		let condition = match condition {
			Some(condition) => condition,
			None => {
				let mut interner = self.sess.int_mut();
				let kind = ExprKind::Literal {
					value: Token {
						// TODO: no dummy span
						span: Span::DUMMY,
						kind: TokenKind::True,
					},
					symbol: interner.intern("true"),
				};
				Expr::new(kind)
			}
		};

		let kind =
			StmtKind::While { condition: condition.into(), body: body.into() };

		body = Stmt { span, kind };

		// initializer
		if let Some(initializer) = initializer {
			let kind = StmtKind::Block {
				statments: vec![initializer.into(), body.into()],
			};

			body = Stmt { span, kind };
		}

		Some(Ok(body))
	}

	fn if_statement(&mut self) -> Option<Result<Stmt>> {
		let start = unwrap!(self.expect(TokenKind::If));
		let _ = unwrap!(self.expect(TokenKind::LeftParen));
		let condition = unwrap!(self.expression());
		let _ = unwrap!(self.expect(TokenKind::RightParen));

		let then_branch = unwrap!(self.statement());
		let (else_branch, end): (Option<Box<Stmt>>, Span) =
			if some_matches!(self.peek(), TokenKind::Else) {
				let _ = unwrap!(self.expect(TokenKind::Else));
				let stmt = unwrap!(self.statement());
				let span = stmt.span;
				(Some(stmt.into()), span)
			} else {
				(None, then_branch.span)
			};

		let span = start.span.union(end);
		let kind = StmtKind::If {
			condition: condition.into(),
			then_branch: then_branch.into(),
			else_branch,
		};
		Some(Ok(Stmt { span, kind }))
	}

	fn print_statement(&mut self) -> Option<Result<Stmt>> {
		let start = unwrap!(self.expect(TokenKind::Print));
		let expr = unwrap!(self.expression());
		let end = unwrap!(self.expect(TokenKind::Semicolon));

		let span = start.span.union(end.span);
		let kind = StmtKind::Print { expression: expr.into() };
		Some(Ok(Stmt { span, kind }))
	}

	fn return_statement(&mut self) -> Option<Result<Stmt>> {
		let start = unwrap!(self.expect(TokenKind::Return));
		let value = if some_matches!(self.peek(), TokenKind::Semicolon) {
			None
		} else {
			Some(expect_some!(self.expression()).into())
		};

		let end = unwrap!(self.expect(TokenKind::Semicolon));

		let span = start.span.union(end.span);
		let kind = StmtKind::Return { keyword: start, value };
		Some(Ok(Stmt { span, kind }))
	}

	fn while_statement(&mut self) -> Option<Result<Stmt>> {
		let start = unwrap!(self.expect(TokenKind::While));
		let _ = unwrap!(self.expect(TokenKind::LeftParen));
		let condition = unwrap!(self.expression());
		let _ = unwrap!(self.expect(TokenKind::RightParen));
		let body = unwrap!(self.statement());

		let span = start.span.union(body.span);
		let kind =
			StmtKind::While { condition: condition.into(), body: body.into() };
		Some(Ok(Stmt { span, kind }))
	}

	fn expression_statement(&mut self) -> Option<Result<Stmt>> {
		let expr = unwrap!(self.expression());
		let _ = unwrap!(self.expect(TokenKind::Semicolon));

		let span = expr.span.add_high(1u32.into());
		let kind = StmtKind::Expression { expression: expr.into() };
		Some(Ok(Stmt { span, kind }))
	}

	fn block(&mut self) -> Option<Result<(Span, Vec<Box<Stmt>>)>> {
		let open = unwrap!(self.expect(TokenKind::LeftBrace));

		let mut stmts = Vec::new();

		while self.peek().is_some()
			&& !some_matches!(self.peek(), TokenKind::RightBrace)
		{
			stmts.push(unwrap!(self.declaration()).into());
		}

		let close = unwrap!(self.expect(TokenKind::RightBrace));

		Some(Ok((open.span.union(close.span), stmts)))
	}

	fn expression(&mut self) -> Option<Result<Expr>> {
		self.assignment()
	}

	fn assignment(&mut self) -> Option<Result<Expr>> {
		let expr = unwrap!(self.or());

		if some_matches!(self.peek(), TokenKind::Equal) {
			let _ = self.expect(TokenKind::Equal)?;
			let value = unwrap!(self.assignment());

			return match expr {
				Expr {
					kind: ExprKind::Variable { name, symbol, .. }, ..
				} => Some(Ok(Expr::new(ExprKind::Assign {
					name,
					symbol,
					value: value.into(),
				}))),
				_ => Some(Err(ParseError::UnexpectedExpression {
					span: expr.span,
					expected: "variable",
					got: expr.name(),
				})),
			};
		}

		Some(Ok(expr))
	}

	fn or(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.and());

		if some_matches!(self.peek(), TokenKind::Or) {
			// consume operator
			let operator = unwrap!(self.expect(TokenKind::Or));
			let right: Expr = expect_some!(self.and());
			let kind = ExprKind::Logical {
				left: expr.into(),
				operator,
				right: right.into(),
			};
			expr = Expr::new(kind);
		}

		Some(Ok(expr))
	}

	fn and(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.equality());

		if some_matches!(self.peek(), TokenKind::And) {
			// consume operator
			let operator = unwrap!(self.expect(TokenKind::And));
			let right: Expr = expect_some!(self.equality());
			let kind = ExprKind::Logical {
				left: expr.into(),
				operator,
				right: right.into(),
			};
			expr = Expr::new(kind);
		}

		Some(Ok(expr))
	}

	fn equality(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.comparison());

		while some_matches!(
			self.peek(),
			TokenKind::BangEqual | TokenKind::EqualEqual
		) {
			// consume operator
			let operator =
				self.consume().expect("valid equality operator token");
			let right: Expr = expect_some!(self.comparison());
			let kind = ExprKind::Binary {
				left: expr.into(),
				operator,
				right: right.into(),
			};
			expr = Expr::new(kind);
		}

		Some(Ok(expr))
	}

	fn comparison(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.addition());

		while some_matches!(
			self.peek(),
			TokenKind::Greater
				| TokenKind::GreaterEqual
				| TokenKind::Less
				| TokenKind::LessEqual
		) {
			// consume operator
			let operator =
				self.consume().expect("valid comparison operator token");
			let right = expect_some!(self.addition());
			let kind = ExprKind::Binary {
				left: expr.into(),
				operator,
				right: right.into(),
			};
			expr = Expr::new(kind);
		}

		Some(Ok(expr))
	}

	fn addition(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.multiplication());

		while some_matches!(self.peek(), TokenKind::Plus | TokenKind::Minus) {
			// consume operator
			let operator =
				self.consume().expect("valid addition operator token");
			let right = expect_some!(self.multiplication());
			let kind = ExprKind::Binary {
				left: expr.into(),
				operator,
				right: right.into(),
			};
			expr = Expr::new(kind);
		}

		Some(Ok(expr))
	}

	fn multiplication(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.unary());

		while some_matches!(self.peek(), TokenKind::Slash | TokenKind::Star) {
			// consume operatorcurly-braced block statement that defines a local scope
			let operator =
				self.consume().expect("valid multiplication operator token");
			let right = expect_some!(self.unary());
			let kind = ExprKind::Binary {
				left: expr.into(),
				operator,
				right: right.into(),
			};
			expr = Expr::new(kind);
		}

		Some(Ok(expr))
	}

	fn unary(&mut self) -> Option<Result<Expr>> {
		if some_matches!(self.peek(), TokenKind::Bang | TokenKind::Minus) {
			// consume operator
			let operator = self.consume().expect("valid unary operator token");
			let right = expect_some!(self.unary());
			let kind = ExprKind::Unary { operator, right: right.into() };
			let expr = Expr::new(kind);

			Some(Ok(expr))
		} else {
			self.call()
		}
	}

	fn call(&mut self) -> Option<Result<Expr>> {
		let mut expr = unwrap!(self.primary());

		loop {
			if some_matches!(self.peek(), TokenKind::LeftParen) {
				expr = unwrap!(self.finish_call(expr));
			} else {
				break;
			}
		}

		Some(Ok(expr))
	}

	fn finish_call(&mut self, callee: Expr) -> Option<Result<Expr>> {
		let _ = unwrap!(self.expect(TokenKind::LeftParen));

		let mut arguments: Vec<Box<Expr>> = Vec::new();

		if !matches!(
			self.peek().expect("valid parameter token").kind,
			TokenKind::RightParen
		) {
			loop {
				if arguments.len() >= MAX_ARGS_COUNT {
					let span = callee.span.union(
						arguments.last().expect("last valid argument").span,
					);
					// TODO: just report error and continue
					return Some(Err(ParseError::ArgumentCountExceeded {
						span,
					}));
				}

				arguments.push(expect_some!(self.expression()).into());

				if !matches!(
					self.peek().expect("valid parameter token").kind,
					TokenKind::Comma
				) {
					break;
				}

				let _ = unwrap!(self.expect(TokenKind::Comma));
			}
		}

		let end = unwrap!(self.expect(TokenKind::RightParen));

		let kind =
			ExprKind::Call { callee: callee.into(), arguments, paren: end };

		Some(Ok(Expr::new(kind)))
	}

	fn primary(&mut self) -> Option<Result<Expr>> {
		// Abort early if `self.peek()` is None/Eof.
		if matches!(
			self.peek()?.kind,
			TokenKind::False
				| TokenKind::True
				| TokenKind::Nil | TokenKind::Number
				| TokenKind::String
		) {
			let literal = self.consume().expect("valid literal token");

			let span = match literal.kind {
				TokenKind::String => {
					literal.span.add_low(1u32.into()).sub_high(1u32.into())
				}
				_ => literal.span,
			};

			let symbol = match self.sess.intern(span) {
				Some(symbol) => symbol,
				None => panic!(
					"Could not find span {} for {} in source",
					span,
					literal.name()
				),
			};

			let kind = ExprKind::Literal { value: literal, symbol };

			return Some(Ok(Expr::new(kind)));
		}

		if some_matches!(self.peek(), TokenKind::Identifier) {
			let identifier = self.consume().expect("valid identifier token");

			let symbol = match self.sess.intern(identifier.span) {
				Some(symbol) => symbol,
				None => panic!(
					"Could not find span {} for {} in source",
					identifier.span,
					identifier.name()
				),
			};

			let kind = ExprKind::Variable { name: identifier, symbol };

			return Some(Ok(Expr::new(kind)));
		}

		if some_matches!(self.peek(), TokenKind::LeftParen) {
			// consume parentheses
			let _ = self.consume().expect("valid left parentheses token");
			let expr = unwrap!(self.expression());
			// TODO: look in stdlib if there is a better way to do this.
			let _ = unwrap!(self.expect(TokenKind::RightParen));
			let kind = ExprKind::Grouping { expression: expr.into() };
			return Some(Ok(Expr::new(kind)));
		}

		// TODO: we can get into an error loop here because we never consume the token which
		// errors. Will probably later be handled by `self.synchronize`.
		// TODO: better location information for EOF
		Some(Err(ParseError::ExpectedExpression {
			got: self
				.peek()
				.unwrap_or_else(|| Token::new(TokenKind::Eof, Span::DUMMY)),
		}))
	}

	fn peek(&mut self) -> Option<Token> {
		let peek = self.cursor.first()?;
		match peek.kind {
			TokenKind::Eof => None,
			_ => Some(peek),
		}
	}

	fn expect(&mut self, kind: TokenKind) -> Option<Result<Token>> {
		Some(match self.peek() {
			// TODO: better location information for EOF
			None => Err(ParseError::UnexpectedToken {
				expected: kind,
				got: Token::new(TokenKind::Eof, Span::DUMMY),
			}),
			Some(t) if t.kind == kind => {
				self.consume().expect("valid token");
				Ok(t)
			}
			Some(x) => {
				Err(ParseError::UnexpectedToken { expected: kind, got: x })
			}
		})
	}

	fn consume(&mut self) -> Option<Token> {
		let b = self.cursor.next()?;
		self.current += 1;
		Some(b)
	}
}

impl<'a> Iterator for Parser<'a> {
	type Item = Result<Stmt>;

	fn next(&mut self) -> Option<Self::Item> {
		self.next_stmt()
	}
}
