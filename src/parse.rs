use std::fmt;

use crate::expr::Expr;
use crate::lex::Cursor;
use crate::span::*;
use crate::token::Token;

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, Copy)]
pub enum ParseError {
    UnexpectedToken {
        expected: Token,
        got: Spanned<Token>,
    },

    ExpectedExpression {
        got: Spanned<Token>,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken {
                expected,
                got: Spanned {
                    item: Token::Eof, ..
                },
            } => write!(f, "[EOF]: Expected token {} at the end", expected),

            Self::UnexpectedToken { expected, got } => write!(
                f,
                "[{}]: Expected token {} but got {} instead",
                got.span, expected, got.item
            ),

            Self::ExpectedExpression {
                got: Spanned {
                    item: Token::Eof, ..
                },
            } => write!(f, "[EOF]: Expected an expression "),

            Self::ExpectedExpression { got } => write!(
                f,
                "[{}]: Expected an expression but got {} instead",
                got.span, got.item
            ),
        }
    }
}

// TODO: look in stdlib if there is a better way to do this.
macro_rules! unwrap {
    ($or:expr) => {
        match $or? {
            Ok(value) => value,
            Err(err) => return Some(Err(err)),
        }
    };
}

// https://doc.rust-lang.org/src/core/macros/mod.rs.html#254-261
macro_rules! some_matches {
        ($option:expr, $($pattern:pat)|+) => {
            match $option {
                None => false,
                Some(token) => matches!(token.item, $($pattern)|+),
            }
        }
    }

impl std::error::Error for ParseError {}

pub struct Parser<'a> {
    cursor: Cursor<'a, Spanned<Token>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self {
            cursor: Cursor::new(tokens),
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        match self.expression()? {
            Ok(expr) => Some(expr),
            Err(err) => {
                eprintln!("ParseError @ {}", err);
                std::process::exit(128);
            }
        }
    }

    #[allow(dead_code)]
    fn synchronize(&mut self) -> Option<()> {
        // TODO: cleanup
        if matches!(
            self.peek()?.item,
            Token::Class
                | Token::Fun
                | Token::Var
                | Token::For
                | Token::If
                | Token::While
                | Token::Print
                | Token::Return
        ) {
            return Some(());
        }

        let prev = self.consume()?;
        loop {
            if prev.item == Token::Semicolon {
                return Some(());
            }

            if matches!(
                self.peek()?.item,
                Token::Class
                    | Token::Fun
                    | Token::Var
                    | Token::For
                    | Token::If
                    | Token::While
                    | Token::Print
                    | Token::Return
            ) {
                return Some(());
            }
        }
    }

    fn expression(&mut self) -> Option<Result<Expr>> {
        self.equality()
    }

    fn equality(&mut self) -> Option<Result<Expr>> {
        let mut expr = unwrap!(self.comparison());

        while some_matches!(self.peek(), Token::BangEqual | Token::EqualEqual) {
            // consume operator
            let operator = self.consume().expect("valid operator token");
            let right: Expr = unwrap!(self.comparison());
            expr = Expr::Binary {
                left: expr.into(),
                operator,
                right: right.into(),
            };
        }

        Some(Ok(expr))
    }

    fn comparison(&mut self) -> Option<Result<Expr>> {
        let mut expr = unwrap!(self.addition());

        while some_matches!(
            self.peek(),
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual
        ) {
            // consume operator
            let operator = self.consume().expect("valid operator token");
            let right = unwrap!(self.addition());
            expr = Expr::Binary {
                left: expr.into(),
                operator,
                right: right.into(),
            };
        }

        Some(Ok(expr))
    }

    fn addition(&mut self) -> Option<Result<Expr>> {
        let mut expr = unwrap!(self.multiplication());

        while some_matches!(self.peek(), Token::Plus | Token::Minus) {
            // consume operator
            let operator = self.consume().expect("valid operator token");
            let right = unwrap!(self.multiplication());
            expr = Expr::Binary {
                left: expr.into(),
                operator,
                right: right.into(),
            };
        }

        Some(Ok(expr))
    }

    fn multiplication(&mut self) -> Option<Result<Expr>> {
        let mut expr = unwrap!(self.unary());

        while some_matches!(self.peek(), Token::Slash | Token::Star) {
            // consume operator
            let operator = self.consume().expect("valid operator token");
            let right = unwrap!(self.unary());
            expr = Expr::Binary {
                left: expr.into(),
                operator,
                right: right.into(),
            };
        }

        Some(Ok(expr))
    }

    fn unary(&mut self) -> Option<Result<Expr>> {
        if some_matches!(self.peek(), Token::Bang | Token::Minus) {
            // consume operator
            let operator = self.consume().expect("valid operator token");
            let right = unwrap!(self.unary());
            let expr = Expr::Unary {
                operator,
                right: right.into(),
            };

            Some(Ok(expr))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Result<Expr>> {
        if some_matches!(
            self.peek(),
            Token::False | Token::True | Token::Nil | Token::Number | Token::String
        ) {
            return Some(Ok(Expr::Literal {
                value: self.consume().expect("valid literal token"),
            }));
        }

        if some_matches!(self.peek(), Token::LeftParen) {
            // consume parentheses
            let _ = self.consume().expect("valid left parentheses token");
            let expr = unwrap!(self.expression());
            // TODO: look in stdlib if there is a better way to do this.
            if let Err(err) = self.expect(Token::RightParen) {
                return Some(Err(err));
            }
            return Some(Ok(Expr::Grouping {
                expression: expr.into(),
            }));
        }

        // TODO: we can get into an error loop here because we never consume the token which
        // errors. Will probably later be handled by `self.synchronize`.
        // TODO: better location information for EOF
        Some(Err(ParseError::ExpectedExpression {
            got: self.peek().unwrap_or_else(||
                Span::new(u32::max_value().into(), u32::max_value().into()).span(Token::Eof),
            ),
        }))
    }

    fn peek(&mut self) -> Option<Spanned<Token>> {
        self.cursor.first()
    }

    fn expect(&mut self, token: Token) -> Result<Spanned<Token>> {
        match self.peek() {
            // TODO: better location information for EOF
            None => Err(ParseError::UnexpectedToken {
                expected: token,
                got: Span::new(u32::max_value().into(), u32::max_value().into()).span(Token::Eof),
            }),
            Some(t) if t.item == token => {
                self.consume().expect("valid token");
                Ok(t)
            }
            Some(x) => Err(ParseError::UnexpectedToken {
                expected: token,
                got: x,
            }),
        }
    }

    fn consume(&mut self) -> Option<Spanned<Token>> {
        let b = self.cursor.next()?;
        self.current += 1;
        Some(b)
    }
}
