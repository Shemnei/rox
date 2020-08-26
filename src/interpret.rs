use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt;
use std::ops::*;

use crate::expr::Expr;
use crate::span::{Pos, Span};
use crate::token::Token;

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    InvalidOperation {
        span: Span,
        msg: String,
    },

    UnexpectedToken {
        span: Span,
        expected: &'static str,
        got: Token,
    },

    UnexpectedExpression {
        span: Span,
        expected: &'static str,
        got: &'static str,
    },
}

impl RuntimeError {
    pub fn with_span(mut self, sp: Span) -> Self {
        match self {
            Self::InvalidOperation { ref mut span, .. } => *span = sp,
            Self::UnexpectedToken { ref mut span, .. } => *span = sp,
            Self::UnexpectedExpression { ref mut span, .. } => *span = sp,
        }
        self
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidOperation { span, msg } => write!(f, "[{}]: {}", span, msg),

            Self::UnexpectedToken {
                span,
                expected,
                got,
            } => write!(
                f,
                "[{}]: Expected token {} but got {} instead",
                span, expected, got
            ),

            Self::UnexpectedExpression {
                span,
                expected,
                got,
            } => write!(
                f,
                "[{}]: Expected expression {} but got {} instead",
                span, expected, got
            ),
        }
    }
}

impl std::error::Error for RuntimeError {}

fn join(slices: &[&str]) -> String {
    slices.concat()
}

// TODO: check if it even helps as a ref is 16 bytes big.
#[derive(Debug, PartialEq)]
pub enum CowStr<'a> {
    Owned(String),
    Borrowed(Vec<&'a str>),
}

impl<'a> CowStr<'a> {
    pub fn into_owned(self) -> Self {
        match self {
            Self::Owned(_) => self,
            Self::Borrowed(slices) => Self::Owned(join(&slices)),
        }
    }
}

impl<'a> Add for CowStr<'a> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Owned(so1), Self::Owned(so2)) => Self::Owned(so1 + &so2),

            (Self::Borrowed(sb1), Self::Borrowed(mut sb2)) => {
                let mut joined = sb1;
                joined.append(&mut sb2);
                Self::Borrowed(joined)
            }

            (Self::Owned(so1), Self::Borrowed(sb1)) => Self::Owned(so1 + &join(&sb1)),

            (Self::Borrowed(sb1), Self::Owned(so1)) => Self::Owned(join(&sb1) + &so1),
        }
    }
}

impl<'a> fmt::Display for CowStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Owned(s) => write!(f, "{}", s),
            Self::Borrowed(slices) => write!(f, "{}", join(slices)),
        }
    }
}

// TODO: if CowStr stays implement custom PartialEq (check actual strings not spans).

// FIXME: the lox specs define NaN == NaN as true, so we need to implement
// a custom PartialEq.
#[derive(Debug)]
pub enum Value<'a> {
    String(CowStr<'a>),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl<'a> Value<'a> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::String(_) => "String",
            Self::Number(_) => "Number",
            Self::Boolean(_) => "Boolean",
            Self::Nil => "Nil",
        }
    }
}

pub trait TryAdd {
    type Output;
    type Error: std::error::Error;

    fn try_add(self, rhs: Self) -> std::result::Result<Self::Output, Self::Error>;
}

impl<'a> TryAdd for Value<'a> {
    type Output = Self;
    type Error = RuntimeError;

    fn try_add(self, rhs: Self) -> Result<Self::Output> {
        match (self, rhs) {
            (Self::String(s1), Self::String(s2)) => Ok(Self::String(s1 + s2)),
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 + n2)),

            (lhs, rhs) => Err(RuntimeError::InvalidOperation {
                span: Span::DUMMY,
                msg: format!(
                    "addition not implemented for {} and {}",
                    lhs.name(),
                    rhs.name()
                ),
            }),
        }
    }
}

pub trait TrySub {
    type Output;
    type Error: std::error::Error;

    fn try_sub(self, rhs: Self) -> std::result::Result<Self::Output, Self::Error>;
}

impl<'a> TrySub for Value<'a> {
    type Output = Self;
    type Error = RuntimeError;

    fn try_sub(self, rhs: Self) -> Result<Self::Output> {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 - n2)),

            (lhs, rhs) => Err(RuntimeError::InvalidOperation {
                span: Span::DUMMY,
                msg: format!(
                    "subtraction not implemented for {} and {}",
                    lhs.name(),
                    rhs.name()
                ),
            }),
        }
    }
}

pub trait TryDiv {
    type Output;
    type Error: std::error::Error;

    fn try_div(self, rhs: Self) -> std::result::Result<Self::Output, Self::Error>;
}

impl<'a> TryDiv for Value<'a> {
    type Output = Self;
    type Error = RuntimeError;

    fn try_div(self, rhs: Self) -> Result<Self::Output> {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 / n2)),

            (lhs, rhs) => Err(RuntimeError::InvalidOperation {
                span: Span::DUMMY,
                msg: format!(
                    "division not implemented for {} and {}",
                    lhs.name(),
                    rhs.name()
                ),
            }),
        }
    }
}

pub trait TryMul {
    type Output;
    type Error: std::error::Error;

    fn try_mul(self, rhs: Self) -> std::result::Result<Self::Output, Self::Error>;
}

impl<'a> TryMul for Value<'a> {
    type Output = Self;
    type Error = RuntimeError;

    fn try_mul(self, rhs: Self) -> Result<Self::Output> {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 * n2)),

            (lhs, rhs) => Err(RuntimeError::InvalidOperation {
                span: Span::DUMMY,
                msg: format!(
                    "multiplication not implemented for {} and {}",
                    lhs.name(),
                    rhs.name()
                ),
            }),
        }
    }
}

pub trait TryNeg {
    type Output;
    type Error: std::error::Error;

    fn try_neg(self) -> std::result::Result<Self::Output, Self::Error>;
}

impl<'a> TryNeg for Value<'a> {
    type Output = Self;
    type Error = RuntimeError;

    fn try_neg(self) -> Result<Self::Output> {
        match self {
            Self::Number(n) => Ok(Self::Number(-n)),

            s => Err(RuntimeError::InvalidOperation {
                span: Span::DUMMY,
                msg: format!("unary negation not implemented for {}", s.name()),
            }),
        }
    }
}

pub trait Truthy {
    fn is_truthy(&self) -> bool;
}

impl<'a> Truthy for Value<'a> {
    fn is_truthy(&self) -> bool {
        match self {
            Self::Boolean(b) => *b,
            Self::Nil => false,
            _ => true,
        }
    }
}

impl<'a> Not for Value<'a> {
    type Output = bool;

    fn not(self) -> Self::Output {
        !self.is_truthy()
    }
}

pub trait TryOrd {
    type Error: std::error::Error;

    fn try_cmp(&self, other: &Self) -> std::result::Result<Ordering, Self::Error>;
}

impl<'a> TryOrd for Value<'a> {
    type Error = RuntimeError;

    fn try_cmp(&self, other: &Self) -> Result<Ordering> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => Ok(n1.partial_cmp(n2).unwrap()),

            (lhs, rhs) => Err(RuntimeError::InvalidOperation {
                span: Span::DUMMY,
                msg: format!(
                    "comparison not implemented for {} and {}",
                    lhs.name(),
                    rhs.name()
                ),
            }),
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(s1), Self::String(s2)) => s1 == s2,
            // The Lox specs define NaN == NaN as true.
            (Self::Number(n1), Self::Number(n2)) if n1.is_nan() && n2.is_nan() => true,
            (Self::Number(n1), Self::Number(n2)) => n1 == n2,
            (Self::Boolean(b1), Self::Boolean(b2)) => b1 == b2,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub const fn new() -> Self {
        Self {}
    }

    pub fn interpret<'a>(&self, source: &'a str, expr: &Expr) -> Result<()> {
        let value = self.evaluate(source, expr)?;
        println!("{}", value);
        Ok(())
    }

    fn evaluate<'a>(&self, source: &'a str, expr: &Expr) -> Result<Value<'a>> {
        match expr {
            Expr::Literal { .. } => self.evaluate_literal(source, expr),
            Expr::Unary { .. } => self.evaluate_unary(source, expr),
            Expr::Binary { .. } => self.evaluate_binary(source, expr),
            _ => Err(RuntimeError::UnexpectedExpression {
                span: expr.span(),
                expected: "any".into(),
                got: expr.name().into(),
            }),
        }
    }

    fn evaluate_literal<'a>(&self, source: &'a str, expr: &Expr) -> Result<Value<'a>> {
        if let Expr::Literal { value } = expr {
            match value.item {
                Token::String => {
                    // The span for the string also includes the delimiters (`"`).
                    let val = &source[value.span.low.to_usize() + 1..value.span.high.to_usize() - 1];
                    Ok(Value::String(CowStr::Borrowed(vec![val])))
                }

                Token::Number => {
                    let val = &source[value.span];
                    Ok(Value::Number(val.parse().unwrap()))
                }

                Token::True => Ok(Value::Boolean(true)),

                Token::False => Ok(Value::Boolean(false)),

                Token::Nil => Ok(Value::Nil),

                x => Err(RuntimeError::UnexpectedToken {
                    span: expr.span(),
                    expected: "literal",
                    got: x,
                }),
            }
        } else {
            Err(RuntimeError::UnexpectedExpression {
                span: expr.span(),
                expected: "literal",
                got: expr.name(),
            })
        }
    }

    fn evaluate_unary<'a>(&self, source: &'a str, expr: &Expr) -> Result<Value<'a>> {
        if let Expr::Unary { operator, right } = expr {
            let right = self.evaluate(source, right)?;

            match operator.item {
                Token::Minus => right.try_neg().map_err(|err| err.with_span(expr.span())),

                Token::Bang => Ok(Value::Boolean(!right)),

                x => Err(RuntimeError::UnexpectedToken {
                    span: expr.span(),
                    expected: "unary operator",
                    got: x,
                }),
            }
        } else {
            Err(RuntimeError::UnexpectedExpression {
                span: expr.span(),
                expected: "unary",
                got: expr.name(),
            })
        }
    }

    fn evaluate_binary<'a>(&self, source: &'a str, expr: &Expr) -> Result<Value<'a>> {
        if let Expr::Binary {
            left,
            operator,
            right,
        } = expr
        {
            let left = self.evaluate(source, left)?;
            let right = self.evaluate(source, right)?;

            match operator.item {
                Token::Plus => left
                    .try_add(right)
                    .map_err(|err| err.with_span(expr.span())),

                Token::Minus => left
                    .try_sub(right)
                    .map_err(|err| err.with_span(expr.span())),

                Token::Slash => left
                    .try_div(right)
                    .map_err(|err| err.with_span(expr.span())),

                Token::Star => left
                    .try_mul(right)
                    .map_err(|err| err.with_span(expr.span())),

                Token::Greater => {
                    // left > right
                    let cmp = left
                        .try_cmp(&right)
                        .map_err(|err| err.with_span(expr.span()))?;
                    Ok(Value::Boolean(cmp == Ordering::Greater))
                }

                Token::GreaterEqual => {
                    // left >= right
                    let cmp = left
                        .try_cmp(&right)
                        .map_err(|err| err.with_span(expr.span()))?;
                    Ok(Value::Boolean(
                        cmp == Ordering::Greater || cmp == Ordering::Equal,
                    ))
                }

                Token::Less => {
                    // left < right
                    let cmp = left
                        .try_cmp(&right)
                        .map_err(|err| err.with_span(expr.span()))?;
                    Ok(Value::Boolean(cmp == Ordering::Less))
                }

                Token::LessEqual => {
                    // left <= right
                    let cmp = left
                        .try_cmp(&right)
                        .map_err(|err| err.with_span(expr.span()))?;
                    Ok(Value::Boolean(
                        cmp == Ordering::Less || cmp == Ordering::Equal,
                    ))
                }

                Token::BangEqual => Ok(Value::Boolean(left != right)),

                Token::EqualEqual => Ok(Value::Boolean(left == right)),

                x => Err(RuntimeError::UnexpectedToken {
                    span: expr.span(),
                    expected: "binary operator",
                    got: x,
                }),
            }
        } else {
            Err(RuntimeError::UnexpectedExpression {
                span: expr.span(),
                expected: "binary",
                got: expr.name(),
            })
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {}
    }
}
