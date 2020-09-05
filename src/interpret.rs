use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt;
use std::ops::*;

use crate::expr::{Expr, ExprKind};
use crate::parse;
use crate::rox::Session;
use crate::span::Span;
use crate::stmt::{Stmt, StmtKind};
use crate::token::TokenKind;

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
        got: TokenKind,
    },

    UnexpectedExpression {
        span: Span,
        expected: &'static str,
        got: &'static str,
    },

    UnexpectedStatement {
        span: Span,
        expected: &'static str,
        got: &'static str,
    },

    UndefinedVariable {
        span: Span,
        name: &'static str,
    },

    ResolveError {
        span: Span,
        kind: &'static str,
    },
}

impl RuntimeError {
    pub fn with_span(mut self, sp: Span) -> Self {
        match self {
            Self::InvalidOperation { ref mut span, .. } => *span = sp,
            Self::UnexpectedToken { ref mut span, .. } => *span = sp,
            Self::UnexpectedExpression { ref mut span, .. } => *span = sp,
            Self::UnexpectedStatement { ref mut span, .. } => *span = sp,
            Self::UndefinedVariable { ref mut span, .. } => *span = sp,
            Self::ResolveError { ref mut span, .. } => *span = sp,
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
                span,
                expected,
                got.name()
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

            Self::UnexpectedStatement {
                span,
                expected,
                got,
            } => write!(
                f,
                "[{}]: Expected statement {} but got {} instead",
                span, expected, got
            ),

            Self::UndefinedVariable { span, name } => {
                write!(f, "[{}]: Variable {} not defined", span, name)
            }

            Self::ResolveError { span, kind } => {
                write!(f, "[{}]: Could not resolve {}", span, kind)
            }
        }
    }
}

impl std::error::Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Value {
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

impl TryAdd for Value {
    type Output = Self;
    type Error = RuntimeError;

    fn try_add(self, rhs: Self) -> Result<Self::Output> {
        match (self, rhs) {
            (Self::String(s1), Self::String(s2)) => Ok(Self::String(s1 + &s2)),
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

impl TrySub for Value {
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

impl TryDiv for Value {
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

impl TryMul for Value {
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

impl TryNeg for Value {
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

impl Truthy for Value {
    fn is_truthy(&self) -> bool {
        match self {
            Self::Boolean(b) => *b,
            Self::Nil => false,
            _ => true,
        }
    }
}

impl Not for Value {
    type Output = bool;

    fn not(self) -> Self::Output {
        !self.is_truthy()
    }
}

pub trait TryOrd {
    type Error: std::error::Error;

    fn try_cmp(&self, other: &Self) -> std::result::Result<Ordering, Self::Error>;
}

impl TryOrd for Value {
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

impl PartialEq for Value {
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    sess: &'a mut Session,
}

impl<'a> Interpreter<'a> {
    pub const fn new(session: &'a mut Session) -> Self {
        Self { sess: session }
    }

    pub fn interpret<I>(&mut self, stmts: I) -> Result<()>
    where
        I: Iterator<Item = parse::Result<Stmt>>,
    {
        for stmt in stmts {
            // TODO: better error handling (e.g. dont even parse if error occured, error flag in
            // parser).
            match stmt {
                Ok(stmt) => {
                    eprintln!("{:#?}", stmt);
                    match self.visit_stmt(&stmt) {
                        Ok(_) => {}
                        Err(err) => eprintln!("{}", err),
                    }
                }
                Err(err) => eprintln!("ParseError: {}", err),
            };
        }

        Ok(())
    }
}

use crate::expr::ExprVisitor;
use crate::symbol::Symbol;
use crate::token::Token;

#[rustfmt::skip]
impl<'a> ExprVisitor for Interpreter<'a> {
    type Output = Result<Value>;

    fn visit_assign_expr(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        value: &Expr
    ) -> Self::Output {
        let value = self.visit_expr(value)?;

        if !self.sess.env_mut().assign(symbol, value.clone()) {
            let var_name = self.sess.get(symbol);
            return Err(RuntimeError::UndefinedVariable {
                span: name.span,
                name: var_name,
            });
        }

        Ok(value)
    }

    fn visit_binary_expr(
        &mut self,
        span: Span,
        left: &Expr,
        operator: Token,
        right: &Expr
    ) -> Self::Output {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match operator.kind {
            TokenKind::Plus => left.try_add(right).map_err(|err| err.with_span(span)),

            TokenKind::Minus => left.try_sub(right).map_err(|err| err.with_span(span)),

            TokenKind::Slash => left.try_div(right).map_err(|err| err.with_span(span)),

            TokenKind::Star => left.try_mul(right).map_err(|err| err.with_span(span)),

            TokenKind::Greater => {
                // left > right
                let cmp = left
                    .try_cmp(&right)
                    .map_err(|err| err.with_span(span))?;
                Ok(Value::Boolean(cmp == Ordering::Greater))
            }

            TokenKind::GreaterEqual => {
                // left >= right
                let cmp = left
                    .try_cmp(&right)
                    .map_err(|err| err.with_span(span))?;
                Ok(Value::Boolean(
                    cmp == Ordering::Greater || cmp == Ordering::Equal,
                ))
            }

            TokenKind::Less => {
                // left < right
                let cmp = left
                    .try_cmp(&right)
                    .map_err(|err| err.with_span(span))?;
                Ok(Value::Boolean(cmp == Ordering::Less))
            }

            TokenKind::LessEqual => {
                // left <= right
                let cmp = left
                    .try_cmp(&right)
                    .map_err(|err| err.with_span(span))?;
                Ok(Value::Boolean(
                    cmp == Ordering::Less || cmp == Ordering::Equal,
                ))
            }

            TokenKind::BangEqual => Ok(Value::Boolean(left != right)),

            TokenKind::EqualEqual => Ok(Value::Boolean(left == right)),

            x => Err(RuntimeError::UnexpectedToken {
                span,
                expected: "binary operator",
                got: x,
            }),
        }
    }

    fn visit_call_expr(
        &mut self,
        span: Span,
        callee: &Expr,
        arguments: &[Box<Expr>],
        paren: Token
    ) -> Self::Output {
        todo!()
    }

    fn visit_get_expr(
        &mut self,
        span: Span,
        object: &Expr,
        name: Token,
        symbol: Symbol
    ) -> Self::Output {
        todo!()
    }

    fn visit_grouping_expr(
        &mut self,
        span: Span,
        expression: &Expr
    ) -> Self::Output {
        unimplemented!()
    }

    fn visit_literal_expr(
        &mut self,
        span: Span,
        value: Token,
        symbol: Symbol
    ) -> Self::Output {
        match value.kind {
            TokenKind::String => {
                let val = self.sess.get(symbol);
                Ok(Value::String(val.into()))
            }

            TokenKind::Number => {
                let val = self.sess.get(symbol);
                Ok(Value::Number(val.parse().unwrap()))
            }

            TokenKind::True => Ok(Value::Boolean(true)),

            TokenKind::False => Ok(Value::Boolean(false)),

            TokenKind::Nil => Ok(Value::Nil),

            x => Err(RuntimeError::UnexpectedToken {
                span,
                expected: "literal",
                got: x,
            }),
        }
    }

    fn visit_logical_expr(
        &mut self,
        span: Span,
        left: &Expr,
        operator: Token,
        right: &Expr
    ) -> Self::Output {
        let left = self.visit_expr(left)?;

        match operator.kind {
            TokenKind::Or if left.is_truthy() => return Ok(left),
            TokenKind::And if !left.is_truthy() => return Ok(left),
            _ => {}
        };

        self.visit_expr(right)
    }

    fn visit_set_expr(
        &mut self,
        span: Span,
        object: &Expr,
        name: Token,
        symbol: Symbol,
        value: &Expr
    ) -> Self::Output {
        todo!()
    }

    fn visit_super_expr(
        &mut self,
        span: Span,
        keyword: Token,
        method: Token,
        symbol: Symbol
    ) -> Self::Output {
        todo!()
    }

    fn visit_this_expr(
        &mut self,
        span: Span,
        keyword: Token,
    ) -> Self::Output {
        todo!()
    }

    fn visit_unary_expr(
        &mut self,
        span: Span,
        operator: Token,
        right: &Expr
    ) -> Self::Output {
        let right = self.visit_expr(right)?;

        match operator.kind {
            TokenKind::Minus => right.try_neg().map_err(|err| err.with_span(span)),

            TokenKind::Bang => Ok(Value::Boolean(!right)),

            x => Err(RuntimeError::UnexpectedToken {
                span,
                expected: "unary operator",
                got: x,
            }),
        }
    }

    fn visit_variable_expr(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol
    ) -> Self::Output {
        let var = self.sess.env_mut().get(symbol).map(|v| v.clone());

        match var {
            Some(var) => Ok(var),
            None => {
                let var_name = self.sess.get(symbol);
                Err(RuntimeError::UndefinedVariable {
                    span: name.span,
                    name: var_name.into(),
                })
            }
        }
    }
}

use crate::stmt::StmtVisitor;

impl<'a> StmtVisitor for Interpreter<'a> {
    type Output = Result<()>;

    fn visit_block_stmt(
        &mut self,
        span: Span,
        statements: &[Box<Stmt>]
    ) -> Self::Output {
        self.sess.env_mut().push_scope();

        for s in statements {
            if let Err(err) = self.visit_stmt(s) {
                self.sess.env_mut().pop_scope();
                return Err(err);
            }
        }

        self.sess.env_mut().pop_scope();

        Ok(())
    }

    fn visit_class_stmt(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        superclass: Option<&Box<Expr>>,
        methods: &[Box<Stmt>]
    ) -> Self::Output {
        todo!()
    }

    fn visit_expression_stmt(
        &mut self,
        span: Span,
        expression: &Expr
    ) -> Self::Output {
        self.visit_expr(expression)?;
        Ok(())
    }

    fn visit_function_stmt(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        params: &[Token],
        body: &[Box<Stmt>]
    ) -> Self::Output {
        todo!()
    }

    fn visit_if_stmt(
        &mut self,
        span: Span,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Box<Stmt>>
    ) -> Self::Output {
        if self.visit_expr(condition)?.is_truthy() {
            self.visit_stmt(then_branch)?;
        } else if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)?;
        }

        Ok(())
    }

    fn visit_print_stmt(
        &mut self,
        span: Span,
        expression: &Expr
    ) -> Self::Output {
        let value = self.visit_expr(expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        span: Span,
        keyword: Token,
        value: &Expr
    ) -> Self::Output {
        todo!()
    }

    fn visit_var_stmt(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        initializer: Option<&Box<Expr>>
    ) -> Self::Output {
        // FIXME: not optimal as we first evaluate and then check if the var even exists.
        let value = match initializer {
            Some(initializer) => self.visit_expr(initializer)?,
            None => Value::Nil,
        };

        self.sess.env_mut().define(symbol, value);
        Ok(())
    }

    fn visit_while_stmt(
        &mut self,
        span: Span,
        condition: &Expr,
        body: &Stmt
    ) -> Self::Output {
        while self.visit_expr(condition)?.is_truthy() {
            self.visit_stmt(body)?;
        }

        Ok(())
    }
}
