use crate::span::Span;
use crate::symbol::Symbol;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Variable assignment e.g. `x = 20`.
    Assign {
        name: Token,
        symbol: Symbol,
        value: Box<Expr>,
    },

    /// Binary expression e.g. `2 + 2`.
    /// Infix arithmetic: `+`, `-`, `*`, `/`.
    /// Logic: `==`, `!=`, `<`, `<=`, `>`, `>=`.
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    //Call {
    //    callee: Box<Expr>,
    //    paren: Span<Token>,
    //    arguments: Vec<Box<Expr>>,
    //},

    //Get {
    //    object: Box<Expr>,
    //    name: Span<Token>,
    //},
    /// Grouped expression with parentheses e.g. `(2)`.
    Grouping { expression: Box<Expr> },

    /// Literal value like numbers, strings, booleans and `nil` e.g. `2`.
    Literal { value: Token, symbol: Symbol },

    //Logical {
    //    left: Box<Expr>,
    //    operator: Span<Token>,
    //    right: Box<Expr>,
    //},

    //Set {
    //    object: Box<Expr>,
    //    name: Span<Token>,
    //    value: Box<Expr>,
    //},

    //Super {
    //    keyword: Span<Token>,
    //    method: Span<Token>,
    //},

    //This {
    //    keyword: Span<Token>,
    //},
    /// Unary expression like `!` and `-` e.g. `!2`.
    Unary { operator: Token, right: Box<Expr> },

    /// Variable access expressions e.g. `x`.
    Variable { name: Token, symbol: Symbol },
}

impl ExprKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Assign { .. } => "Assign",
            Self::Binary { .. } => "Binary",
            Self::Grouping { .. } => "Grouping",
            Self::Literal { .. } => "Literal",
            Self::Unary { .. } => "Unary",
            Self::Variable { .. } => "Variable",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self {
            span: span(&kind),
            kind,
        }
    }

    pub fn name(&self) -> &'static str {
        self.kind.name()
    }
}

fn span(kind: &ExprKind) -> Span {
    match kind {
        ExprKind::Assign { name, value, .. } => name.span.union(value.span),

        ExprKind::Binary {
            left,
            operator: _,
            right,
        } => left.span.union(right.span),

        ExprKind::Grouping { expression } => expression.span,

        ExprKind::Literal { value, .. } => value.span,

        ExprKind::Unary { operator, right } => operator.span.union(right.span),

        ExprKind::Variable { name, .. } => name.span,
    }
}

pub fn pretty_fmt(out: &mut String, expr: &Expr, source: &str) {
    match &expr.kind {
        ExprKind::Binary {
            ref left,
            operator,
            ref right,
        } => {
            out.push('(');
            out.push_str(&source[operator.span]);
            out.push(' ');
            pretty_fmt(out, left, source);
            out.push(' ');
            pretty_fmt(out, right, source);
            out.push(')');
        }

        ExprKind::Grouping { ref expression } => {
            out.push('(');
            out.push_str("group ");
            pretty_fmt(out, expression, source);
            out.push(')');
        }

        ExprKind::Literal { value, .. } => {
            out.push_str(&source[value.span]);
        }

        ExprKind::Unary {
            operator,
            ref right,
        } => {
            out.push('(');
            out.push_str(&source[operator.span]);
            out.push(' ');
            pretty_fmt(out, right, source);
            out.push(')');
        }

        _ => panic!("Unknown expr {}", expr.kind.name()),
    }
}
