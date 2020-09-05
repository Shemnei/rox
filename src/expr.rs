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

    /// Call expression e.g. `hello_world()`.
    Call {
        callee: Box<Expr>,
        arguments: Vec<Box<Expr>>,
        // closing parentheses
        paren: Token,
    },

    /// Getter expression e.g. `test.var`.
    Get {
        object: Box<Expr>,
        name: Token,
        symbol: Symbol,
    },

    /// Grouped expression with parentheses e.g. `(2)`.
    Grouping { expression: Box<Expr> },

    /// Literal value like numbers, strings, booleans and `nil` e.g. `2`.
    Literal { value: Token, symbol: Symbol },

    /// Logical operators like `and` and `or` e.g. `true or false`.
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    /// Setter expression e.g. `test.var = 2`.
    Set {
        object: Box<Expr>,
        name: Token,
        symbol: Symbol,
        value: Box<Expr>,
    },

    /// Super expression e.g. `super.hello_world()`.
    Super {
        keyword: Token,
        method: Token,
        symbol: Symbol,
    },

    /// Super expression e.g. `this.hello_world()`.
    This {
        keyword: Token,
    },

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
            Self::Call { .. } => "Call",
            Self::Get { .. } => "Get",
            Self::Grouping { .. } => "Grouping",
            Self::Literal { .. } => "Literal",
            Self::Logical { .. } => "Logical",
            Self::Set { .. } => "Set",
            Self::Super { .. } => "Super",
            Self::This { .. } => "This",
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

        ExprKind::Binary { left, right, .. } => left.span.union(right.span),

        ExprKind::Call { callee, paren, .. } => callee.span.union(paren.span),

        ExprKind::Get { object, name, .. } => object.span.union(name.span),

        ExprKind::Grouping { expression } => expression.span,

        ExprKind::Literal { value, .. } => value.span,

        ExprKind::Logical { left, right, .. } => left.span.union(right.span),

        ExprKind::Set { object, value, .. } => object.span.union(value.span),

        ExprKind::Super { keyword, method, .. } => keyword.span.union(method.span),

        ExprKind::This { keyword } => keyword.span,

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

#[rustfmt::skip]
pub trait ExprVisitor {
    type Output;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        match *expr {
            Expr {
                span,
                kind:
                    ExprKind::Assign {
                        name,
                        symbol,
                        ref value,
                    },
            } => self.visit_assign_expr(span, name, symbol, value),

            Expr {
                span,
                kind:
                    ExprKind::Binary {
                        ref left,
                        operator,
                        ref right,
                    },
            } => self.visit_binary_expr(span, left, operator, right),

            Expr {
                span,
                kind: ExprKind::Call {
                    ref callee,
                    ref arguments,
                    paren,
                },
            } => self.visit_call_expr(span, callee, arguments, paren),

            Expr {
                span,
                kind: ExprKind::Get {
                    ref object,
                    name,
                    symbol,
                },
            } => self.visit_get_expr(span, object, name, symbol),

            Expr {
                span,
                kind: ExprKind::Grouping {
                    ref expression
                },
            } => self.visit_grouping_expr(span, expression),

            Expr {
                span,
                kind: ExprKind::Literal {
                    value,
                    symbol
                },
            } => self.visit_literal_expr(span, value, symbol),

            Expr {
                span,
                kind:
                    ExprKind::Logical {
                        ref left,
                        operator,
                        ref right,
                    },
            } => self.visit_logical_expr(span, left, operator, right),

            Expr {
                span,
                kind: ExprKind::Set {
                    ref object,
                    name,
                    symbol,
                    ref value,
                },
            } => self.visit_set_expr(span, object, name, symbol, value),

            Expr {
                span,
                kind: ExprKind::Super {
                    keyword,
                    method,
                    symbol,
                },
            } => self.visit_super_expr(span, keyword, method, symbol),

            Expr {
                span,
                kind: ExprKind::This {
                    keyword,
                },
            } => self.visit_this_expr(span, keyword),

            Expr {
                span,
                kind:
                    ExprKind::Unary {
                        operator,
                        ref right,
                    },
            } => self.visit_unary_expr(span, operator, right),

            Expr {
                span,
                kind: ExprKind::Variable {
                    name,
                    symbol
                },
            } => self.visit_variable_expr(span, name, symbol),
        }
    }

    fn visit_assign_expr(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        value: &Expr
    ) -> Self::Output;

    fn visit_binary_expr(
        &mut self,
        span: Span,
        left: &Expr,
        operator: Token,
        right: &Expr
    ) -> Self::Output;

    fn visit_call_expr(
        &mut self,
        span: Span,
        callee: &Expr,
        arguments: &[Box<Expr>],
        paren: Token
    ) -> Self::Output;

    fn visit_get_expr(
        &mut self,
        span: Span,
        object: &Expr,
        name: Token,
        symbol: Symbol
    ) -> Self::Output;

    fn visit_grouping_expr(
        &mut self,
        span: Span,
        expression: &Expr
    ) -> Self::Output;

    fn visit_literal_expr(
        &mut self,
        span: Span,
        value: Token,
        symbol: Symbol
    ) -> Self::Output;

    fn visit_logical_expr(
        &mut self,
        span: Span,
        left: &Expr,
        operator: Token,
        right: &Expr
    ) -> Self::Output;

    fn visit_set_expr(
        &mut self,
        span: Span,
        object: &Expr,
        name: Token,
        symbol: Symbol,
        value: &Expr
    ) -> Self::Output;

    fn visit_super_expr(
        &mut self,
        span: Span,
        keyword: Token,
        method: Token,
        symbol: Symbol
    ) -> Self::Output;

    fn visit_this_expr(
        &mut self,
        span: Span,
        keyword: Token,
    ) -> Self::Output;

    fn visit_unary_expr(
        &mut self,
        span: Span,
        operator: Token,
        right: &Expr
    ) -> Self::Output;

    fn visit_variable_expr(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol
    ) -> Self::Output;
}
