use crate::expr::Expr;
use crate::span::Span;
use crate::symbol::Symbol;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Curly-braced block statement that defines a local scope e.g. `{ .. }`.
    Block { statments: Vec<Box<Stmt>> },

    /// Class declaration e.g. `class Test ...`.
    Class {
        name: Token,
        symbol: Symbol,
        superclass: Option<Box<Expr>>,
        methods: Vec<Box<Stmt>>,
    },

    /// An expression e.g. `2 + 2`.
    Expression { expression: Box<Expr> },

    /// Function declaration e.g. `hello_world() { .. }`.
    Function {
        name: Token,
        symbol: Symbol,
        params: Vec<Token>,
        body: Vec<Box<Stmt>>,
    },

    /// If statement e.g. `if (2 == 2) print x; else print y;`.
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    /// Print statement e.g. `print "Hello"`.
    Print { expression: Box<Expr> },

    /// Return statement e.g. `return 2;`.
    Return { keyword: Token, value: Box<Expr> },

    /// Variable declaration e.g. `var x = 2`.
    Var {
        name: Token,
        symbol: Symbol,
        initializer: Option<Box<Expr>>,
    },

    /// While loop e.g. `while (true) { .. }`.
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
}

impl StmtKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Block { .. } => "Block",
            Self::Class { .. } => "Class",
            Self::Expression { .. } => "Expression",
            Self::Function { .. } => "Function",
            Self::If { .. } => "If",
            Self::Print { .. } => "Print",
            Self::Return { .. } => "Return",
            Self::Var { .. } => "Var",
            Self::While { .. } => "While",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn name(&self) -> &'static str {
        self.kind.name()
    }
}

#[rustfmt::skip]
pub trait StmtVisitor {
    type Output;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output {
        match *stmt {
            Stmt {
                span,
                kind: StmtKind::Block {
                    ref statments,
                },
            } => self.visit_block_stmt(span, statments),

            Stmt {
                span,
                kind: StmtKind::Class {
                    name,
                    symbol,
                    ref superclass,
                    ref methods,
                },
            } => self.visit_class_stmt(span, name, symbol, superclass.as_ref(), methods),

            Stmt {
                span,
                kind: StmtKind::Expression {
                    ref expression,
                },
            } => self.visit_expression_stmt(span, expression),

            Stmt {
                span,
                kind: StmtKind::Function {
                    name,
                    symbol,
                    ref params,
                    ref body,
                },
            } => self.visit_function_stmt(span, name, symbol, params, body),

            Stmt {
                span,
                kind: StmtKind::If {
                    ref condition,
                    ref then_branch,
                    ref else_branch,
                },
            } => self.visit_if_stmt(span, condition, then_branch, else_branch.as_ref()),

            Stmt {
                span,
                kind: StmtKind::Print {
                    ref expression,
                },
            } => self.visit_print_stmt(span, expression),

            Stmt {
                span,
                kind: StmtKind::Return {
                    keyword,
                    ref value,
                },
            } => self.visit_return_stmt(span, keyword, value),

            Stmt {
                span,
                kind: StmtKind::Var {
                    name,
                    symbol,
                    ref initializer,
                },
            } => self.visit_var_stmt(span, name, symbol, initializer.as_ref()),

            Stmt {
                span,
                kind: StmtKind::While {
                    ref condition,
                    ref body,
                },
            } => self.visit_while_stmt(span, condition, body),
        }
    }

    fn visit_block_stmt(
        &mut self,
        span: Span,
        statements: &[Box<Stmt>]
    ) -> Self::Output;

    fn visit_class_stmt(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        superclass: Option<&Box<Expr>>,
        methods: &[Box<Stmt>]
    ) -> Self::Output;

    fn visit_expression_stmt(
        &mut self,
        span: Span,
        expression: &Expr
    ) -> Self::Output;

    fn visit_function_stmt(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        params: &[Token],
        body: &[Box<Stmt>]
    ) -> Self::Output;

    fn visit_if_stmt(
        &mut self,
        span: Span,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Box<Stmt>>
    ) -> Self::Output;

    fn visit_print_stmt(
        &mut self,
        span: Span,
        expression: &Expr
    ) -> Self::Output;

    fn visit_return_stmt(
        &mut self,
        span: Span,
        keyword: Token,
        value: &Expr
    ) -> Self::Output;

    fn visit_var_stmt(
        &mut self,
        span: Span,
        name: Token,
        symbol: Symbol,
        initializer: Option<&Box<Expr>>
    ) -> Self::Output;

    fn visit_while_stmt(
        &mut self,
        span: Span,
        condition: &Expr,
        body: &Stmt
    ) -> Self::Output;
}
