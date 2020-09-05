use crate::expr::Expr;
use crate::span::Span;
use crate::symbol::Symbol;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Curly-braced block statement that defines a local scope e.g. `{ .. }`.
    Block {
        statments: Vec<Box<Stmt>>,
    },

    //Class {
    //    name: Spanned<Token>,
    //    superclass: Option<Box<Expr>>,
    //    methods: Vec<Box<Stmt>>,
    //},
    Expression {
        expression: Box<Expr>,
    },

    //Function {
    //    name: Spanned<Token>,
    //    params: Vec<Spanned<Token>>,
    //    body: Vec<Box<Stmt>>,
    //},

    /// If statement e.g. `if (2 == 2) .. else .. `
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    /// Print statement e.g. `print "Hello"`.
    Print {
        expression: Box<Expr>,
    },

    //Return {
    //    // TODO: check if either or both
    //    keyword: Spanned<Token>,
    //    value: Box<Expr>,
    //},
    /// Variable declaration e.g. `var x = 2`.
    Var {
        name: Token,
        symbol: Symbol,
        initializer: Option<Box<Expr>>,
    },

    /// While loop e.g. `while (true) { .. }`
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
}

impl StmtKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Block { .. } => "Block",
            Self::Expression { .. } => "Expression",
            Self::If { .. } => "If",
            Self::Print { .. } => "Print",
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
