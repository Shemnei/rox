use std::fmt;

use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
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

    Invalid(u8),

    Eof,
}

impl TokenKind {
    pub fn name(&self) -> &'static str {
        use self::TokenKind::*;

        match self {
            LeftParen => "LeftParen",
            RightParen => "RightParen",
            LeftBrace => "LeftBrace",
            RightBrace => "RightBrace",
            Comma => "Comma",
            Dot => "Dot",
            Minus => "Minus",
            Plus => "Plus",
            Semicolon => "Semicolon",
            Slash => "Slash",
            Star => "Star",
            Bang => "Bang",
            BangEqual => "BangEqual",
            Equal => "Equal",
            EqualEqual => "EqualEqual",
            Greater => "Greater",
            GreaterEqual => "GreaterEqual",
            Less => "Less",
            LessEqual => "LessEqual",
            Identifier => "Identifier",
            String => "String",
            Number => "Number",
            And => "And",
            Class => "Class",
            Else => "Else",
            False => "False",
            Fun => "Fun",
            For => "For",
            If => "If",
            Nil => "Nil",
            Or => "Or",
            Print => "Print",
            Return => "Return",
            Super => "Super",
            This => "This",
            True => "True",
            Var => "Var",
            While => "While",
            Invalid(_) => "Invalid",
            Eof => "Eof",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let TokenKind::Invalid(byte) = self {
            if let Some(c) = std::char::from_u32(*byte as u32) {
                return write!(f, "Invalid({})", c);
            }
        }
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { span, kind }
    }

    pub fn name(&self) -> &'static str {
        self.kind.name()
    }
}
