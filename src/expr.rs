use crate::span::Spanned;
use crate::token::Token;

enum Expr {
    ///// Variable assignment e.g. `x = 20`.
    //Assign {
    //    name: Span<Token>,
    //    value: Box<Expr>,
    //},
    /// Binary expression e.g. `2 + 2`.
    /// Infix arithmetic: `+`, `-`, `*`, `/`.
    /// Logic: `==`, `!=`, `<`, `<=`, `>`, `>=`.
    Binary {
        left: Box<Expr>,
        operator: Spanned<Token>,
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
    Literal { value: Spanned<Token> },

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
    Unary {
        operator: Spanned<Token>,
        right: Box<Expr>,
    },
    //Variable {
    //    name: Span<Token>,
    //},
}

fn pretty_fmt(out: &mut String, expr: &Expr, source: &str) {
    match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            out.push('(');
            out.push_str(&source[operator.span]);
            out.push(' ');
            pretty_fmt(out, left, source);
            out.push(' ');
            pretty_fmt(out, right, source);
            out.push(')');
        }

        Expr::Grouping { expression } => {
            out.push('(');
            out.push_str("group ");
            pretty_fmt(out, expression, source);
            out.push(')');
        }

        Expr::Literal { value } => {
            out.push_str(&source[value.span]);
        }

        Expr::Unary { operator, right } => {
            out.push('(');
            out.push_str(&source[operator.span]);
            out.push(' ');
            pretty_fmt(out, right, source);
            out.push(')');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::*;
    use crate::token::Token;

    #[test]
    fn demo_pretty_fmt() {
        let expr = Expr::Binary {
            left: Expr::Unary {
                operator: Span::new(0u32.into(), 1u32.into()).span(Token::Minus),
                right: Expr::Literal {
                    value: Span::new(1u32.into(), 4u32.into()).span(Token::Number),
                }
                .into(),
            }
            .into(),
            operator: Span::new(5u32.into(), 6u32.into()).span(Token::Star),
            right: Expr::Grouping {
                expression: Expr::Literal {
                    value: Span::new(7u32.into(), 12u32.into()).span(Token::Number),
                }
                .into(),
            }
            .into(),
        };
        let source = "-123 * 45.67";

        let mut out = String::new();

        pretty_fmt(&mut out, &expr, &source);

        assert_eq!(&out, "(* (- 123) (group 45.67))");
    }
}
