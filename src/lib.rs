#![feature(or_patterns, const_fn)]

// TODO: all extra challenges.

pub mod env;
pub mod expr;
pub mod interpret;
pub mod lex;
pub mod parse;
pub mod rox;
pub mod source;
pub mod span;
pub mod stmt;
pub mod symbol;
pub mod token;

pub use crate::rox::*;
