use std::ops::Range;

use crate::span::Span;

#[derive(Debug, Clone, Default)]
pub struct Source {
	sources: Vec<String>,
}

impl Source {
	pub const fn new() -> Self {
		Self { sources: Vec::new() }
	}

	pub fn add(&mut self, source: String) {
		self.sources.push(source)
	}

	pub fn resolve_range(&self, range: Range<usize>) -> Option<&str> {
		Some(&self.sources.last()?[range])
	}

	pub fn resolve(&self, span: Span) -> Option<&str> {
		self.resolve_range(span.into())
	}
}
