use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

impl Symbol {
	const fn new(id: u32) -> Self {
		Self(id)
	}

	pub const fn as_u32(self) -> u32 {
		self.0
	}

	pub const fn as_usize(self) -> usize {
		self.0 as usize
	}
}

#[derive(Default, Debug)]
pub struct Interner {
	values: Vec<&'static str>,
	symbols: HashMap<&'static str, Symbol>,
}

impl Interner {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn prefill(init: &[&'static str]) -> Self {
		Self {
			values: init.into(),
			symbols: init
				.iter()
				.copied()
				.zip((0..).map(Symbol::new))
				.collect(),
		}
	}

	pub fn with_keywords() -> Self {
		let keywords = crate::lex::KEYWORDS
			.iter()
			.map(|&(keyword, _)| keyword)
			.collect::<Vec<_>>();
		Self::prefill(&keywords)
	}

	pub fn intern(&mut self, value: &str) -> Symbol {
		if let Some(&symbol) = self.symbols.get(value) {
			return symbol;
		}

		let symbol = Symbol::new(self.values.len() as u32);
		let string: &'static str =
			unsafe { std::mem::transmute::<&str, &'static str>(value) };
		self.values.push(string);
		self.symbols.insert(string, symbol);

		symbol
	}

	pub fn get(&self, symbol: Symbol) -> &'static str {
		self.values[symbol.as_usize()]
	}
}
