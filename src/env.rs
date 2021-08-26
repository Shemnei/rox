use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::interpret::Value;
use crate::symbol::Symbol;

#[derive(Debug, Default, Clone)]
pub struct Scope {
	values: HashMap<Symbol, Value>,
}

impl Scope {
	pub fn new() -> Self {
		Self { values: HashMap::new() }
	}

	pub fn define(&mut self, symbol: Symbol, value: Value) {
		self.values.insert(symbol, value);
	}

	pub fn assign(&mut self, symbol: Symbol, value: Value) -> bool {
		if let Entry::Vacant(e) = self.values.entry(symbol) {
			e.insert(value);
			true
		} else {
			false
		}
	}

	pub fn get(&mut self, symbol: Symbol) -> Option<Value> {
		self.values.get(&symbol).cloned()
	}

	pub fn contains(&self, symbol: Symbol) -> bool {
		self.values.contains_key(&symbol)
	}
}

#[derive(Debug, Default, Clone)]
pub struct Environment {
	scopes: Vec<Scope>,
}

impl Environment {
	pub fn new() -> Self {
		Self { scopes: vec![Scope::new()] }
	}

	pub fn define(&mut self, symbol: Symbol, value: Value) {
		self.scopes
			.last_mut()
			.expect("no global scope in environment")
			.define(symbol, value)
	}

	pub fn assign(&mut self, symbol: Symbol, value: Value) -> bool {
		for scope in self.scopes.iter_mut().rev() {
			if scope.contains(symbol) {
				return scope.assign(symbol, value);
			}
		}
		false
	}

	pub fn get(&mut self, symbol: Symbol) -> Option<Value> {
		self.scopes.iter_mut().rev().map(|s| s.get(symbol)).find_map(|v| v)
	}

	pub fn peek(&self) -> &Scope {
		self.scopes.last().expect("at least one global scope")
	}

	pub fn pop(&mut self) -> Scope {
		if self.scopes.len() > 1 {
			self.scopes.pop().expect("scope")
		} else {
			panic!("Tried to pop global scope");
		}
	}

	pub fn push(&mut self, scope: Scope) -> &mut Self {
		self.scopes.push(scope);
		self
	}

	pub fn push_scope(&mut self) -> &mut Self {
		self.scopes.push(Scope::new());
		self
	}

	pub fn pop_scope(&mut self) -> &mut Self {
		if self.scopes.len() > 1 {
			self.scopes.pop();
			self
		} else {
			panic!("Tried to pop global scope");
		}
	}
}
