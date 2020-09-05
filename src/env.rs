use std::collections::HashMap;

use crate::interpret::Value;
use crate::symbol::Symbol;

#[derive(Debug, Default)]
pub struct Scope {
    values: HashMap<Symbol, Value>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, symbol: Symbol, value: Value) {
        self.values.insert(symbol, value);
    }

    pub fn assign(&mut self, symbol: Symbol, value: Value) -> bool {
        if self.values.contains_key(&symbol) {
            self.values.insert(symbol, value);
            true
        } else {
            false
        }
    }

    pub fn get(&mut self, symbol: Symbol) -> Option<Value> {
        self.values.get(&symbol).map(|v| v.clone())
    }

    pub fn contains(&self, symbol: Symbol) -> bool {
        self.values.contains_key(&symbol)
    }
}

#[derive(Debug, Default)]
pub struct Environment {
    scopes: Vec<Scope>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
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
        self.scopes
            .iter_mut()
            .rev()
            .map(|s| s.get(symbol))
            .find_map(|v| v)
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
