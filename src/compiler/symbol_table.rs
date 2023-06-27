use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum Scope {
    Global,
    Local,
}

#[derive(Debug, Clone)]
pub(super) struct Symbol {
    name: String,
    pub scope: Scope,
    pub index: usize,
}

impl Symbol {
    fn new(name: String, scope: Scope, index: usize) -> Self {
        Self { name, scope, index }
    }
}

// A symbol table is a data structure used by compilers to keep track of information about
// the identifier (e.g., functions, variables, which can be called "symbol") defined or used in a program.

// When a program is compiled, the compiler generates a symbol table that contains
// information about the symbols defined in the program, such as their names, types,
// and memory locations. The symbol table is used by the compile to resolve references
// to symbols that are defined previously
#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    pub outer: Option<Box<SymbolTable>>,
    pub count: usize,
}

impl SymbolTable {
    pub(super) fn define(&mut self, name: String) -> Symbol {
        let mut scope = Scope::Global;
        if self.outer.is_some() {
            scope = Scope::Local
        }
        let symbol = Symbol::new(name.clone(), scope, self.count);
        self.store.insert(name, symbol.clone());
        self.count += 1;

        symbol
    }

    pub(super) fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(name) {
            Some(s) => Some(s.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|outer| outer.as_ref().resolve(name)),
        }
    }

    pub fn enclosed(outer: Self) -> Self {
        Self {
            outer: Some(Box::new(outer)),
            ..Self::default()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn resolve_should_work() {
        let mut st = SymbolTable::default();
        st.define("a".into());
        st.define("b".into());

        assert_eq!(0, st.resolve("a").unwrap().index);
        assert_eq!(1, st.resolve("b").unwrap().index);

        let mut st1 = SymbolTable::enclosed(st);
        st1.define("c".into());

        assert_eq!(0, st1.resolve("a").unwrap().index);
        assert_eq!(Scope::Global, st1.resolve("a").unwrap().scope);
        assert_eq!(1, st1.resolve("b").unwrap().index);
        assert_eq!(Scope::Global, st1.resolve("b").unwrap().scope);
        assert_eq!(0, st1.resolve("c").unwrap().index);
        assert_eq!(Scope::Local, st1.resolve("c").unwrap().scope);
    }
}
