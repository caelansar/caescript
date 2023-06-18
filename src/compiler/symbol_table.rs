use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub(super) enum Scope {
    Global,
    Local,
}

#[derive(Debug, Clone)]
pub(super) struct Symbol {
    name: String,
    scope: Scope,
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
#[derive(Debug, Default)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    count: usize,
}

impl SymbolTable {
    pub(super) fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol::new(name.clone(), Scope::Global, self.count);
        self.store.insert(name, symbol.clone());
        self.count += 1;

        symbol
    }

    pub(super) fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
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
    }
}
