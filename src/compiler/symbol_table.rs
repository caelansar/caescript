use std::collections::HashMap;

use crate::eval::builtin;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum Scope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
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
    pub(super) free: Vec<Symbol>,
    pub outer: Option<Box<SymbolTable>>,
    pub count: usize,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self::new_with_builtins(builtin::default_builtins)
    }

    pub(crate) fn new_with_builtins(f: impl FnOnce() -> Vec<(String, builtin::BuiltinFn)>) -> Self {
        let mut symbol_table = Self::default();

        builtin::BUILTINS.get_or_init(f);

        if let Some(builtin) = builtin::BUILTINS.get() {
            builtin.iter().enumerate().for_each(|(idx, builtin)| {
                symbol_table.define_builtin(idx, builtin.0.clone());
            });
        }

        symbol_table
    }

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

    pub(super) fn define_builtin(&mut self, idx: usize, name: String) -> Symbol {
        assert!(!name.is_empty());
        let symbol = Symbol::new(name.clone(), Scope::Builtin, idx);
        self.store.insert(name, symbol.clone());

        symbol
    }

    fn define_free(&mut self, free: Symbol) -> Symbol {
        let name = free.name.clone();
        self.free.push(free);

        let symbol = Symbol::new(name.clone(), Scope::Free, self.free.len() - 1);
        self.store.insert(name, symbol.clone());

        symbol
    }

    pub(super) fn define_function(&mut self, name: String) -> Symbol {
        let symbol = Symbol::new(name.clone(), Scope::Function, 0);
        self.store.insert(name, symbol.clone());

        symbol
    }

    pub(super) fn resolve(&mut self, name: impl AsRef<str>) -> Option<Symbol> {
        match self.store.get(name.as_ref()) {
            Some(s) => Some(s.clone()),
            None => {
                let sym = self
                    .outer
                    .as_mut()
                    .and_then(|outer| outer.as_mut().resolve(name))?;

                if sym.scope == Scope::Global || sym.scope == Scope::Builtin {
                    Some(sym)
                } else {
                    Some(self.define_free(sym))
                }
            }
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
