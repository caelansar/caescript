use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

use super::{builtin::new_builtins, object::Object};

/// Environment holds a store of kv pairs and a pointer to an "outer", enclosing environment
#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: new_builtins(),
            outer: None,
        }
    }

    pub fn from_store(store: HashMap<String, Object>) -> Self {
        Self { store, outer: None }
    }

    pub fn enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: impl Borrow<str>) -> Option<Object> {
        match self.store.get(name.borrow()) {
            Some(obj) => Some(obj.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|outer| outer.borrow_mut().get(name.borrow())),
        }
    }

    pub fn set_self(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn set(&mut self, name: String, value: Object) {
        // key exists in outer env but not in inner
        // should update outer env
        let outer = self
            .outer
            .as_ref()
            .and_then(|ref outer| outer.borrow_mut().get(name.clone()).map(|v| v.clone()));

        let inner = self.store.get(&name).map(|v| v.clone());

        if inner.is_none() && outer.is_some() {
            self.outer
                .as_ref()
                .map(|ref outer| outer.borrow_mut().set(name.clone(), value.clone()));
        } else {
            self.set_self(name, value)
        }
    }
}

#[test]
fn env_should_work() {
    let mut env = Environment::new();

    assert_eq!(None, env.get("1"));
    env.set("1".to_string(), Object::Int(1));
    assert_eq!(Some(Object::Int(1)), env.get("1"));

    let outer = Rc::new(RefCell::new(env));
    let mut env1 = Environment::enclosed(outer.clone());
    env1.set("2".to_string(), Object::Null);
    assert_eq!(Some(Object::Int(1)), env1.get("1"));
    assert_eq!(Some(Object::Null), env1.get("2"));

    // make sure value in outer is Object::Int(1) for the first time
    assert_eq!(Some(Object::Int(1)), outer.clone().borrow_mut().get("1"));
    env1.set("1".to_string(), Object::Int(2));
    assert_eq!(Some(Object::Int(2)), env1.get("1"));
    // the value in outer env should be updated
    assert_eq!(Some(Object::Int(2)), outer.clone().borrow_mut().get("1"));
    // should not set set value for self hashmap
    assert_eq!(None, env1.store.get("1"))
}
