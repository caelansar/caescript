use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn outer(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: impl Borrow<str>) -> Option<Object> {
        match self.store.get(name.borrow()).map(|v| v.clone()) {
            Some(obj) => Some(obj),
            None => self.outer.as_ref().and_then(|outer| {
                outer
                    .borrow_mut()
                    .store
                    .get(name.borrow())
                    .map(|v| v.clone())
            }),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}

#[test]
fn env_should_work() {
    let mut env = Environment::new();

    assert_eq!(None, env.get("1"));
    env.set("1".to_string(), Object::Null);
    assert_eq!(Some(Object::Null), env.get("1".to_string()));

    let mut env1 = Environment::outer(Rc::new(RefCell::new(env)));
    env1.set("2".to_string(), Object::Null);
    assert_eq!(Some(Object::Null), env1.get("1".to_string()));
    assert_eq!(Some(Object::Null), env1.get("2".to_string()));
}
