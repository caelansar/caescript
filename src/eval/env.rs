use std::{borrow::Borrow, collections::HashMap};

use super::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: impl Borrow<str>) -> Option<Object> {
        self.store.get(name.borrow()).map(|v| v.clone())
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
}
