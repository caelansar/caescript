use std::{collections::HashMap, fmt::Display, sync::OnceLock};

use super::object::Object;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Builtin {
    Len,
    Puts,
    Push,
    First,
    Last,
    Rest,
}

pub static BUILTINS: OnceLock<Vec<(String, BuiltinFn)>> = OnceLock::new();

pub type BuiltinFn = fn(Vec<Object>) -> Object;

pub fn default_builtins() -> Vec<(String, BuiltinFn)> {
    let mut builtins: Vec<(String, BuiltinFn)> = Vec::new();

    builtins.push((Builtin::Len.to_string(), len));
    builtins.push((Builtin::Puts.to_string(), puts));
    builtins.push((Builtin::Push.to_string(), push));
    builtins.push((Builtin::First.to_string(), first));
    builtins.push((Builtin::Last.to_string(), last));
    builtins.push((Builtin::Rest.to_string(), rest));

    builtins
}

pub fn update_builtins(key: String, f: BuiltinFn) -> Vec<(String, BuiltinFn)> {
    let mut builtins = default_builtins();

    builtins
        .iter_mut()
        .filter(|x| x.0 == key)
        .for_each(|x| x.1 = f);

    builtins
}

impl From<String> for Builtin {
    fn from(value: String) -> Self {
        match value.as_str() {
            "len" => Self::Len,
            "puts" => Self::Puts,
            "push" => Self::Push,
            "first" => Self::First,
            "last" => Self::Last,
            "rest" => Self::Rest,
            _ => unreachable!(),
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Len => f.write_str("len"),
            Self::Puts => f.write_str("puts"),
            Self::Push => f.write_str("push"),
            Self::First => f.write_str("first"),
            Self::Last => f.write_str("last"),
            Self::Rest => f.write_str("rest"),
        }
    }
}

impl Builtin {
    pub fn iterator() -> impl Iterator<Item = Self> {
        [
            Self::Len,
            Self::Puts,
            Self::Push,
            Self::First,
            Self::Last,
            Self::Rest,
        ]
        .iter()
        .copied()
    }

    pub fn call(&self, args: Vec<Object>) -> Object {
        BUILTINS
            .get()
            .unwrap()
            .iter()
            .find(|x| x.0 == self.to_string())
            .map(|f| f.1(args.clone()))
            .unwrap()
    }
}

pub fn new_builtins() -> HashMap<String, Object> {
    let mut map = HashMap::new();
    Builtin::iterator().for_each(|builtin| {
        map.insert(builtin.to_string(), Object::Builtin(builtin));
    });
    map
}

fn len(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::String(s) => Object::Int(s.0.len() as i64),
        Object::Array(a) => Object::Int(a.len() as i64),
        Object::Hash(h) => Object::Int(h.len() as i64),
        _ => unreachable!(),
    }
}

fn puts(args: Vec<Object>) -> Object {
    args.iter().for_each(|a| println!("{}", a.to_string()));
    Object::Null
}

fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!("invalid args number: {:?}", args));
    }

    let mut args = args.clone();
    let obj = args.pop().unwrap();
    let arr = args.pop().unwrap();

    if let Object::Array(mut arr) = arr {
        arr.push(obj);
        Object::Array(arr)
    } else {
        Object::Error("not array type".into())
    }
}

fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("invalid args number: {:?}", args));
    }

    let mut args = args.clone();
    let arr = args.pop().unwrap();

    if let Object::Array(arr) = arr {
        arr.first().map(|x| x.clone()).unwrap_or(Object::Null)
    } else {
        Object::Error("not array type".into())
    }
}

fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("invalid args number".into());
    }

    let mut args = args.clone();
    let arr = args.pop().unwrap();

    if let Object::Array(arr) = arr {
        arr.last().map(|x| x.clone()).unwrap_or(Object::Null)
    } else {
        Object::Error("not array type".into())
    }
}

fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("invalid args number".into());
    }

    let mut args = args.clone();
    let arr = args.pop().unwrap();

    if let Object::Array(arr) = arr {
        arr.split_first()
            .map(|(_, x)| Object::Array(Vec::from(x.clone())))
            .unwrap_or(Object::Null)
    } else {
        Object::Error("not array type".into())
    }
}
