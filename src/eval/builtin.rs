use std::{collections::HashMap, fmt::Display};

use crate::map;

use super::object::Object;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Builtin {
    Len,
    Puts,
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Len => f.write_str("len"),
            Self::Puts => f.write_str("puts"),
        }
    }
}

impl Builtin {
    pub fn iterator() -> impl Iterator<Item = Self> {
        [Self::Len, Self::Puts].iter().copied()
    }

    pub fn call(&self, args: Vec<Object>) -> Object {
        match self {
            Builtin::Len => len(args),
            Builtin::Puts => puts(args),
        }
    }
}

pub fn new_builtins() -> HashMap<String, Object> {
    map! {
        String::from("len") => Object::Builtin(Builtin::Len),
        String::from("puts")=> Object::Builtin(Builtin::Puts)
    }
}

fn len(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::String(s) => Object::Int(s.0.len() as i64),
        Object::Array(a) => Object::Int(a.len() as i64),
        Object::Hash(h) => Object::Int(h.len() as i64),
        _ => todo!(),
    }
}

fn puts(args: Vec<Object>) -> Object {
    args.iter().for_each(|a| println!("{}", a.to_string()));
    Object::Null
}
