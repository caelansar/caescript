use std::collections::HashMap;

use crate::map;

use super::object::Object;

pub fn new_builtins() -> HashMap<String, Object> {
    map! {
        String::from("len") => Object::Builtin(len),
        String::from("puts")=> Object::Builtin(puts)
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
