use std::fmt::Display;

pub const BOOL_OBJ_TRUE: Object = Object::Bool(true);
pub const BOOL_OBJ_FALSE: Object = Object::Bool(false);

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        match value {
            true => BOOL_OBJ_TRUE,
            false => BOOL_OBJ_FALSE,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Return(Box<Object>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Float(float) => write!(f, "{}", float),
            Object::Bool(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Return(r) => write!(f, "{}", r.to_string()),
            Object::Null => write!(f, "null"),
        }
    }
}

#[test]
fn object_display_should_work() {
    assert_eq!("123", Object::Int(123).to_string());
    assert_eq!("true", Object::Bool(true).to_string());
    assert_eq!("false", Object::Bool(false).to_string());
    assert_eq!("null", Object::Null.to_string());
}
