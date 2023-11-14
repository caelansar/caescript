use super::object::Object;
use crate::eval::object::{BOOL_OBJ_FALSE, BOOL_OBJ_TRUE};

macro_rules! arithmetic_operator {
    ($l:expr, $r:expr, $op:tt, $($t:ident),*) => {
        match $l {
            $(
                Object::$t(a) => {
                    if let Object::$t(b) = $r {
                        Object::$t(a $op b)
                    } else {
                        Object::Error(format!("type mismatch: {} {} {}", a, stringify!($op), $r))
                    }
                }
            )*
            other => Object::Error(format!("unknown operator {} for {:?}", stringify!($op), other))
        }
    };
}

macro_rules! arithmetic_operator_ref {
    ($l:expr, $r:expr, $op:tt, $($t:ident),*) => {
        match $l {
            $(
                Object::$t(a) => {
                    if let Object::$t(ref b) = $r {
                        Object::$t(a $op b)
                    } else {
                        Object::Error(format!("type mismatch: {} {} {}", a, stringify!($op), $r))
                    }
                }
            )*
            other => Object::Error(format!("unknown operator {} for {:?}", stringify!($op), other))
        }
    };
}

#[macro_export]
macro_rules! map {
    ($($k:expr => $v:expr),* $(,)?) => {{
        use std::iter::{Iterator, IntoIterator};
        Iterator::collect(IntoIterator::into_iter([$(($k, $v),)*]))
    }};
}

impl std::ops::Shl for Object {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, <<, Int)
    }
}

impl std::ops::Shr for Object {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, >>, Int)
    }
}

impl std::ops::BitAnd for Object {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, &, Int)
    }
}

impl std::ops::BitOr for Object {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, |, Int)
    }
}

impl std::ops::BitXor for Object {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, ^, Int)
    }
}

impl std::ops::Add for &Object {
    type Output = Object;

    fn add(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, +, Int, Float, String)
    }
}

impl std::ops::Sub for &Object {
    type Output = Object;

    fn sub(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, -, Int, Float)
    }
}

impl std::ops::Mul for &Object {
    type Output = Object;

    fn mul(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, *, Int, Float)
    }
}

impl std::ops::Div for &Object {
    type Output = Object;

    fn div(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, /, Int, Float)
    }
}

impl std::ops::Rem for &Object {
    type Output = Object;

    fn rem(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, %, Int, Float)
    }
}

impl std::ops::Not for &Object {
    type Output = Object;

    fn not(self) -> Self::Output {
        match self {
            Object::Bool(b) => (!b).into(),
            Object::Null => BOOL_OBJ_TRUE,
            Object::Int(i) => Object::Int(!*i),
            _ => BOOL_OBJ_FALSE,
        }
    }
}

impl std::ops::Neg for &Object {
    type Output = Object;

    fn neg(self) -> Self::Output {
        match self {
            Object::Int(i) => Object::Int(-*i),
            Object::Float(f) => Object::Float(-*f),
            _ => Object::Error(format!("unknown operator: -{}", self)),
        }
    }
}

impl std::ops::Shl for &Object {
    type Output = Object;

    fn shl(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, <<, Int)
    }
}

impl std::ops::Shr for &Object {
    type Output = Object;

    fn shr(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, >>, Int)
    }
}

impl std::ops::BitAnd for &Object {
    type Output = Object;

    fn bitand(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, &, Int)
    }
}

impl std::ops::BitOr for &Object {
    type Output = Object;

    fn bitor(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, |, Int)
    }
}

impl std::ops::BitXor for &Object {
    type Output = Object;

    fn bitxor(self, rhs: Self) -> Self::Output {
        arithmetic_operator_ref!(self, rhs, ^, Int)
    }
}
