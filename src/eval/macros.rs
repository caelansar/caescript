use super::object::Object;
use crate::eval::object::{BOOL_OBJ_FALSE, BOOL_OBJ_TRUE};

#[allow(unused_macros)]
macro_rules! arithmetic_operator {
    ($l:expr, $r:expr, $op:tt, $ref:tt, $($t:ident),*) => {
        match ($l, $r) {
            $(
                (Object::$t(a), Object::$t($ref b)) => Object::$t(a $op b),
                (Object::$t(a), b) => Object::Error(format!("type mismatch: {} {} {}", a, stringify!($op), b)),
                (a, Object::$t($ref b)) => Object::Error(format!("type mismatch: {} {} {}", a, stringify!($op), b)),
            )*
            (a, b) => Object::Error(format!("unsupported operator: {} {} {}", a, stringify!($op), b))
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

impl std::ops::Add for &Object {
    type Output = Object;

    fn add(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, +, ref, Int, Float, String)
    }
}

impl std::ops::Sub for &Object {
    type Output = Object;

    fn sub(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, -, ref, Int, Float)
    }
}

impl std::ops::Mul for &Object {
    type Output = Object;

    fn mul(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, *, ref, Int, Float)
    }
}

impl std::ops::Div for &Object {
    type Output = Object;

    fn div(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, /, ref, Int, Float)
    }
}

impl std::ops::Rem for &Object {
    type Output = Object;

    fn rem(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, %, ref, Int, Float)
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
        arithmetic_operator!(self, rhs, <<, ref, Int)
    }
}

impl std::ops::Shr for &Object {
    type Output = Object;

    fn shr(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, >>, ref, Int)
    }
}

impl std::ops::BitAnd for &Object {
    type Output = Object;

    fn bitand(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, &, ref, Int)
    }
}

impl std::ops::BitOr for &Object {
    type Output = Object;

    fn bitor(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, |, ref, Int)
    }
}

impl std::ops::BitXor for &Object {
    type Output = Object;

    fn bitxor(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, ^, ref, Int)
    }
}
