use super::object::Object;

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

impl std::ops::Add for Object {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, +, Int, Float, String)
    }
}

impl std::ops::Sub for Object {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, -, Int, Float)
    }
}

impl std::ops::Mul for Object {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, *, Int, Float)
    }
}

impl std::ops::Div for Object {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, /, Int, Float)
    }
}

impl std::ops::Rem for Object {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, %, Int, Float)
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
