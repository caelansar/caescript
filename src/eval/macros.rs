#[macro_export]
macro_rules! arithmetic_operator {
    ($l:expr, $r:expr, $op:tt, $($t:ident),*) => {
        match $l {
            $(
                Object::$t(a) => {
                    if let Object::$t(b) = $r {
                        Object::$t(a $op b)
                    } else {
                        Object::Null
                    }
                }
            )*
            _ => Object::Null
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
