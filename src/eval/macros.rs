#[macro_export]
macro_rules! obj_operator {
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
