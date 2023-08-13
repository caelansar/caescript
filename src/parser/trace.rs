use std::sync::atomic::{AtomicU32, Ordering};

pub(crate) struct ScopeCall<F: FnOnce()> {
    pub(crate) c: Option<F>,
}

impl<F: FnOnce()> Drop for ScopeCall<F> {
    fn drop(&mut self) {
        self.c.take().unwrap()()
    }
}

#[macro_export]
macro_rules! defer {
    ($f:ident, $exp:expr) => {
        let x = $exp;
        let _scope_call = ScopeCall {
            c: Some(|| -> () { $f(x) }),
        };
    };
}

static TRACE_LEVEL: AtomicU32 = AtomicU32::new(0);

fn ident_level() -> String {
    " ".repeat((TRACE_LEVEL.load(Ordering::Relaxed) - 1) as usize)
}

fn inc_ident() {
    TRACE_LEVEL.fetch_add(1, Ordering::Relaxed);
}

fn dec_ident() {
    TRACE_LEVEL.fetch_sub(1, Ordering::Relaxed);
}

pub fn trace(msg: &str) -> &str {
    inc_ident();
    trace_print(format!("BEGIN {}", msg).as_str());
    msg
}

pub fn untrace(msg: &str) {
    trace_print(format!("END {}", msg).as_str());
    dec_ident()
}

fn trace_print(msg: &str) {
    println!("{}{}", ident_level(), msg)
}
