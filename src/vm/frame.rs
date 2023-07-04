use crate::{code, eval::object};

#[derive(Debug)]
pub struct Frame {
    pub closure: object::Closure,
    pub ip: usize, // instruction pointer
    pub bp: usize, // base pointer
}

impl Frame {
    pub fn new(closure: object::Closure, bp: usize) -> Self {
        Self { closure, bp, ip: 0 }
    }

    pub fn instructions(&self) -> &code::Instructions {
        &self.closure.func.instructions
    }
}
