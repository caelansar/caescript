use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{code, eval::object};

#[derive(Debug, Clone)]
pub struct Frame {
    pub closure: Rc<RefCell<object::Closure>>,
    pub ip: usize, // instruction pointer
    pub bp: usize, // base pointer
}

impl Frame {
    pub fn new(closure: Rc<RefCell<object::Closure>>, bp: usize) -> Self {
        Self { closure, bp, ip: 0 }
    }

    pub fn instructions(&self) -> code::Instructions {
        self.closure.borrow().func.instructions.clone()
    }

    pub fn borrow(&self) -> Ref<object::Closure> {
        self.closure.borrow()
    }
}
