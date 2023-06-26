use crate::code;

#[derive(Debug)]
pub struct Frame {
    func: code::Instructions,
    pub ip: usize,
    pub bp: usize,
}

impl Frame {
    pub fn new(func: code::Instructions, bp: usize) -> Self {
        Self { func, bp, ip: 0 }
    }

    pub fn instructions(&self) -> &code::Instructions {
        &self.func
    }
}
