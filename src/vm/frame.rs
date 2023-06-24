use crate::code;

#[derive(Debug)]
pub struct Frame {
    func: code::Instructions,
    pub ip: usize,
}

impl Frame {
    pub fn new(f: code::Instructions) -> Self {
        Self { func: f, ip: 0 }
    }

    pub fn instructions(&self) -> &code::Instructions {
        &self.func
    }
}
