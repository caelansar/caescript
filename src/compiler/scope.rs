use crate::{code, eval::object};

#[derive(Debug, Default, Clone)]
pub(super) struct CompilationScope {
    pub(super) instructions: object::Instructions,
    pub(super) last: Option<EmittedInstruction>,
    pub(super) prev: Option<EmittedInstruction>,
    pub(super) is_loop: bool,
}

#[derive(Debug, Clone)]
pub struct EmittedInstruction {
    pub(super) op: code::Op,
    pub(super) pos: usize,
}

impl EmittedInstruction {
    pub fn new(op: code::Op, pos: usize) -> Self {
        Self { op, pos }
    }
}
