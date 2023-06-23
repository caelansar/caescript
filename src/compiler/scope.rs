use crate::code;

use super::EmittedInstruction;

#[derive(Debug, Default, Clone)]
pub(super) struct CompilationScope {
    pub(super) instructions: code::Instructions,
    pub(super) last: Option<EmittedInstruction>,
    pub(super) prev: Option<EmittedInstruction>,
}
