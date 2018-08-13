#![feature(macro_at_most_once_rep)]
#![feature(core_intrinsics)]
#![feature(int_to_from_bytes)]

const DEBUG_EXECUTION: bool = true;

pub mod runtime_structure;
pub mod modules;
pub mod numerics;
pub mod instructions;
pub mod structure_references;
