#![feature(macro_at_most_once_rep)]
#![feature(core_intrinsics)]
#![feature(int_to_from_bytes)]
#![feature(nll)]

extern crate greenwasm_structure;
extern crate greenwasm_validation;
extern crate frounding;

const DEBUG_EXECUTION: bool = false;

pub mod runtime_structure;
pub mod modules;
pub mod numerics;
pub mod instructions;
