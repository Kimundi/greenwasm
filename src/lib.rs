//! Implementation based on the wasm-core-1 W3C standard

#![feature(non_modrs_mods)]
#![feature(slice_patterns)]
#![feature(macro_at_most_once_rep)]
#![feature(rust_2018_preview)]

pub extern crate greenwasm_structure as structure;
pub extern crate greenwasm_validation as validation;
pub extern crate greenwasm_binary_format as binary_format;

pub mod execution;
