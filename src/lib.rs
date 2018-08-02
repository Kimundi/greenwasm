//! Implementation based on the wasm-core-1 W3C standard

#![feature(non_modrs_mods)]
#![feature(slice_patterns)]
#![feature(macro_at_most_once_rep)]

#![recursion_limit="256"]

#[macro_use]
extern crate nom;

pub extern crate greenwasm_structure as structure;

pub mod decoding;
pub mod validation;
pub mod execution;
