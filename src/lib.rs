//! Implementation based on the wasm-core-1 W3C standard

#![feature(non_modrs_mods)]
#![feature(slice_patterns)]
#![feature(macro_at_most_once_rep)]

#[macro_use]
extern crate nom;

pub mod structure;
pub mod decoding;
pub mod validation;
pub mod execution;


