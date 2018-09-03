#![feature(nll)]

extern crate binaryen;

use binaryen::tools::*;

fn main() {
    if let (Some(ref seed), Some(ref out)) = (::std::env::args().nth(1), ::std::env::args().nth(2)) {
        let seed = std::fs::read(&seed).unwrap();
        let module = translate_to_fuzz_mvp(&seed);
        let module = module.write();
        std::fs::write(out, &module).unwrap();
        println!("Wrote wasm of size {}", module.len());
    } else {
        println!("Usage: prog <seedinput> <output>");
        ::std::process::exit(1);
    }
}
