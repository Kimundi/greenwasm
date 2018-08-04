# greenwasm
An implementation of the Webassembly spec in Rust.

The structure of the project tries to follow the structure of the Spec where possible. Current progress:

- [x] __Stucture__ (crate `greenwasm-structure`): Typedefs for Wasm Types, Instructions and Modules.
- [x] __Validation__ (crate `greenwasm-validation`): Validator for a Wasm Module.
- [ ] __Execution__: Interpreter/Vm.
- [x] __Binary-Format__ (crate `greenwasm-binary-format`) parser for the `.wasm` binary format.
- [ ] __Text-Format__: Parser for the `.wat` text format.

In the current version this is mainly a learning exercise, but the long-term goals include:

- __Modularity__: It should be possible to use the parser/validator/typedefs independent from each other. This is already somewhat possible due to the split in different crates.
- __Genericy__: It should be possible to parse/validate independent from the underlying AST format.
- __Performance__: It should be usable for performance-oriented projects.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
