extern crate green_wasm;

use green_wasm::structure::types::*;
use green_wasm::structure::modules::*;
use green_wasm::structure::instructions::Instr::*;
use green_wasm::structure::instructions::*;
use green_wasm::decoding::binary_format::parse_binary_format;

#[test]
fn factorial() {
    let file = std::fs::read("tests/factorial.wasm").unwrap();
    let (module, _custom_sections) = parse_binary_format(&file).unwrap();
    assert_eq!(module, Module {
        types: vec![
            FuncType {
                args: vec![ValType::F64],
                results: vec![ValType::F64],
            },
        ],
        funcs: vec![
            Func {
                type_: 0,
                locals: vec![],
                body: Expr {
                    body: vec![
                        GetLocal(0),
                        F64Const(1.0),
                        F64Lt,
                        IfElse(Some(ValType::F64), vec![
                            F64Const(1.0),
                        ], vec![
                            GetLocal(0),
                            GetLocal(0),
                            F64Const(1.0),
                            F64Sub,
                            Call(0),
                            F64Mul,
                        ])
                    ]
                },
            },
        ],
        tables: vec![
        ],
        mems: vec![
        ],
        globals: vec![
        ],
        elem: vec![
        ],
        data: vec![
        ],
        start: None,
        imports: vec![
        ],
        exports: vec![
            Export {
                name: "fac".to_string(),
                desc: ExportDesc::Func(0),
            },
        ],
    });
}
