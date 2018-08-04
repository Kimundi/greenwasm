extern crate greenwasm;

use greenwasm::structure::types::*;
use greenwasm::structure::modules::*;
use greenwasm::structure::instructions::Instr::*;
use greenwasm::structure::instructions::*;
use greenwasm::binary_format::parse_binary_format;
use greenwasm::validation::*;

fn diff_print<T: ::std::fmt::Debug>(value_is: &T, value_should: &T) -> String {
    let value_is = format!("{:#?}", value_is);
    let value_should = format!("{:#?}", value_should);

    let a = value_is.as_bytes();
    let b = value_should.as_bytes();

    let mut i = 0;
    let mut j = 0;
    for (&a, &b) in a.iter().zip(b.iter()) {
        if a == b {
            i += 1;
            if a == b'\n' {
                j = i;
            }
        } else {
            break;
        }
    }

    let value_is = &value_is[j..];
    let value_should = &value_should[j..];

    let p = if j != 0 { "...\n" } else { "" };

    format!("Is:\n{}{}\nShould:\n{}{}", p, value_is, p, value_should)
}

macro_rules! test_file {
    (@ $name:ident, $path:expr, $module:expr) => (
        #[test]
        fn $name() {
            let file = std::fs::read($path).unwrap();
            let (module, _custom_sections) = parse_binary_format(&file).unwrap();
            if let Some(ref_module) = $module {
                assert!(module == ref_module, "{}", diff_print(&module, &ref_module));
            }

            let validation_result = validate::module(&Ctx::new(), &module).unwrap();

            println!("Is valid with {:?}", validation_result);
        }
    );
    ($name:ident, $path:expr) => (
        test_file!(@ $name, $path, None);
    );
    ($name:ident, $path:expr, $module:expr) => (
        test_file!(@ $name, $path, Some($module));
    )
}

test_file!(factorial, "tests/factorial.wasm", Module {
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

test_file!(stuff, "tests/stuff.wasm", Module {
    types: vec![
        FuncType {
            args: vec![ValType::I32],
            results: vec![ValType::I32],
        },
        FuncType {
            args: vec![ValType::F32],
            results: vec![],
        },
        FuncType {
            args: vec![],
            results: vec![],
        },
    ],
    funcs: vec![
        Func {
            type_: 2,
            locals: vec![],
            body: Expr {
                body: vec![
                ]
            },
        },
        Func {
            type_: 1,
            locals: vec![],
            body: Expr {
                body: vec![
                    I32Const(42),
                    Drop,
                ]
            },
        },
    ],
    tables: vec![
        Table {
            type_: TableType {
                limits: Limits {
                    min: 0,
                    max: Some(1),
                },
                elemtype: ElemType::AnyFunc,
            }
        },
    ],
    mems: vec![
        Mem {
            type_: MemType {
                limits: Limits {
                    min: 1,
                    max: Some(1),
                }
            }
        }
    ],
    globals: vec![
    ],
    elem: vec![
    ],
    data: vec![
        Data {
            data: 0,
            offset: Expr {
                body: vec![
                    I32Const(0),
                ]
            },
            init: vec![
                b'h',
                b'i',
            ],
        },
    ],
    start: Some(Start{ func: 1 }),
    imports: vec![
        Import {
            module: "foo".to_string(),
            name: "bar".to_string(),
            desc: ImportDesc::Func(1),
        },
    ],
    exports: vec![
        Export {
            name: "e".to_string(),
            desc: ExportDesc::Func(1),
        },
    ],
});

test_file!(fuzz0, "tests/fuzz0.wasm");
test_file!(pong, "tests/pong.wasm");
test_file!(function_space, "tests/function_space.wasm");
test_file!(parser_abort, "tests/parser_abort.wasm");
