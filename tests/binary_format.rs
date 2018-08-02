extern crate greenwasm;

use greenwasm::structure::types::*;
use greenwasm::structure::modules::*;
use greenwasm::structure::instructions::Instr::*;
use greenwasm::structure::instructions::*;
use greenwasm::binary_format::parse_binary_format;
use greenwasm::validation::*;

macro_rules! test_file {
    ($name:ident, $path:expr, $module:expr) => (
        #[test]
        fn $name() {
            let file = std::fs::read($path).unwrap();
            let (module, _custom_sections) = parse_binary_format(&file).unwrap();
            assert_eq!(module, $module);

            let validation_result = validate::module(&Ctx::new(), &module).unwrap();

        }
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
