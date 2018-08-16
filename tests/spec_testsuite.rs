use structure::types::*;
use structure::modules::*;
use structure::instructions::Instr::*;
use structure::instructions::*;
use binary_format::parse_binary_format;
use validation::validate_module;
use execution::modules::instantiation::instantiate_module;
use execution::modules::allocation::*;
use execution::runtime_structure::*;

use wabt::script::*;

use std::fs;
use std::collections::HashMap;

// TODO: solve the issue here
const BLACKLIST: &[&str] = &[
    "globals.wast",
    "linking.wast",
];

type F32 = f32;
type F64 = f64;

macro_rules! match_trace {
    (match $e:expr; $($p:pat => $b:block)*) => {
        match $e {
            $($p => {
                println!("Execute {}...", stringify!($p));
                $b
            })*
        }
    }
}

#[test]
fn run_tests() {
    for dir in fs::read_dir("tests/spec_testsuite").unwrap() {
        let dir = dir.unwrap();
        let path = dir.path();
        let filename = path.file_name().unwrap().to_str().unwrap();

        if path.metadata().unwrap().file_type().is_file() && filename.ends_with(".wast") && !BLACKLIST.contains(&filename)  {
            let source = fs::read(&path).unwrap();

            println!("\nParse {} ...", filename);
            let mut script = ScriptParser::<F32, F64>::from_source_and_name(&source, filename).unwrap();

            println!("Executing {} ...", filename);
            while let Some(Command { line, kind }) = script.next().unwrap() {
                use wabt::script::CommandKind::*;

                match_trace! {
                    match kind;

                    Module {
                        module,
                        name,
                    } => {
                        let bytes = module.into_vec();
                        let name: Option<String> = name;
                        unimplemented!();
                    }
                    AssertReturn {
                        action,
                        expected,
                    } => {
                        unimplemented!();
                    }
                    AssertReturnCanonicalNan {
                        action,
                    } => {
                        unimplemented!();
                    }
                    AssertReturnArithmeticNan {
                        action,
                    } => {
                        unimplemented!();
                    }
                    AssertTrap {
                        action,
                        message,
                    } => {
                        unimplemented!();
                    }
                    AssertInvalid {
                        module,
                        message,
                    } => {
                        let bytes = module.into_vec();
                        unimplemented!();
                    }
                    AssertMalformed {
                        module,
                        message,
                    } => {
                        let bytes = module.into_vec();
                        unimplemented!();
                    }
                    AssertUninstantiable {
                        module,
                        message,
                    } => {
                        let bytes = module.into_vec();
                        unimplemented!();
                    }
                    AssertExhaustion {
                        action,
                    } => {
                        unimplemented!();
                    }
                    AssertUnlinkable {
                        module,
                        message,
                    } => {
                        let bytes = module.into_vec();
                        unimplemented!();
                    }
                    Register {
                        name,
                        as_name,
                    } => {
                        unimplemented!();
                    }
                    PerformAction(action) => {
                        unimplemented!();
                    }
                }
            }
        }
    }
}
