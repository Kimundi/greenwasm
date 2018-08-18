#![feature(fnbox)]

use structure::types::*;
use structure::modules::*;
use structure::instructions::Instr::*;
use structure::instructions::*;
use binary_format::parse_binary_format;
use validation::{validate_module, ValidatedModule};
use execution::modules::instantiation::instantiate_module;
use execution::modules::allocation::*;
use execution::runtime_structure::*;

use wabt::script::*;

use owning_ref::ArcRef;

use std::fs;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::mpsc::{channel, Receiver, Sender};
//use std::boxed::FnBox;
use std::thread;

// TODO: solve the issue here
const BLACKLIST: &[&str] = &[
    "globals.wast",
    "linking.wast",
];

type F32 = f32;
type F64 = f64;

struct StSt<'ast> {
    store: Store<'ast>,
    stack: Stack<'ast>,
    recv: Receiver<CmdFn>,
}
type CmdFn = Box<for<'ast> Fn(&mut Option<StSt<'ast>>) + Send>;

fn store_thread_frame<'ast>(stst: StSt<'ast>)
{
    let mut stst = Some(stst);
    while stst.is_some() {
        match stst.as_ref().unwrap().recv.recv() {
            Ok(cmd) => {
                cmd(&mut stst);
            }
            Err(_) => return,
        }
    }
}

struct StoreCtrl {
    tx: Sender<CmdFn>,
    handle: thread::JoinHandle<()>,
}

impl StoreCtrl {
    fn new() -> Self {
        let (tx, rx) = channel();

        let handle = thread::spawn(|| {
            store_thread_frame(StSt { store: Store::new(), stack: Stack::new(), recv: rx});
        });

        StoreCtrl {
            tx,
            handle,
        }
    }

    fn new_frame<F: Fn(StSt) + Send + 'static>(&self, f: F) {
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.take().unwrap();
            f(stst);
        })).unwrap();
    }
}

#[test]
fn run_tests() {
    let mut successes = 0;
    let mut failures = vec![];

    for dir in fs::read_dir("tests/spec_testsuite").unwrap() {
        let dir = dir.unwrap();
        let path = dir.path();
        let filename = path.file_name().unwrap().to_str().unwrap();

        let sctrl = StoreCtrl::new();

        if path.metadata().unwrap().file_type().is_file() && filename.ends_with(".wast") && !BLACKLIST.contains(&filename)  {
            let source = fs::read(&path).unwrap();

            println!("\nParse {} ...", filename);
            let mut script = ScriptParser::<F32, F64>::from_source_and_name(&source, filename).unwrap();

            println!("Executing {} ...", filename);
            while let Some(Command { line, kind }) = script.next().unwrap() {
                use wabt::script::CommandKind::*;
                use std::panic::catch_unwind;
                use std::panic::AssertUnwindSafe;

                macro_rules! match_trace {
                    (match $e:expr; $($p:pat => $b:block)*) => {
                        match $e {
                            $($p => {
                                println!("Execute line {}, {} ...", line, stringify!($p));
                                if let Err(_) = catch_unwind(AssertUnwindSafe(|| $b)) {
                                    failures.push((filename.to_owned(), line));
                                } else {
                                    successes += 1;
                                }
                            })*
                        }
                    }
                }

                match_trace! {
                    match kind;

                    Module {
                        module,
                        name,
                    } => {
                        let bytes = module.into_vec();
                        let name: Option<String> = name;

                        sctrl.new_frame(move |stst: StSt| {
                            let (module, _custom_sections) = parse_binary_format(&bytes).unwrap();
                            let validated_module = validate_module(module).unwrap();

                            let mut stst = stst;
                            let moduleaddr = instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &[]).unwrap();

                            store_thread_frame(stst);
                        });
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

    if failures.len() > 0 {
        println!("wast failures:");
        for (i, f) in failures.iter().enumerate() {
            println!("    {}:{}", f.0, f.1);
            if i > 10 {
                println!("    ...");
                break;
            }
        }
        println!("wast total: {} passed; {} failed", successes, failures.len());
        panic!("some wast commands failed");
    }
}
