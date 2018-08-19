#![feature(fnbox)]

use structure::types::*;
use structure::modules::*;
use structure::instructions::Instr::*;
use structure::instructions::*;
use binary_format::parse_binary_format;
use validation::{validate_module, ValidatedModule};
use execution::modules::instantiation::instantiate_module;
use execution::modules::allocation::*;
use execution::modules::invocation::*;
use execution::runtime_structure::*;
use execution::runtime_structure::Result as IResult;

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

#[must_use]
struct FrameWitness;

fn store_thread_frame<'ast>(stst: StSt<'ast>) -> FrameWitness {
    let mut stst = Some(stst);
    while stst.is_some() {
        match stst.as_ref().unwrap().recv.recv() {
            Ok(cmd) => {
                cmd(&mut stst);
            }
            Err(_) => return FrameWitness,
        }
    }
    FrameWitness
}

struct StoreCtrl {
    tx: Sender<CmdFn>,
    handle: thread::JoinHandle<()>,
    modules: HashMap<String, ModuleAddr>,
    last_module: Option<ModuleAddr>,
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
            modules: HashMap::new(),
            last_module: None,
        }
    }

    fn new_frame<T: Send + 'static, F: Fn(StSt, &Sender<T>) -> FrameWitness + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.take().unwrap();
            let _: FrameWitness = f(stst, &tx);
        })).unwrap();
        rx.recv().expect("new_frame() closure terminated before producing result")
    }

    fn action<T: Send + 'static, F: Fn(&mut StSt) -> T + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.as_mut().unwrap();
            let r = f(stst);
            tx.send(r).unwrap();
        })).unwrap();
        rx.recv().expect("action() closure terminated before producing result")
    }

    fn add_module(&mut self, name: Option<String>, module: ModuleAddr) {
        if let Some(name) = name {
            self.modules.insert(name, module);
        }
        self.last_module = Some(module);
    }

    fn get_module(&self, name: Option<String>) -> ModuleAddr {
        name.map(|name| self.modules[&name]).or(self.last_module).unwrap()
    }
}

fn val_wabt2greenwasm(v: Value) -> Val {
    match v {
        Value::I32(v) => Val::I32(v as _),
        Value::I64(v) => Val::I64(v as _),
        Value::F32(v) => Val::F32(v),
        Value::F64(v) => Val::F64(v),
    }
}
fn val_greenwasm2wabt(v: Val) -> Value {
    match v {
        Val::I32(v) => Value::I32(v as _),
        Val::I64(v) => Value::I64(v as _),
        Val::F32(v) => Value::F32(v),
        Val::F64(v) => Value::F64(v),
    }
}

impl CommandDispatch for StoreCtrl {
    fn module(&mut self, bytes: Vec<u8>, name: Option<String>) {
        let moduleaddr = self.new_frame(move |stst: StSt, tx| {
            let (module, _custom_sections) = parse_binary_format(&bytes).unwrap();
            let validated_module = validate_module(module).unwrap();

            let mut stst = stst;
            let moduleaddr = instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &[]).unwrap();

            tx.send(moduleaddr).unwrap();

            store_thread_frame(stst)
        });

        self.add_module(name, moduleaddr);
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>) {
        let test = || {
            let (module, _) = parse_binary_format(&bytes).map_err(|_| "parsing failed")?;
            validate_module(module).map_err(|_| "validation failed")
        };
        assert!(test().is_err());
    }

    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> Option<Vec<Value>> {
        let moduleaddr = self.get_module(module);
        self.action(move |stst| {
            let funcaddr = (|| {
                let module = &stst.store.modules[moduleaddr];
                for e in &module.exports {
                    if **e.name == *field {
                        if let ExternVal::Func(funcaddr) = e.value {
                            return Ok(funcaddr);
                        }
                    }
                }
                Err("No matching export found")
            })();

            funcaddr.and_then(|funcaddr| {
                let args = args.iter().cloned().map(val_wabt2greenwasm).collect::<Vec<_>>();
                invoke(&mut stst.store, &mut stst.stack, funcaddr, &args).map(|r| {
                    match r {
                        IResult::Vals(v) => {
                            Some(v.into_iter().map(val_greenwasm2wabt).collect())
                        }
                        IResult::Trap => None
                    }
                }).map_err(|_| "Invokation error")
            })
        }).unwrap()
    }
    fn action_get(&mut self, module: Option<String>, field: String) -> Value {
        let moduleaddr = self.get_module(module);
        self.action(move |stst| {
            let globaladdr = (|| {
                let module = &stst.store.modules[moduleaddr];
                for e in &module.exports {
                    if **e.name == *field {
                        if let ExternVal::Global(globaladdr) = e.value {
                            return Some(globaladdr);
                        }
                    }
                }
                None
            })();
            globaladdr.map(|globaladdr| val_greenwasm2wabt(stst.store.globals[globaladdr].value))
        }).expect("No matching export found")
    }
}

trait CommandDispatch {
    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> Option<Vec<Value>>;
    fn action_get(&mut self, module: Option<String>, field: String) -> Value;

    fn module(&mut self, bytes: Vec<u8>, name: Option<String>);
    fn assert_return(&mut self, action: Action, expected: Vec<Value>) {
        let results = match action {
            Action::Invoke { module, field, args } => {
                self.action_invoke(module, field, args).expect("invokation returned Trap")
            }
            Action::Get { module, field } => {
                vec![self.action_get(module, field)]
            }
        };
        assert_eq!(expected, results);
    }
    fn assert_trap(&mut self, action: Action) {
        match action {
            Action::Invoke { module, field, args } => {
                if let Some(results) = self.action_invoke(module, field, args) {
                    panic!("invokation did not trap, but return {:?}", results);
                }
            }
            Action::Get { .. } => {
                panic!("a global access can not trap!")
            }
        }
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>);
}
fn command_dispatch<C: CommandDispatch>(cmd: CommandKind, c: &mut C) {
    use wabt::script::CommandKind::*;
    match cmd {
        Module { module, name } => {
            c.module(module.into_vec(), name);
        }
        AssertReturn { action, expected } => {
            c.assert_return(action, expected);
        }
        AssertReturnCanonicalNan {
            action,
        } => {
            unimplemented!("AssertReturnCanonicalNan");
        }
        AssertReturnArithmeticNan {
            action,
        } => {
            unimplemented!("AssertReturnArithmeticNan");
        }
        AssertTrap { action, message } => {
            c.assert_trap(action);
        }
        AssertInvalid {
            module,
            message,
        } => {
            let bytes = module.into_vec();
            unimplemented!("AssertInvalid");
        }
        AssertMalformed {
            module,
            message,
        } => {
            c.assert_malformed(module.into_vec());
        }
        AssertUninstantiable {
            module,
            message,
        } => {
            let bytes = module.into_vec();
            unimplemented!("AssertUninstantiable");
        }
        AssertExhaustion {
            action,
        } => {
            unimplemented!("AssertExhaustion");
        }
        AssertUnlinkable {
            module,
            message,
        } => {
            let bytes = module.into_vec();
            unimplemented!("AssertUnlinkable");
        }
        Register {
            name,
            as_name,
        } => {
            unimplemented!("Register");
        }
        PerformAction(action) => {
            unimplemented!("PerformAction");
        }
    }
}

#[test]
fn run_tests() {
    let mut successes = 0;
    let mut failures = vec![];

    'outer: for dir in fs::read_dir("tests/spec_testsuite").unwrap() {
        let dir = dir.unwrap();
        let path = dir.path();
        let filename = path.file_name().unwrap().to_str().unwrap();
        let mut fatal = false;

        let mut sctrl = StoreCtrl::new();

        if path.metadata().unwrap().file_type().is_file() && filename.ends_with(".wast") && !BLACKLIST.contains(&filename)  {
            let source = fs::read(&path).unwrap();

            println!("\nParse {} ...", filename);
            let mut script = ScriptParser::<F32, F64>::from_source_and_name(&source, filename).unwrap();

            println!("Executing {} ...", filename);
            while let Some(Command { line, kind }) = script.next().unwrap() {
                use std::panic::catch_unwind;
                use std::panic::AssertUnwindSafe;

                if fatal {
                    failures.push((filename.to_owned(), line, "<not attempted>".to_string()));
                    continue;
                }

                if let Err(msg) = catch_unwind(AssertUnwindSafe(|| {
                    command_dispatch(kind, &mut sctrl);
                })) {
                    let msg = if let Some(msg) = msg.downcast_ref::<String>() {
                        msg.to_string()
                    } else if let Some(msg) = msg.downcast_ref::<&'static str>() {
                        msg.to_string()
                    } else {
                        "<unknown>".to_string()
                    };
                    failures.push((filename.to_owned(), line, msg));
                    fatal = true;
                } else {
                    successes += 1;
                }
            }
        }
    }

    if failures.len() > 0 {
        println!("wast failures:");
        for (i, f) in failures.iter().enumerate() {
            println!("    {}:{}, {}", f.0, f.1, f.2);
            if i > 10 {
                println!("    ...");
                break;
            }
        }
        println!("wast total: {} passed; {} failed", successes, failures.len());
        panic!("some wast commands failed");
    }
}
