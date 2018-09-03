#![feature(nll)]

extern crate greenwasm;
extern crate greenwasm_spectest;

use greenwasm::binary_format::parse_binary_format;
use greenwasm::validation::{validate_module};
use greenwasm::execution::modules::instantiation::instantiate_module;
use greenwasm::execution::modules::invocation::*;
use greenwasm::execution::runtime_structure::*;
use greenwasm::execution::runtime_structure::Result as IResult;
use greenwasm_spectest::*;

use std::collections::HashMap;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;

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
    _handle: thread::JoinHandle<()>,
    modules: HashMap<String, ModuleAddr>,
    last_module: Option<ModuleAddr>,
}

impl StoreCtrl {
    fn new() -> Self {
        let (tx, rx) = channel();

        let _handle = thread::spawn(|| {
            let _: FrameWitness = store_thread_frame(
                StSt { store: Store::new(), stack: Stack::new(), recv: rx});
        });

        StoreCtrl {
            tx,
            _handle,
            modules: HashMap::new(),
            last_module: None,
        }
    }

    fn new_frame<T: Send + 'static, F: Fn(StSt, &HashMap<String, ModuleAddr>, &Sender<T>) -> FrameWitness + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        let modules = self.modules.clone();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.take().unwrap();
            let _: FrameWitness = f(stst, &modules, &tx);
        })).unwrap();
        rx.recv().expect("new_frame() closure terminated before producing result")
    }

    fn frame<T: Send + 'static, F: Fn(&mut StSt, &HashMap<String, ModuleAddr>) -> T + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        let modules = self.modules.clone();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.as_mut().unwrap();
            let r = f(stst, &modules);
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

impl ScriptHandler for StoreCtrl {
    fn reset(&mut self) {
        *self = Self::new();
    }

    fn module(&mut self, bytes: Vec<u8>, name: Option<String>) {
        let moduleaddr = self.new_frame(move |stst: StSt, modules: &_, tx: &Sender<::std::result::Result<ModuleAddr, &'static str>>| {
            macro_rules! mytry {
                ($e:expr, $s:expr, $m:expr) => {
                    match $e {
                        Ok(v) => v,
                        Err(_) => {
                            tx.send(Err($m)).unwrap();
                            return store_thread_frame($s);
                        }
                    }
                }
            }

            let (module, _custom_sections) = mytry!(parse_binary_format(&bytes), stst, "parsing failed");
            let validated_module = mytry!(validate_module(module), stst, "validation failed");

            let mut stst = stst;
            let mut exports = vec![];
            for i in &validated_module.imports {
                // println!("i: {:?}", i);
                let exporting_module = *mytry!(modules.get(&i.module[..]).ok_or(()), stst, "import module not found");
                let exporting_module = &stst.store.modules[exporting_module];
                let mut value = None;
                for e in &exporting_module.exports {
                    // println!("  e: {:?}", e);
                    if e.name[..] == i.name[..] {
                        value = Some(e.value);
                        break;
                    }
                }
                exports.push(mytry!(value.ok_or(()), stst, "import not found in import modules exports"));
            }

            let moduleaddr = mytry!(instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &exports), stst, "instantiation failed");

            tx.send(Ok(moduleaddr)).unwrap();
            store_thread_frame(stst)
        });

        self.add_module(name, moduleaddr.unwrap());
    }
    fn assert_uninstantiable(&mut self, bytes: Vec<u8>) {
        self.new_frame(move |stst: StSt, modules: &_, tx: &Sender<::std::result::Result<&'static str, &'static str>>| {
            macro_rules! mytry {
                ($e:expr, $s:expr, $m:expr) => {
                    match $e {
                        Ok(v) => v,
                        Err(_) => {
                            tx.send(Err($m)).unwrap();
                            return store_thread_frame($s);
                        }
                    }
                }
            }
            macro_rules! antimytry {
                ($e:expr, $s:expr, $m:expr) => {
                    match $e {
                        Ok(v) => v,
                        Err(_) => {
                            tx.send(Ok($m)).unwrap();
                            return store_thread_frame($s);
                        }
                    }
                }
            }
            let (module, _custom_sections) = mytry!(parse_binary_format(&bytes), stst, "parsing failed");
            let validated_module = antimytry!(validate_module(module), stst, "validation failed");

            let mut stst = stst;
            let mut exports = vec![];
            for i in &validated_module.imports {
                // println!("i: {:?}", i);
                let exporting_module = *antimytry!(modules.get(&i.module[..]).ok_or(()), stst, "import module not found");
                let exporting_module = &stst.store.modules[exporting_module];
                let mut value = None;
                for e in &exporting_module.exports {
                    // println!("  e: {:?}", e);
                    if e.name[..] == i.name[..] {
                        value = Some(e.value);
                        break;
                    }
                }
                exports.push(antimytry!(value.ok_or(()), stst, "import not found in import modules exports"));
            }

            antimytry!(instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &exports), stst, "instantiation failed");

            tx.send(Err("instantiation did not fail")).unwrap();
            store_thread_frame(stst)
        }).unwrap();
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>) {
        assert!(parse_binary_format(&bytes).is_err(), "parsing did not fail");
    }
    fn assert_invalid(&mut self, bytes: Vec<u8>) {
        let (module, _) = parse_binary_format(&bytes).unwrap();
        assert!(validate_module(module).is_err(), "validation did not fail");
    }
    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> InvokationResult {
        let moduleaddr = self.get_module(module);
        self.frame(move |stst, _modules| {
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
                match invoke(&mut stst.store, &mut stst.stack, funcaddr, &args) {
                    Ok(IResult::Vals(v)) => {
                        Ok(InvokationResult::Vals(v.into_iter().map(val_greenwasm2wabt).collect()))
                    }
                    Ok(IResult::Trap) => {
                        Ok(InvokationResult::Trap)
                    }
                    Err(InvokeError::StackExhaustion) => {
                        Ok(InvokationResult::StackExhaustion)
                    }
                    Err(_) => {
                        Err("Invokation error")
                    }
                }
            })
        }).unwrap()
    }
    fn action_get(&mut self, module: Option<String>, field: String) -> Value {
        let moduleaddr = self.get_module(module);
        self.frame(move |stst, _modules| {
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
    fn assert_exhaustion(&mut self, action: Action) {
        match action {
            Action::Invoke { module, field, args } => {
                if let InvokationResult::StackExhaustion = self.action_invoke(module, field, args) {
                } else {
                    panic!("invokation should exhaust the stack, but did not");
                }
            }
            Action::Get { .. } => {
                panic!("a global access can not exhaust the stack!")
            }
        }
    }
    fn register(&mut self, name: Option<String>, as_name: String) {
        let moduleaddr = name.map(|n| self.modules[&n]).unwrap_or_else(|| self.last_module.unwrap());
        self.add_module(Some(as_name), moduleaddr);
    }
}

#[test]
fn run_tests() {
    run_mvp_spectest(&mut StoreCtrl::new()).present();
}
