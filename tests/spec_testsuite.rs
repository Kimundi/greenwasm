#![feature(nll)]

extern crate greenwasm;
extern crate greenwasm_spectest;
extern crate greenwasm_utils;

use greenwasm::binary_format::parse_binary_format;
use greenwasm::validation::{validate_module};
use greenwasm::execution::modules::instantiation::instantiate_module;
use greenwasm::execution::modules::invocation::*;
use greenwasm::execution::runtime_structure::*;
use greenwasm::execution::runtime_structure::Result as IResult;
use greenwasm::execution::dynamic_adapter::DynamicAdapter;
use greenwasm_spectest::*;
use greenwasm_utils::NamedLookup;

use std::collections::HashMap;
use std::sync::mpsc::Sender;
use std::sync::Arc;

// TODO: remove again
use greenwasm::execution::dynamic_adapter::*;

#[derive(Clone)]
struct MapNameLookup(Arc<HashMap<String, ModuleAddr>>);
impl NamedLookup for MapNameLookup {
    type Element = ModuleAddr;
    fn lookup(&self, name: &str) -> Option<&ModuleAddr> {
        self.0.get(name)
    }
}

struct DynamicAdapterScriptHandler {
    int: DynamicAdapter,
    modules: MapNameLookup,
    last_module: Option<ModuleAddr>,
}

impl DynamicAdapterScriptHandler {
    fn new() -> Self {
        Self {
            int: DynamicAdapter::new(),
            modules: MapNameLookup(Arc::new(HashMap::new())),
            last_module: None,
        }
    }

    fn new_frame<T: Send + 'static, F: Fn(StSt, &Sender<T>) -> FrameWitness + Send + 'static>(&self, f: F) -> T {
        self.int.ctrl.new_frame(f)
    }

    fn frame<T: Send + 'static, F: Fn(&mut StSt) -> T + Send + 'static>(&self, f: F) -> T {
        self.int.ctrl.frame(f)
    }

    pub fn add_module(&mut self, name: Option<String>, module: ModuleAddr) {
        if let Some(name) = name {
            let mut nm = (*self.modules.0).clone();
            nm.insert(name, module);
            self.modules = MapNameLookup(Arc::new(nm));
        }
        self.last_module = Some(module);
    }

    pub fn get_module(&self, name: Option<String>) -> ModuleAddr {
        name.map(|name| self.modules.0[&name]).or(self.last_module).unwrap()
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

impl ScriptHandler for DynamicAdapterScriptHandler {
    fn reset(&mut self) {
        *self = Self::new();
    }

    fn module(&mut self, bytes: Vec<u8>, name: Option<String>) {
        match parse_binary_format(&bytes) {
            Ok((module, _custom_sections)) => {
                let moduleaddr = self.int.load_module(module, self.modules.clone());
                self.add_module(name, moduleaddr);
            }
            Err(_) => {
                self.int.raise_error("parsing failed");
            }
        }
    }
    fn assert_uninstantiable(&mut self, bytes: Vec<u8>) {
        let modules = self.modules.clone();
        self.new_frame(move |stst: StSt, tx: &Sender<::std::result::Result<&'static str, &'static str>>| {
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
                let exporting_module = *antimytry!(modules.lookup(&i.module[..]).ok_or(()), stst, "import module not found");
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
        self.frame(move |stst| {
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
        self.frame(move |stst| {
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
        let moduleaddr = name.map(|n| self.modules.0[&n]).unwrap_or_else(|| self.last_module.unwrap());
        self.add_module(Some(as_name), moduleaddr);
    }
}

#[test]
fn run_tests() {
    run_mvp_spectest(&mut DynamicAdapterScriptHandler::new()).present();
}
