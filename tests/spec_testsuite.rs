#![feature(nll)]

extern crate greenwasm;
extern crate greenwasm_spectest;
extern crate greenwasm_utils;
extern crate greenwasm_generic_interpreter;

use greenwasm::binary_format::parse_binary_format;
use greenwasm::validation::{validate_module};
use greenwasm::execution::runtime_structure::*;
use greenwasm::execution::dynamic_adapter::{DynamicAdapter, InvokeError};
use greenwasm_spectest::*;
use greenwasm_utils::NamedLookup;
use greenwasm_generic_interpreter::generic_interface::{Engine, EngineResult, EngineError};

use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
struct MapNameLookup(Arc<HashMap<String, ModuleAddr>>);
impl NamedLookup<ModuleAddr> for MapNameLookup {
    fn lookup(&self, name: &str) -> Option<ModuleAddr> {
        self.0.get(name).cloned()
    }
}

struct DynamicAdapterScriptHandler {
    da: DynamicAdapter,
    modules: MapNameLookup,
    last_module: Option<ModuleAddr>,
}

impl DynamicAdapterScriptHandler {
    fn new() -> Self {
        Self {
            da: DynamicAdapter::new(),
            modules: MapNameLookup(Arc::new(HashMap::new())),
            last_module: None,
        }
    }

    fn add_module(&mut self, name: Option<String>, module: ModuleAddr) {
        if let Some(name) = name {
            let mut nm = (*self.modules.0).clone();
            nm.insert(name, module);
            self.modules = MapNameLookup(Arc::new(nm));
        }
        self.last_module = Some(module);
    }

    fn get_module(&self, name: Option<String>) -> ModuleAddr {
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
fn vals_wabt2greenwasm(v: Vec<Value>) -> Vec<Val> {
    v.into_iter().map(val_wabt2greenwasm).collect()
}
fn vals_greenwasm2wabt(v: Vec<Val>) -> Vec<Value> {
    v.into_iter().map(val_greenwasm2wabt).collect()
}

impl ScriptHandler for DynamicAdapterScriptHandler {
    fn reset(&mut self) {
        *self = Self::new();
    }

    fn register(&mut self, name: Option<String>, as_name: String) {
        let moduleaddr = name.map(|n| self.modules.0[&n]).unwrap_or_else(|| self.last_module.unwrap());
        self.add_module(Some(as_name), moduleaddr);
    }
    fn module(&mut self, bytes: Vec<u8>, name: Option<String>) {
        let (module, _) = parse_binary_format(&bytes).expect("parsing failed");
        let module = validate_module(module).expect("validation failed");

        let moduleaddr = self.da.load_module(module, self.modules.clone()).unwrap();
        self.add_module(name, moduleaddr);
    }
    fn assert_uninstantiable(&mut self, bytes: Vec<u8>) {
        let (module, _) = parse_binary_format(&bytes).expect("parsing failed");
        let module = validate_module(module).expect("validation failed");

        let r = self.da.load_module(module, self.modules.clone());
        assert!(r.is_err(), "instaniation did not fail");
        use greenwasm::execution::dynamic_adapter::LoadModuleError::*;
        match r.err().unwrap() {
            Validation |
            ImportModule |
            ImportSymbol |
            Instantiation => (), // TODO: check current state of testsuite in regard to the cases that should be covered by this assert
        }
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>) {
        assert!(parse_binary_format(&bytes).is_err(), "parsing did not fail");
    }
    fn assert_invalid(&mut self, bytes: Vec<u8>) {
        let (module, _) = parse_binary_format(&bytes).unwrap();
        assert!(validate_module(module).is_err(), "validation did not fail");
    }
    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> WabtResult {
        let moduleaddr = self.get_module(module);
        let r = self.da.invoke(moduleaddr, field, vals_wabt2greenwasm(args));
        match r {
            Ok(Result::Vals(v)) => {
                WabtResult::Vals(vals_greenwasm2wabt(v))
            }
            Ok(Result::Trap) => {
                WabtResult::Trap
            }
            Err(InvokeError::StackExhaustion) => {
                WabtResult::StackExhaustion
            }
            Err(other) => panic!("{:?}", other),
        }
    }
    fn action_get(&mut self, module: Option<String>, field: String) -> Value {
        let moduleaddr = self.get_module(module);
        let r = self.da.get_global(moduleaddr, field).unwrap();
        val_greenwasm2wabt(r)
    }
    fn assert_exhaustion(&mut self, action: Action) {
        match action {
            Action::Invoke { module, field, args } => {
                if let WabtResult::StackExhaustion = self.action_invoke(module, field, args) {
                } else {
                    panic!("invokation should exhaust the stack, but did not");
                }
            }
            Action::Get { .. } => {
                panic!("a global access can not exhaust the stack!")
            }
        }
    }
}

struct EngineScriptHandler<E> {
    engine: E,
    modules: MapNameLookup,
    last_module: Option<ModuleAddr>,
}

impl<E: Engine + Default> EngineScriptHandler<E> {
    fn new() -> Self {
        Self {
            engine: E::default(),
            modules: MapNameLookup(Arc::new(HashMap::new())),
            last_module: None,
        }
    }

    fn add_module(&mut self, name: Option<String>, module: ModuleAddr) {
        if let Some(name) = name {
            let mut nm = (*self.modules.0).clone();
            nm.insert(name, module);
            self.modules = MapNameLookup(Arc::new(nm));
        }
        self.last_module = Some(module);
    }

    fn get_module(&self, name: Option<String>) -> ModuleAddr {
        name.map(|name| self.modules.0[&name]).or(self.last_module).unwrap()
    }

    fn try_module(&mut self, bytes: Vec<u8>) -> EngineResult<ModuleAddr> {
        let moduleid = self.engine.from_binary_format(&bytes)?;

        let imports = (*self.modules.0).clone();
        let moduleaddr = self.engine.instance_module(moduleid, &imports.into())?;
        Ok(moduleaddr)
    }
}

impl<E: Engine + Default> ScriptHandler for EngineScriptHandler<E> {
    fn reset(&mut self) {
        *self = Self::new();
    }

    fn register(&mut self, name: Option<String>, as_name: String) {
        let moduleaddr = name.map(|n| self.modules.0[&n]).unwrap_or_else(|| self.last_module.unwrap());
        self.add_module(Some(as_name), moduleaddr);
    }
    fn module(&mut self, bytes: Vec<u8>, name: Option<String>) {
        let moduleaddr = self.try_module(bytes).unwrap();

        self.add_module(name, moduleaddr);
    }
    fn assert_uninstantiable(&mut self, bytes: Vec<u8>) {
        match self.try_module(bytes) {
            Err(EngineError::Resolution) => (),
            Err(EngineError::Instantiation) => (),
            otherwise => panic!("{:?}", otherwise),
        }
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>) {
        match self.try_module(bytes) {
            Err(EngineError::Parsing) => (),
            otherwise => panic!("{:?}", otherwise),
        }
    }
    fn assert_invalid(&mut self, bytes: Vec<u8>) {
        match self.try_module(bytes) {
            Err(EngineError::Validation) => (),
            otherwise => panic!("{:?}", otherwise),
        }
    }
    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> WabtResult {
        let moduleaddr = self.get_module(module);
        let r = self.engine.invoke_export(moduleaddr, &field, &vals_wabt2greenwasm(args));
        match r {
            Ok(Result::Vals(v)) => {
                WabtResult::Vals(vals_greenwasm2wabt(v))
            }
            Ok(Result::Trap) => {
                WabtResult::Trap
            }
            Err(EngineError::StackExhaustion) => {
                WabtResult::StackExhaustion
            }
            Err(other) => panic!("{:?}", other),
        }
    }
    fn action_get(&mut self, module: Option<String>, field: String) -> Value {
        let moduleaddr = self.get_module(module);
        let r = self.engine.get_global_export(moduleaddr, &field).unwrap();
        val_greenwasm2wabt(r)
    }
    fn assert_exhaustion(&mut self, action: Action) {
        match action {
            Action::Invoke { module, field, args } => {
                if let WabtResult::StackExhaustion = self.action_invoke(module, field, args) {
                } else {
                    panic!("invokation should exhaust the stack, but did not");
                }
            }
            Action::Get { .. } => {
                panic!("a global access can not exhaust the stack!")
            }
        }
    }
}

#[test]
fn run_tests_poc_greenwasm() {
    run_mvp_spectest(&mut DynamicAdapterScriptHandler::new()).present();
}

#[test]
fn run_tests_engine_poc_greenwasm() {
    use greenwasm_generic_interpreter::greenwasm_poc_engine::PocEngine;
    run_mvp_spectest(&mut EngineScriptHandler::<PocEngine>::new()).present();
}
