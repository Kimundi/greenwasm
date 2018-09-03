#![feature(nll)]

extern crate greenwasm;
extern crate greenwasm_spectest;

use greenwasm::structure::types::*;
use greenwasm::structure::modules::*;
use greenwasm::structure::instructions::*;
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

        let mut s = StoreCtrl {
            tx,
            _handle,
            modules: HashMap::new(),
            last_module: None,
        };

        let moduleaddr = s.new_frame(move |stst: StSt, _modules, tx: &Sender<::std::result::Result<ModuleAddr, &'static str>>| {
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

            let validated_module = mytry!(validate_module(spectest_module()), stst, "validation failed");

            let mut stst = stst;
            let moduleaddr = mytry!(instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &[]), stst, "instantiation failed");

            tx.send(Ok(moduleaddr)).unwrap();
            store_thread_frame(stst)
        });

        s.add_module(Some("spectest".into()), moduleaddr.unwrap());

        s
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

/*
from wabt's spectest-interp.cc

static void InitEnvironment(Environment* env) {
  HostModule* host_module = env->AppendHostModule("spectest");
  host_module->AppendFuncExport("print", {{}, {}}, PrintCallback);
  host_module->AppendFuncExport("print_i32", {{Type::I32}, {}}, PrintCallback);
  host_module->AppendFuncExport("print_f32", {{Type::F32}, {}}, PrintCallback);
  host_module->AppendFuncExport("print_f64", {{Type::F64}, {}}, PrintCallback);
  host_module->AppendFuncExport("print_i32_f32", {{Type::I32, Type::F32}, {}},
                                PrintCallback);
  host_module->AppendFuncExport("print_f64_f64", {{Type::F64, Type::F64}, {}},
                                PrintCallback);

  host_module->AppendTableExport("table", Limits(10, 20));
  host_module->AppendMemoryExport("memory", Limits(1, 2));

  host_module->AppendGlobalExport("global_i32", false, uint32_t(666));
  host_module->AppendGlobalExport("global_i64", false, uint64_t(666));
  host_module->AppendGlobalExport("global_f32", false, float(666.6f));
  host_module->AppendGlobalExport("global_f64", false, double(666.6));
}
*/
// TODO: Get this module in binary wasm form, and inject with same mechanism as testsuite modules
fn spectest_module() -> Module {
    Module {
        types: vec![
            FuncType {
                args: vec![].into(),
                results: vec![].into(),
            },
            FuncType {
                args: vec![ValType::I32].into(),
                results: vec![].into(),
            },
            FuncType {
                args: vec![ValType::F32].into(),
                results: vec![].into(),
            },
            FuncType {
                args: vec![ValType::F64].into(),
                results: vec![].into(),
            },
            FuncType {
                args: vec![ValType::I32, ValType::F32].into(),
                results: vec![].into(),
            },
            FuncType {
                args: vec![ValType::F64, ValType::F64].into(),
                results: vec![].into(),
            },
        ].into(),
        funcs: vec![
            Func {
                type_: TypeIdx(0),
                locals: vec![].into(),
                body: Expr {
                    body: vec![
                    ]
                },
            },
            Func {
                type_: TypeIdx(1),
                locals: vec![].into(),
                body: Expr {
                    body: vec![
                    ]
                },
            },
            Func {
                type_: TypeIdx(2),
                locals: vec![].into(),
                body: Expr {
                    body: vec![
                    ]
                },
            },
            Func {
                type_: TypeIdx(3),
                locals: vec![].into(),
                body: Expr {
                    body: vec![
                    ]
                },
            },
            Func {
                type_: TypeIdx(4),
                locals: vec![].into(),
                body: Expr {
                    body: vec![
                    ]
                },
            },
            Func {
                type_: TypeIdx(5),
                locals: vec![].into(),
                body: Expr {
                    body: vec![
                    ]
                },
            },
        ].into(),
        tables: vec![
            Table {
                type_: TableType {
                    limits: Limits {
                        min: 10,
                        max: Some(20),
                    },
                    elemtype: ElemType::AnyFunc,
                }
            },
        ].into(),
        mems: vec![
            Mem {
                type_: MemType {
                    limits: Limits {
                        min: 1,
                        max: Some(2),
                    }
                }
            }
        ].into(),
        globals: vec![
            Global {
                type_: GlobalType {
                    mutability: Mut::Const,
                    valtype: ValType::I32,
                },
                init: Expr {
                    body: vec![
                        Instr::I32Const(666)
                    ]
                }
            },
            Global {
                type_: GlobalType {
                    mutability: Mut::Const,
                    valtype: ValType::I64,
                },
                init: Expr {
                    body: vec![
                        Instr::I64Const(666)
                    ]
                }
            },
            Global {
                type_: GlobalType {
                    mutability: Mut::Const,
                    valtype: ValType::F32,
                },
                init: Expr {
                    body: vec![
                        Instr::F32Const(666.6)
                    ]
                }
            },
            Global {
                type_: GlobalType {
                    mutability: Mut::Const,
                    valtype: ValType::F64,
                },
                init: Expr {
                    body: vec![
                        Instr::F64Const(666.6)
                    ]
                }
            },
        ].into(),
        elem: vec![].into(),
        data: vec![].into(),
        start: None,
        imports: vec![].into(),
        exports: vec![
            Export {
                name: "print".into(),
                desc: ExportDesc::Func(FuncIdx(0)),
            },
            Export {
                name: "print_i32".into(),
                desc: ExportDesc::Func(FuncIdx(1)),
            },
            Export {
                name: "print_f32".into(),
                desc: ExportDesc::Func(FuncIdx(2)),
            },
            Export {
                name: "print_f64".into(),
                desc: ExportDesc::Func(FuncIdx(3)),
            },
            Export {
                name: "print_i32_f32".into(),
                desc: ExportDesc::Func(FuncIdx(4)),
            },
            Export {
                name: "print_f64_f64".into(),
                desc: ExportDesc::Func(FuncIdx(5)),
            },
            Export {
                name: "table".into(),
                desc: ExportDesc::Table(TableIdx(0)),
            },
            Export {
                name: "memory".into(),
                desc: ExportDesc::Mem(MemIdx(0)),
            },
            Export {
                name: "global_i32".into(),
                desc: ExportDesc::Global(GlobalIdx(0)),
            },
            Export {
                name: "global_i64".into(),
                desc: ExportDesc::Global(GlobalIdx(1)),
            },
            Export {
                name: "global_f32".into(),
                desc: ExportDesc::Global(GlobalIdx(2)),
            },
            Export {
                name: "global_f64".into(),
                desc: ExportDesc::Global(GlobalIdx(3)),
            },
        ].into(),
    }
}

#[test]
fn run_tests() {
    run_mvp_spectest(&mut StoreCtrl::new()).present();
}
