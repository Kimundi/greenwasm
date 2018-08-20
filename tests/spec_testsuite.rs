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

        let moduleaddr = s.new_frame(move |stst: StSt, modules: &_, tx: &Sender<::std::result::Result<ModuleAddr, &'static str>>| {
            macro_rules! try {
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

            let validated_module = try!(validate_module(spectest_module()), stst, "validation failed");

            let mut stst = stst;
            let moduleaddr = try!(instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &[]), stst, "instantiation failed");

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

    fn frame<T: Send + 'static, F: Fn(&mut StSt) -> T + Send + 'static>(&self, f: F) -> T {
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
trait NanPayload {
    fn payload(&self) -> u64;
    fn signif() -> u32;
    fn infinite() -> Self;
    fn arithmetic_nan(payload: u64) -> Self;
    fn canonical_nan() -> Self;
    fn is_arithmetic_nan(&self) -> bool;
    fn is_canonical_nan(&self) -> bool;
}
impl NanPayload for f32 {
    fn payload(&self) -> u64 {
        let bits: u32 = self.to_bits();
        let mask: u32 = (1u32 << 23) - 1;
        let p = bits & mask;
        p as u64
    }
    fn signif() -> u32 { 23 }
    fn infinite() -> Self { 1.0 / 0.0 }
    fn arithmetic_nan(payload: u64) -> Self {
        let bits: u32 = Self::infinite().to_bits();
        let mask: u32 = (1u32 << Self::signif()) - 1;
        let bits = bits | (mask & (payload as u32));
        Self::from_bits(bits)
    }
    fn canonical_nan() -> Self {
        Self::arithmetic_nan(1u64 << (Self::signif() - 1))
    }
    fn is_arithmetic_nan(&self) -> bool {
        if !self.is_nan() {
            return false;
        }
        !self.is_canonical_nan()
    }
    fn is_canonical_nan(&self) -> bool {
        if !self.is_nan() {
            return false;
        }
        self.abs().to_bits() == Self::canonical_nan().to_bits()
    }
}
impl NanPayload for f64 {
    fn payload(&self) -> u64 {
        let bits: u64 = self.to_bits();
        let mask: u64 = (1u64 << 52) - 1;
        let p = bits & mask;
        p
    }
    fn signif() -> u32 { 52 }
    fn infinite() -> Self { 1.0 / 0.0 }
    fn arithmetic_nan(payload: u64) -> Self {
        let bits: u64 = Self::infinite().to_bits();
        let mask: u64 = (1u64 << Self::signif()) - 1;
        let bits = bits | (mask & payload);
        Self::from_bits(bits)
    }
    fn canonical_nan() -> Self {
        Self::arithmetic_nan(1u64 << (Self::signif() - 1))
    }
    fn is_arithmetic_nan(&self) -> bool {
        if !self.is_nan() {
            return false;
        }
        !self.is_canonical_nan()
    }
    fn is_canonical_nan(&self) -> bool {
        if !self.is_nan() {
            return false;
        }
        self.abs().to_bits() == Self::canonical_nan().to_bits()
    }
}

struct NanCompare<'a>(&'a [Value]);
impl<'a> ::std::cmp::PartialEq for NanCompare<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        self.0.iter().zip(other.0.iter()).all(|pair| {
            match pair {
                (Value::I32(l), Value::I32(r)) => l == r,
                (Value::I64(l), Value::I64(r)) => l == r,
                (Value::F32(l), Value::F32(r)) if l.is_nan() && r.is_nan() => {
                    l.payload() == r.payload()
                },
                (Value::F64(l), Value::F64(r)) if l.is_nan() && r.is_nan() => {
                    l.payload() == r.payload()
                },
                (Value::F32(l), Value::F32(r)) => l == r,
                (Value::F64(l), Value::F64(r)) => l == r,
                _ => false,
            }
        })
    }
}
impl<'a> ::std::fmt::Debug for NanCompare<'a> {
    fn fmt(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        formatter.debug_list().entries(self.0.iter().map(|e| {
            match e {
                Value::F32(v) if v.is_nan() => {
                    let p = v.payload();
                    format!("F32(NaN:0x{:x})", p)
                }
                Value::F64(v) if v.is_nan() => {
                    let p = v.payload();
                    format!("F64(NaN:0x{:x})", p)
                }
                _ => format!("{:?}", e)
            }
        })).finish()
    }
}

enum InvokationResult {
    Vals(Vec<Value>),
    Trap,
    StackExhaustion,
}

impl CommandDispatch for StoreCtrl {
    fn module(&mut self, bytes: Vec<u8>, name: Option<String>) {
        let moduleaddr = self.new_frame(move |stst: StSt, modules: &_, tx: &Sender<::std::result::Result<ModuleAddr, &'static str>>| {
            macro_rules! try {
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

            let (module, _custom_sections) = try!(parse_binary_format(&bytes), stst, "parsing failed");
            let validated_module = try!(validate_module(module), stst, "validation failed");

            let mut stst = stst;
            let mut exports = vec![];
            for i in &validated_module.imports {
                // println!("i: {:?}", i);
                let exporting_module = *try!(modules.get(&i.module[..]).ok_or(()), stst, "import module not found");
                let exporting_module = &stst.store.modules[exporting_module];
                let mut value = None;
                for e in &exporting_module.exports {
                    // println!("  e: {:?}", e);
                    if e.name[..] == i.name[..] {
                        value = Some(e.value);
                        break;
                    }
                }
                exports.push(try!(value.ok_or(()), stst, "import not found in import modules exports"));
            }

            let moduleaddr = try!(instantiate_module(&mut stst.store, &mut stst.stack, &validated_module, &exports), stst, "instantiation failed");

            tx.send(Ok(moduleaddr)).unwrap();
            store_thread_frame(stst)
        });

        self.add_module(name, moduleaddr.unwrap());
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
}

trait CommandDispatch {
    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> InvokationResult;
    fn action_get(&mut self, module: Option<String>, field: String) -> Value;
    fn action(&mut self, action: Action) -> Vec<Value> {
        match action {
            Action::Invoke { module, field, args } => {
                if let InvokationResult::Vals(v) = self.action_invoke(module, field, args) {
                    v
                } else {
                    panic!("invokation returned Trap or exhausted the stack");
                }
            }
            Action::Get { module, field } => {
                vec![self.action_get(module, field)]
            }
        }
    }

    fn module(&mut self, bytes: Vec<u8>, name: Option<String>);
    fn assert_return(&mut self, action: Action, expected: Vec<Value>) {
        let results = self.action(action);
        assert_eq!(NanCompare(&expected), NanCompare(&results));
    }
    fn assert_trap(&mut self, action: Action) {
        match action {
            Action::Invoke { module, field, args } => {
                if let InvokationResult::Vals(results) = self.action_invoke(module, field, args) {
                    panic!("invokation did not trap, but return {:?}", results);
                }
            }
            Action::Get { .. } => {
                panic!("a global access can not trap!")
            }
        }
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>);
    fn assert_invalid(&mut self, bytes: Vec<u8>);
    fn assert_exhaustion(&mut self, action: Action);
    fn assert_return_canonical_nan(&mut self, action: Action) {
        let results = self.action(action);
        match *results {
            [Value::F32(v)] if v.is_canonical_nan() => {}
            [Value::F64(v)] if v.is_canonical_nan() => {}
            ref x => {
                panic!("unexpected value {:?}", NanCompare(x));
            }
        }
    }
    fn assert_return_arithmetic_nan(&mut self, action: Action) {
        let results = self.action(action);
        match *results {
            [Value::F32(v)] if v.is_arithmetic_nan() => {}
            [Value::F64(v)] if v.is_arithmetic_nan() => {}
            ref x => {
                panic!("unexpected value {:?}", NanCompare(x));
            }
        }
    }
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
        AssertReturnCanonicalNan { action } => {
            c.assert_return_canonical_nan(action);
        }
        AssertReturnArithmeticNan { action } => {
            c.assert_return_arithmetic_nan(action);
        }
        AssertTrap { action, message } => {
            c.assert_trap(action);
        }
        AssertInvalid { module, message } => {
            c.assert_invalid(module.into_vec());
        }
        AssertMalformed { module, message } => {
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
            c.assert_exhaustion(action);
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

            //println!("\nParse {} ...", filename);
            let mut script = ScriptParser::<F32, F64>::from_source_and_name(&source, filename).unwrap();

            //println!("Executing {} ...", filename);
            while let Some(Command { line, kind }) = script.next().unwrap() {
                use std::panic::catch_unwind;
                use std::panic::AssertUnwindSafe;

                // println!("Line {} ...", line);

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
                    //panic!();
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
