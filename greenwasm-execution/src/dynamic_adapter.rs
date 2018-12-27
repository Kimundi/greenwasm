extern crate greenwasm_utils;
use self::greenwasm_utils::{NamedLookup};

use greenwasm_structure::modules::Module;
use greenwasm_validation::{validate_module};
use ::modules::instantiation::instantiate_module;
use ::modules::invocation::*;
use ::runtime_structure::*;
use std::result::Result as StdResult;
use ::modules::invocation::InvokeError as ModInvokeError;

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

pub struct DynamicAdapter {
    tx: Sender<CmdFn>,
    _handle: thread::JoinHandle<()>,
}

impl DynamicAdapter {
    pub fn new() -> Self {
        let (tx, rx) = channel();

        let _handle = thread::spawn(|| {
            let _: FrameWitness = store_thread_frame(
                StSt { store: Store::new(), stack: Stack::new(), recv: rx});
        });

        DynamicAdapter {
            tx,
            _handle,
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

    fn frame<T: Send + 'static, F: Fn(&mut StSt) -> T + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.as_mut().unwrap();
            let r = f(stst);
            tx.send(r).unwrap();
        })).unwrap();
        rx.recv().expect("frame() closure terminated before producing result")
    }
}

#[derive(Debug)]
pub enum LoadModuleError {
    Validation,
    ImportModule,
    ImportSymbol,
    Instantiation,
}

#[derive(Debug)]
pub enum InvokeError {
    MismatchedArguments,
    StackExhaustion,
    UnknownSymbol,
}

impl DynamicAdapter {
    pub fn load_module<L>(&mut self, module: Module, lookup: L)
        -> StdResult<ModuleAddr, LoadModuleError>
        where L: NamedLookup<ModuleAddr> + Send + 'static
    {
        use std::cell::RefCell;
        let module = RefCell::new(Some(module));

        let moduleaddr = self.new_frame(move |stst: StSt, tx: &Sender<::std::result::Result<ModuleAddr, LoadModuleError>>| {
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

            let module = module.borrow_mut().take().unwrap();
            let validated_module = mytry!(validate_module(module), stst, LoadModuleError::Validation);

            let mut stst = stst;
            let mut exports = vec![];
            for i in &validated_module.imports {
                // println!("i: {:?}", i);
                let exporting_module = mytry!(lookup.lookup(&i.module[..]).ok_or(()), stst, LoadModuleError::ImportModule);
                let exporting_module = &stst.store.modules[exporting_module];
                let mut value = None;
                for e in &exporting_module.exports {
                    // println!("  e: {:?}", e);
                    if e.name[..] == i.name[..] {
                        value = Some(e.value);
                        break;
                    }
                }
                exports.push(mytry!(value.ok_or(()), stst, LoadModuleError::ImportSymbol));
            }

            let moduleaddr = mytry!(
                instantiate_module(
                    &mut stst.store,
                    &mut stst.stack,
                    &validated_module, &exports),
                stst,
                LoadModuleError::Instantiation);

            tx.send(Ok(moduleaddr)).unwrap();
            store_thread_frame(stst)
        });

        moduleaddr
    }
    pub fn invoke(&mut self, moduleaddr: ModuleAddr, field: String, args: Vec<Val>)
        -> StdResult<Result, InvokeError>
    {
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
                Err(InvokeError::UnknownSymbol)
            })();

            funcaddr.and_then(|funcaddr| {
                invoke(&mut stst.store, &mut stst.stack, funcaddr, &args)
                    .map_err(|s| match s {
                        ModInvokeError::StackExhaustion
                            => InvokeError::StackExhaustion,
                        ModInvokeError::MismatchedArgumentCount |
                        ModInvokeError::MismatchedArgumentType
                            => InvokeError::MismatchedArguments,
                    })
            })
        })
    }
    pub fn get_global(&mut self, moduleaddr: ModuleAddr, field: String) -> Val {
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
            globaladdr.map(|globaladdr| stst.store.globals[globaladdr].value)
        }).expect("No matching export found")
    }
}
