extern crate greenwasm_utils;
use self::greenwasm_utils::IdAppendContainer;

use greenwasm_structure::modules::Module;
use greenwasm_validation::{validate_module};
use ::modules::instantiation::instantiate_module;
use ::modules::invocation::*;
use ::runtime_structure::*;
use ::runtime_structure::Result as IResult;

use std::collections::HashMap;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;

pub struct StSt<'ast> {
    pub store: Store<'ast>,
    pub stack: Stack<'ast>,
    pub recv: Receiver<CmdFn>,
}
pub type CmdFn = Box<for<'ast> Fn(&mut Option<StSt<'ast>>) + Send>;

#[must_use]
pub struct FrameWitness;

pub fn store_thread_frame<'ast>(stst: StSt<'ast>) -> FrameWitness {
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

pub struct StoreCtrl {
    tx: Sender<CmdFn>,
    _handle: thread::JoinHandle<()>,
    pub modules: HashMap<String, ModuleAddr>,
    pub last_module: Option<ModuleAddr>,
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

    pub fn new_frame<T: Send + 'static, F: Fn(StSt, &HashMap<String, ModuleAddr>, &Sender<T>) -> FrameWitness + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        let modules = self.modules.clone();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.take().unwrap();
            let _: FrameWitness = f(stst, &modules, &tx);
        })).unwrap();
        rx.recv().expect("new_frame() closure terminated before producing result")
    }

    pub fn frame<T: Send + 'static, F: Fn(&mut StSt, &HashMap<String, ModuleAddr>) -> T + Send + 'static>(&self, f: F) -> T {
        let (tx, rx) = channel();
        let modules = self.modules.clone();
        self.tx.send(Box::new(move |stst: &mut Option<StSt>| {
            let stst = stst.as_mut().unwrap();
            let r = f(stst, &modules);
            tx.send(r).unwrap();
        })).unwrap();
        rx.recv().expect("action() closure terminated before producing result")
    }

    pub fn add_module(&mut self, name: Option<String>, module: ModuleAddr) {
        if let Some(name) = name {
            self.modules.insert(name, module);
        }
        self.last_module = Some(module);
    }

    pub fn get_module(&self, name: Option<String>) -> ModuleAddr {
        name.map(|name| self.modules[&name]).or(self.last_module).unwrap()
    }
}

pub type ModuleId = u64;
pub struct DynamicAdapter {
    pub ctrl: StoreCtrl,
    pub loaded_modules: IdAppendContainer<ModuleAddr>,
}

impl DynamicAdapter {
    pub fn new() -> Self {
        Self {
            ctrl: StoreCtrl::new(),
            loaded_modules: Default::default(),
        }
    }

    pub fn raise_error(&mut self, e: &'static str) {
        self.ctrl.new_frame(move |stst: StSt, modules: &_, tx: &Sender<::std::result::Result<ModuleAddr, &'static str>>| {
            tx.send(Err(e)).unwrap();
            store_thread_frame(stst)
        });
    }

    pub fn load_module(&mut self, module: Module, name: Option<String>) {
        use std::cell::RefCell;
        let module = RefCell::new(Some(module));

        let moduleaddr = self.ctrl.new_frame(move |stst: StSt, modules: &_, tx: &Sender<::std::result::Result<ModuleAddr, &'static str>>| {
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

        self.ctrl.add_module(name, moduleaddr.unwrap());
    }
}
