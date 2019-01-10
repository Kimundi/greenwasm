use greenwasm_execution::modules::invocation::InvokeError;
pub use greenwasm_execution::runtime_structure::ModuleAddr;
use greenwasm_execution::runtime_structure::{Result as InvokeResult, Val};
use greenwasm_utils::NamedLookup;

#[derive(Debug)]
pub enum EngineError {
    Parsing,
    Validation,
    Instantiation,
    UnknownId,
    Resolution,
    StackExhaustion,
    UnknownSymbol,
    SignatureMismatch,
}

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct ModuleId(pub usize);

use std::collections::HashMap;

#[derive(Default, Clone)]
pub struct Imports {
    modules: HashMap<String, ModuleAddr>,
}
impl From<HashMap<String, ModuleAddr>> for Imports {
    fn from(v: HashMap<String, ModuleAddr>) -> Self {
        Self { modules: v }
    }
}
impl Into<HashMap<String, ModuleAddr>> for Imports {
    fn into(self) -> HashMap<String, ModuleAddr> {
        self.modules
    }
}

impl NamedLookup<ModuleAddr> for Imports {
    fn lookup(&self, name: &str) -> Option<ModuleAddr> {
        self.modules.get(name).map(|x| ((*x).0).into())
    }
}

pub type EngineResult<T> = Result<T, EngineError>;

pub trait Engine {
    fn from_binary_format(&mut self, data: &[u8]) -> EngineResult<ModuleId>;

    fn instance_module(&mut self, module: ModuleId, imports: &Imports) -> EngineResult<ModuleAddr>;

    fn invoke_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        args: &[Val],
    ) -> EngineResult<InvokeResult>;

    fn get_global_export(&mut self, moduleaddr: ModuleAddr, symbol: &str) -> EngineResult<Val>;
    fn set_global_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        value: Val,
    ) -> EngineResult<()>;
}

impl<E: Engine + ?Sized> Engine for Box<E> {
    fn from_binary_format(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
        E::from_binary_format(self, data)
    }

    fn instance_module(&mut self, module: ModuleId, imports: &Imports) -> EngineResult<ModuleAddr> {
        E::instance_module(self, module, imports)
    }

    fn invoke_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        args: &[Val],
    ) -> EngineResult<InvokeResult> {
        E::invoke_export(self, moduleaddr, symbol, args)
    }

    fn get_global_export(&mut self, moduleaddr: ModuleAddr, symbol: &str) -> EngineResult<Val> {
        E::get_global_export(self, moduleaddr, symbol)
    }
    fn set_global_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        value: Val,
    ) -> EngineResult<()>{
        E::set_global_export(self, moduleaddr, symbol, value)
    }
}

impl<'a, E: Engine + ?Sized> Engine for &'a mut E {
    fn from_binary_format(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
        E::from_binary_format(self, data)
    }

    fn instance_module(&mut self, module: ModuleId, imports: &Imports) -> EngineResult<ModuleAddr> {
        E::instance_module(self, module, imports)
    }

    fn invoke_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        args: &[Val],
    ) -> EngineResult<InvokeResult> {
        E::invoke_export(self, moduleaddr, symbol, args)
    }

    fn get_global_export(&mut self, moduleaddr: ModuleAddr, symbol: &str) -> EngineResult<Val> {
        E::get_global_export(self, moduleaddr, symbol)
    }
    fn set_global_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        value: Val,
    ) -> EngineResult<()>{
        E::set_global_export(self, moduleaddr, symbol, value)
    }
}


/*

interface:
- 1 engine
- 1 memory module format
- 1 parser pipeline




let mut engine: Box<DynamicEngine> = Box::new(GreenwasmEngine::<>)


*/

#[test]
fn test_trait_object() {
    #[derive(Default)]
    struct Foo;
    impl Engine for Foo {
        fn from_binary_format(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
            unimplemented!()
        }

        fn instance_module(&mut self, module: ModuleId, imports: &Imports) -> EngineResult<ModuleAddr> {
            unimplemented!()
        }

        fn invoke_export(
            &mut self,
            moduleaddr: ModuleAddr,
            symbol: &str,
            args: &[Val],
        ) -> EngineResult<InvokeResult> {
            unimplemented!()
        }

        fn get_global_export(&mut self, moduleaddr: ModuleAddr, symbol: &str) -> EngineResult<Val> {
            unimplemented!()
        }

        fn set_global_export(
            &mut self,
            moduleaddr: ModuleAddr,
            symbol: &str,
            value: Val,
        ) -> EngineResult<()> {
            unimplemented!()
        }
    }

    fn bar<E: Engine>(_: E) {}

    bar(Foo);
    bar(&mut Foo);
    bar(Box::new(Foo));
    bar(&mut Foo as &mut Engine);
    bar(Box::new(Foo) as Box<Engine>);
}
