use greenwasm_execution::modules::invocation::InvokeError;
pub use greenwasm_execution::runtime_structure::ModuleAddr;
use greenwasm_execution::runtime_structure::{Result as InvokeResult, Val};
use greenwasm_utils::NamedLookup;

#[derive(Debug)]
pub enum EngineError {
    Parsing,
    Validation,
    Instantiation,
    WrongId,
    Resolution,
}

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct ModuleId(pub usize);

use std::collections::HashMap;

#[derive(Default)]
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

pub trait Engine: Default {
    fn from_binary_format(&mut self, data: &[u8]) -> EngineResult<ModuleId>;

    fn instance_module(&mut self, module: ModuleId, imports: Imports) -> EngineResult<ModuleAddr>;

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

    fn from_binary_format_eager_validation(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
        self.from_binary_format(data)
    }
}


/*

interface:
- 1 engine
- 1 memory module format
- 1 parser pipeline




let mut engine: Box<DynamicEngine> = Box::new(GreenwasmEngine::<>)


*/
