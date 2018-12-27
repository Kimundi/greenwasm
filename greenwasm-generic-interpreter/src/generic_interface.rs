use greenwasm_utils::NamedLookup;
use greenwasm_execution::runtime_structure::ModuleAddr;

#[derive(Debug)]
pub struct EngineError;

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct ModuleId(pub u64);

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct InstancedModuleId(pub u64);

use std::collections::HashMap;

#[derive(Default)]
pub struct Imports {
    modules: HashMap<String, InstancedModuleId>
}
impl From<HashMap<String, InstancedModuleId>> for Imports {
    fn from(v: HashMap<String, InstancedModuleId>) -> Self {
        Self { modules: v }
    }
}
impl Into<HashMap<String, InstancedModuleId>> for Imports {
    fn into(self) -> HashMap<String, InstancedModuleId> {
        self.modules
    }
}

impl NamedLookup<ModuleAddr> for Imports {
    fn lookup(&self, name: &str) -> Option<ModuleAddr> {
        self.modules.get(name).map(|x| ((*x).0 as usize).into())
    }
}

pub type EngineResult<T> = Result<T, EngineError>;

pub trait Engine {
    fn from_binary_format(&mut self, data: &[u8])
        -> EngineResult<ModuleId>;

    fn instance_module(&mut self, module: ModuleId, imports: Imports)
        -> EngineResult<InstancedModuleId>;
}

/*

interface:
- 1 engine
- 1 memory module format
- 1 parser pipeline




let mut engine: Box<DynamicEngine> = Box::new(GreenwasmEngine::<>)


*/
