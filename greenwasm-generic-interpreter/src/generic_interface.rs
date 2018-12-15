#[derive(Debug)]
pub struct EngineError;

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct ModuleId(pub u64);

#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct InstancedModuleId(pub u64);

pub type EngineResult<T> = Result<T, EngineError>;

pub trait Engine {
    fn load_module_from_slice(&mut self, data: &[u8])
        -> EngineResult<ModuleId>;

    fn instance_module(&mut self, module: ModuleId)
        -> EngineResult<InstancedModuleId>;
}

/*

interface:
- 1 engine
- 1 memory module format
- 1 parser pipeline




let mut engine: Box<DynamicEngine> = Box::new(GreenwasmEngine::<>)


*/
