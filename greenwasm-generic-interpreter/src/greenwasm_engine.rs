use generic_interface::*;
use std::collections::HashMap;
use std::sync::Arc;

// use greenwasm_structure::modules::Module;
use greenwasm_validation::ValidatedModule;

struct IdAppendContainer<T> {
    counter: u64,
    data: HashMap<u64, T>,
}

impl<T> IdAppendContainer<T> {
    fn new() -> Self {
        Self {
            counter: 0,
            data: Default::default(),
        }
    }
    fn append(&mut self, t: T) -> u64 {
        let c = self.counter;
        self.counter += 1;
        self.data.insert(c, t);
        return c;
    }
}



pub struct GreenwasmEngine {
    modules: IdAppendContainer<Arc<ValidatedModule>>,
}

impl GreenwasmEngine {
    pub fn new() -> Self {
        Self {
            modules: IdAppendContainer::new(),
        }
    }
}

impl Engine for GreenwasmEngine {
    fn load_module_from_slice(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
        let module = greenwasm_binary_format::parse_binary_format(data);
        let (module, _custom_sections) = module.map_err(|_| EngineError)?;

        let module = greenwasm_validation::validate_module(module);
        let module = module.map_err(|_| EngineError)?;

        let id = self.modules.append(Arc::new(module));
        Ok(ModuleId(id))
    }
    fn instance_module(&mut self, module: ModuleId, imports: Imports)
        -> EngineResult<InstancedModuleId> {
        unimplemented!()

        /*
        - need list of imports to be provided
        - don't automatically transitilvy import modules
        */
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generic_test<T: Engine>(e: &mut T) -> EngineResult<()> {
        let module_id = e.load_module_from_slice(&[])?;

        let instanced_module_id = e.instance_module(
            module_id, Imports::default())?;

        let _tmp = instanced_module_id;

        Ok(())
    }

    #[test]
    fn greenwasm_test() {
        let mut e = GreenwasmEngine::new();
        let _tmp = generic_test(&mut e);
    }
}
