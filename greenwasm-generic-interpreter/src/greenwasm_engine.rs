use generic_interface::*;
use std::collections::HashMap;
use std::sync::Arc;
use super::*;
use greenwasm_utils::IdAppendContainer;

// use greenwasm_structure::modules::Module;
use greenwasm_validation::ValidatedModule;

pub struct GreenwasmEngine {
    modules: IdAppendContainer<Arc<ValidatedModule>>,
    instanced_modules: IdAppendContainer<ModuleInst>,
}

impl GreenwasmEngine {
    pub fn new() -> Self {
        Self {
            modules: IdAppendContainer::new(),
            instanced_modules: IdAppendContainer::new(),
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

        let module = self.modules.get(module.0).ok_or(EngineError)?;
        let imports: HashMap<String, InstancedModuleId> = imports.into();
        for (modulename, instanced_module_id) in imports {

        }


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
        let _ = generic_test(&mut e); //.unwrap();
    }
}
