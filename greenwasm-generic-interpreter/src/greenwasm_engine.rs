use generic_interface::*;
use std::collections::HashMap;
use greenwasm_structure::modules::Module;

pub struct GreenwasmEngine {
    modules: HashMap<ModuleId, Module>,
    modules_count: u64,
}

impl GreenwasmEngine {
    pub fn new() -> Self {
        Self {
            modules: Default::default(),
            modules_count: 0,
        }
    }
}

impl Engine for GreenwasmEngine {
    fn load_module_from_slice(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
        let r = greenwasm_binary_format::parse_binary_format(data);
        match r {
            Ok((module, _custom_sections)) => {
                self.modules.insert(ModuleId(self.modules_count), module);
                self.modules_count += 1;
                Ok(ModuleId(self.modules_count - 1))
            }
            Err(_) => {
                Err(EngineError)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generic_test<T: Engine>(e: &mut T) {
        let _id_result = e.load_module_from_slice(&[]);
    }

    #[test]
    fn greenwasm_test() {
        let mut e = GreenwasmEngine::new();
        generic_test(&mut e);
    }
}
