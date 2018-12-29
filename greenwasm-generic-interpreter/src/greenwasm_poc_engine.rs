use generic_interface::*;
use greenwasm_binary_format::parse_binary_format;
use greenwasm_execution::dynamic_adapter::*;
use greenwasm_structure::modules::Module;
use greenwasm_validation::{ValidatedModule, validate_module};
use greenwasm_utils::IdAppendContainer;
use greenwasm_execution::runtime_structure::Val;
use greenwasm_execution::runtime_structure::Result as InvokeResult;

#[derive(Default)]
pub struct PocEngine {
    da: DynamicAdapter,
    modules: IdAppendContainer<ValidatedModule>,
}

impl PocEngine {
    pub fn new() -> Self {
        Self {
            da: DynamicAdapter::new(),
            modules: IdAppendContainer::new(),
        }
    }
}

impl Engine for PocEngine {
    fn from_binary_format(&mut self, data: &[u8]) -> EngineResult<ModuleId> {
        let module = parse_binary_format(data);
        let (module, _custom_sections) = module.map_err(|_| EngineError::Parsing)?;
        let module = validate_module(module).map_err(|_| EngineError::Validation)?;
        let id = self.modules.append(module);
        Ok(ModuleId(id))
    }

    fn instance_module(&mut self, module: ModuleId, imports: &Imports) -> EngineResult<ModuleAddr> {
        let module = self.modules.get(module.0).ok_or(EngineError::UnknownId)?;
        let (module, imports) = (module.clone(), imports.clone());
        let moduleaddr = self
            .da
            .load_module(module, imports)
            .map_err(|x| match x {
                LoadModuleError::Validation => EngineError::Validation,
                LoadModuleError::ImportModule => EngineError::Resolution,
                LoadModuleError::ImportSymbol => EngineError::Resolution,
                LoadModuleError::Instantiation => EngineError::Instantiation,
            })?;
        Ok(ModuleAddr(moduleaddr.into()))
    }

    fn invoke_export(
        &mut self,
        moduleaddr: ModuleAddr,
        symbol: &str,
        args: &[Val],
    ) -> EngineResult<InvokeResult> {
        let r = self.da.invoke(moduleaddr, symbol.to_owned(), args.to_owned());
        r.map_err(|e| match e {
            InvokeError::StackExhaustion => EngineError::StackExhaustion,
            InvokeError::MismatchedArguments => EngineError::SignatureMismatch,
            InvokeError::UnknownSymbol => EngineError::UnknownSymbol,
        })
    }

    fn get_global_export(&mut self, moduleaddr: ModuleAddr, symbol: &str) -> EngineResult<Val> {
        let r = self.da.get_global(moduleaddr, symbol.to_owned());
        r.map_err(|e| match e {
            GetGlobalError::UnknownSymbol => EngineError::UnknownSymbol,
        })
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
