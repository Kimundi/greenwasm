extern crate greenwasm_execution;
extern crate greenwasm_structure;
extern crate greenwasm_validation;
extern crate greenwasm_utils;
extern crate greenwasm_binary_format;

use greenwasm_execution::runtime_structure::{
    TypedIndexVec,
    FuncAddr,
    TableAddr,
    MemAddr,
    GlobalAddr,
    ModuleAddr,
    TableInst,
    MemInst,
    GlobalInst,
    ExternVal,
};
use greenwasm_structure::types::{
    FuncType,
    ValType,
    Name,
};
use greenwasm_structure::modules::{
    FuncIdx,
    GlobalIdx,
    MemIdx,
    TableIdx,
    TypeIdx,
};
use greenwasm_validation::ValidatedModule;

use std::sync::Arc;

pub mod generic_interface;
pub mod greenwasm_engine;
pub mod greenwasm_test_engine;

#[derive(Default)]
pub struct Store<HostFunc> {
    pub funcs: TypedIndexVec<FuncInst<HostFunc>, FuncAddr>,
    pub tables: TypedIndexVec<TableInst, TableAddr>,
    pub mems: TypedIndexVec<MemInst, MemAddr>,
    pub globals: TypedIndexVec<GlobalInst, GlobalAddr>,
    pub modules: TypedIndexVec<ModuleInst, ModuleAddr>,
}
impl<HostFunc> Store<HostFunc> {
    pub fn new() -> Self where HostFunc: Default {
        Self::default()
    }
}

#[derive(Clone)]
pub struct ModuleInst {
    pub module: Arc<ValidatedModule>,
    // pub types: Vec<FuncType>, TODO: provided by assoicated module?
    pub funcaddrs: TypedIndexVec<FuncAddr, FuncIdx>,
    pub tableaddrs: TypedIndexVec<TableAddr, TableIdx>,
    pub memaddrs: TypedIndexVec<MemAddr, MemIdx>,
    pub globaladdrs: TypedIndexVec<GlobalAddr, GlobalIdx>,
    pub exports: Vec<ExportInst>,
}

#[derive(Clone)]
pub enum FuncInst<HostFunc> {
    Internal {
        module: ModuleAddr,
        type_: TypeIdx,
        code: FuncIdx,
    },
    Host {
        type_: FuncType,
        hostcode: HostFunc
    },
}

#[derive(Clone, Debug)]
pub struct ExportInst {
    // pub name: Name, // TODO: could derive from module reference
    pub value: ExternVal,
}
