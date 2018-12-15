extern crate greenwasm_execution;
extern crate greenwasm_structure;

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

pub mod generic_interface;
pub mod greenwasm_engine;

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
    pub types: Vec<FuncType>,
    pub funcaddrs: TypedIndexVec<FuncAddr, FuncIdx>,
    pub tableaddrs: TypedIndexVec<TableAddr, TableIdx>,
    pub memaddrs: TypedIndexVec<MemAddr, MemIdx>,
    pub globaladdrs: TypedIndexVec<GlobalAddr, GlobalIdx>,
    pub exports: Vec<ExportInst>,
}

#[derive(Clone)]
pub struct Func {
    pub type_: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: (),
}

#[derive(Clone)]
pub enum FuncInst<HostFunc> {
    Internal {
        r#type: FuncType,
        module: ModuleAddr,
        code: Func,
    },
    Host {
        r#type: FuncType,
        hostcode: HostFunc
    },
}

#[derive(Clone, Debug)]
pub struct ExportInst {
    pub name: Name,
    pub value: ExternVal,
}
