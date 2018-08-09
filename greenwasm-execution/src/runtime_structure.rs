use structure::types::*;
use structure::modules::*;
use structure::instructions::*;
use crate::structure_references::*;

#[derive(Copy, Clone)]
pub enum Val {
    I32(I32),
    I64(I64),
    F32(F32),
    F64(F64),
}
impl Val {
    pub fn ty(&self) -> ValType {
        match *self {
            Val::I32(_) => ValType::I32,
            Val::I64(_) => ValType::I64,
            Val::F32(_) => ValType::F32,
            Val::F64(_) => ValType::F64,
        }
    }
}

pub enum Result {
    Vals(Vec<Val>),
    Trap,
}

pub struct Store<Refs = Module>
    where Refs: StructureReference
{
    pub funcs: Vec<FuncInst<Refs>>,
    pub tables: Vec<TableInst>,
    pub mems: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,

    /// All instanced modules in the `Store`
    ///
    /// This is a modification of the spec to make it easier to resolve cycles
    /// between data structures.
    pub modules: Vec<ModuleInst<Refs>>,
}

#[derive(Clone, Copy)]
pub struct FuncAddr(pub usize);

#[derive(Clone, Copy)]
pub struct TableAddr(pub usize);

#[derive(Clone, Copy)]
pub struct MemAddr(pub usize);

#[derive(Clone, Copy)]
pub struct GlobalAddr(pub usize);

/// Address of a `ModuleInst` in the `Store`
///
/// This is a modification of the spec to make it easier to resolve cycles
/// between data structures.
#[derive(Clone, Copy)]
pub struct ModuleAddr(pub usize);

#[derive(Clone)]
pub struct ModuleInst<Refs>
    where Refs: StructureReference
{
    pub types: Vec<FuncType>, // TODO: replace with reference to Module
    pub funcaddrs: Vec<FuncAddr>,
    pub tableaddrs: Vec<TableAddr>,
    pub memaddrs: Vec<MemAddr>,
    pub globaladdrs: Vec<GlobalAddr>,
    pub exports: Vec<ExportInst<Refs>>,
}

#[derive(Clone)]
pub enum FuncInst<Refs>
    where Refs: StructureReference
{
    Internal {
        type_: FuncType,
        module: ModuleAddr,
        code: Refs::FuncRef,
    },
    Host {
        type_: FuncType,
        hostcode: HostFunc
    },
}

#[derive(Clone, Copy)]
pub struct HostFunc {
    pub id: u32
}

pub struct TableInst {
    pub elem: Wec<FuncElem>,
    pub max: Option<u32>
}

#[derive(Clone, Copy)]
pub struct FuncElem(pub Option<FuncAddr>);

pub struct MemInst {
    pub data: Wec<Byte>,
    pub max: Option<u32>,
}

pub struct GlobalInst {
    pub value: Val,
    pub mutability: Mut,
}

#[derive(Clone)]
pub struct ExportInst<Refs>
    where Refs: StructureReference
{
    pub name: Refs::NameRef, // TODO: change implementation to something more sane
    pub value: ExternVal,
}

#[derive(Clone)]
pub enum ExternVal {
    Func(FuncAddr),
    Table(TableAddr),
    Mem(MemAddr),
    Global(GlobalAddr),
}

pub type Stack = Vec<StackElem>;

pub enum StackElem {
    Val(Val),
    Label {
        n: usize, // NB. Can only be 0 or 1
        branch_target: Vec<Instr>, // TODO: change implementation to something more sane
    },
    Activation {
        n: usize, // NB. Can only be 0 or 1
        frame: Frame,
    },
}

pub struct Frame {
    pub locals: Vec<Val>,
    pub module: ModuleAddr,
}
