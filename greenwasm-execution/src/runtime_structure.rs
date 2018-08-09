use structure::types::*;
use structure::modules::*;
use structure::instructions::*;

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

pub struct Store {
    pub funcs: Vec<FuncInst>,
    pub tables: Vec<TableInst>,
    pub mems: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,
}

#[derive(Clone, Copy)]
pub struct FuncAddr(pub usize);

#[derive(Clone, Copy)]
pub struct TableAddr(pub usize);

#[derive(Clone, Copy)]
pub struct MemAddr(pub usize);

#[derive(Clone, Copy)]
pub struct GlobalAddr(pub usize);

#[derive(Clone)]
pub struct ModuleInst {
    pub types: Vec<FuncType>,
    pub funcaddrs: Vec<FuncAddr>,
    pub tableaddrs: Vec<TableAddr>,
    pub memaddrs: Vec<MemAddr>,
    pub globaladdrs: Vec<GlobalAddr>,
    pub exports: Vec<ExportInst>,
}

#[derive(Clone)]
pub enum FuncInst {
    Internal {
        type_: FuncType,
        module: ModuleInst, // TODO: change implementation to something more sane
        code: Func
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
pub struct ExportInst {
    pub name: Name,
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
    pub module: ModuleInst, // TODO: change implementation to something more sane
}
