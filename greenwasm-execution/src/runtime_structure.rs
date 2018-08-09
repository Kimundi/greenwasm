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

pub struct FuncAddr(pub usize);

pub struct TableAddr(pub usize);

pub struct MemAddr(pub usize);

pub struct GlobalAddr(pub usize);

pub struct ModuleInst {
    pub types: Vec<FuncType>,
    pub funcaddrs: Vec<FuncAddr>,
    pub tableaddrs: Vec<TableAddr>,
    pub memaddrs: Vec<MemAddr>,
    pub globaladdrs: Vec<GlobalAddr>,
    pub exports: Vec<ExportInst>,
}

pub enum FuncInst {
    Internal {
        type_: FuncType,
        module: ModuleInst,
        code: Func
    },
    Host {
        type_: FuncType,
        hostcode: HostFunc
    },
}

pub struct HostFunc {
    pub id: usize
}

pub struct TableInst {
    pub elem: Wec<FuncElem>,
    pub max: Option<u32>
}

pub struct FuncElem(pub Option<FuncAddr>);

pub struct MemInst {
    pub data: Wec<Byte>,
    pub max: Option<u32>,
}

pub struct GlobalInst {
    pub value: Val,
    pub mutability: Mut,
}

pub struct ExportInst {
    pub name: Name,
    pub value: ExternVal,
}

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
