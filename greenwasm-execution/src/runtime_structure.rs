use structure::types::*;
use structure::instructions::*;
use structure::modules::*;
use crate::structure_references::*;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

// TODO: util module
#[derive(Clone, Debug)]
pub struct TypedIndexVec<T, IndexT> {
    data: Vec<T>,
    _marker: PhantomData<IndexT>,
}

impl<T, IndexT> TypedIndexVec<T, IndexT>
    where IndexT: Into<usize> + From<usize>
{
    pub fn get(&self, addr: IndexT) -> Option<&T> {
        self.data.get(addr.into())
    }
    pub fn get_mut(&mut self, addr: IndexT) -> Option<&mut T> {
        self.data.get_mut(addr.into())
    }
    pub fn next_addr(&self) -> IndexT {
        self.data.len().into()
    }
    pub fn push(&mut self, inst: T) -> IndexT {
        let addr = self.next_addr();
        self.data.push(inst);
        addr
    }

    /// This should only be called for temporary data
    pub fn pop_aux(&mut self) {
        self.data.pop();
    }
}
impl<T, IndexT> From<Vec<T>> for TypedIndexVec<T, IndexT> {
    fn from(v: Vec<T>) -> Self {
        Self {
            data: v,
            _marker: PhantomData,
        }
    }
}
impl<T, IndexT> Index<IndexT> for TypedIndexVec<T, IndexT>
    where IndexT: Into<usize> + From<usize>
{
    type Output = T;
    fn index(&self, addr: IndexT) -> &T {
        &self.data[addr.into()]
    }
}
impl<T, IndexT> IndexMut<IndexT> for TypedIndexVec<T, IndexT>
    where IndexT: Into<usize> + From<usize>
{
    fn index_mut(&mut self, addr: IndexT) -> &mut T {
        &mut self.data[addr.into()]
    }
}

#[derive(Copy, Clone, PartialEq)]
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

pub struct Store<Refs = ModuleCloneRef>
    where Refs: StructureReference
{
    pub funcs: TypedIndexVec<FuncInst<Refs>, FuncAddr>,
    pub tables: TypedIndexVec<TableInst, TableAddr>,
    pub mems: TypedIndexVec<MemInst, MemAddr>,
    pub globals: TypedIndexVec<GlobalInst, GlobalAddr>,

    /// All instanced modules in the `Store`
    ///
    /// This is a modification of the spec to make it easier to resolve cycles
    /// between data structures.
    pub modules: TypedIndexVec<ModuleInst<Refs>, ModuleAddr>,
}

#[derive(Clone, Copy, PartialEq)]
pub struct FuncAddr(pub usize);
impl Into<usize> for FuncAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for FuncAddr { fn from(a: usize) -> Self { FuncAddr(a) } }

#[derive(Clone, Copy, PartialEq)]
pub struct TableAddr(pub usize);
impl Into<usize> for TableAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for TableAddr { fn from(a: usize) -> Self { TableAddr(a) } }

#[derive(Clone, Copy, PartialEq)]
pub struct MemAddr(pub usize);
impl Into<usize> for MemAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for MemAddr { fn from(a: usize) -> Self { MemAddr(a) } }

#[derive(Clone, Copy, PartialEq)]
pub struct GlobalAddr(pub usize);
impl Into<usize> for GlobalAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for GlobalAddr { fn from(a: usize) -> Self { GlobalAddr(a) } }

/// Address of a `ModuleInst` in the `Store`
///
/// This is a modification of the spec to make it easier to resolve cycles
/// between data structures.
#[derive(Clone, Copy, PartialEq)]
pub struct ModuleAddr(pub usize);
impl Into<usize> for ModuleAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for ModuleAddr { fn from(a: usize) -> Self { ModuleAddr(a) } }

#[derive(Clone)]
pub struct ModuleInst<Refs>
    where Refs: StructureReference
{
    pub types: Refs::FuncTypesRef,
    pub funcaddrs: TypedIndexVec<FuncAddr, FuncIdx>,
    pub tableaddrs: TypedIndexVec<TableAddr, TableIdx>,
    pub memaddrs: TypedIndexVec<MemAddr, MemIdx>,
    pub globaladdrs: TypedIndexVec<GlobalAddr, GlobalIdx>,
    pub exports: Vec<ExportInst<Refs>>,
}

#[derive(Clone)]
pub enum FuncInst<Refs>
    where Refs: StructureReference
{
    Internal {
        type_: Refs::FuncTypeRef,
        module: ModuleAddr,
        code: Refs::FuncRef,
    },
    Host {
        type_: Refs::FuncTypeRef,
        hostcode: HostFunc
    },
}
impl<Refs> FuncInst<Refs> where Refs: StructureReference {
    pub fn type_(&self) -> &Refs::FuncTypeRef {
        match self {
            | FuncInst::Internal { type_, .. }
            | FuncInst::Host { type_, .. }
            => {
                type_
            }
        }
    }
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
    pub name: Refs::NameRef,
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

#[derive(PartialEq)]
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

#[derive(PartialEq)]
pub struct Frame {
    pub locals: Vec<Val>,
    pub module: ModuleAddr,
}
