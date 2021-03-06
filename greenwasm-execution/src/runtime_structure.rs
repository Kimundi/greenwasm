use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use greenwasm_structure::types::*;
use greenwasm_structure::instructions::*;
use greenwasm_structure::modules::*;
use DEBUG_EXECUTION;

// TODO: util module
#[derive(Clone, PartialEq)]
pub struct TypedIndexVec<T, IndexT> {
    data: Vec<T>,
    _marker: PhantomData<IndexT>,
}
impl<T, IndexT> Default for TypedIndexVec<T, IndexT> {
    fn default() -> Self {
        TypedIndexVec { data: vec![], _marker: PhantomData }
    }
}
use std::fmt::Debug;
impl<T: Debug, IndexT> Debug for TypedIndexVec<T, IndexT> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        self.data.fmt(f)
    }
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

#[derive(Copy, Clone, PartialEq, Debug)]
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

#[derive(Default)]
pub struct Store<'ast>

{
    pub funcs: TypedIndexVec<FuncInst<'ast>, FuncAddr>,
    pub tables: TypedIndexVec<TableInst, TableAddr>,
    pub mems: TypedIndexVec<MemInst, MemAddr>,
    pub globals: TypedIndexVec<GlobalInst, GlobalAddr>,

    /// All instanced modules in the `Store`
    ///
    /// This is a modification of the spec to make it easier to resolve cycles
    /// between data structures.
    pub modules: TypedIndexVec<ModuleInst<'ast>, ModuleAddr>,
}
impl<'ast> Store<'ast> {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct FuncAddr(pub usize);
impl Into<usize> for FuncAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for FuncAddr { fn from(a: usize) -> Self { FuncAddr(a) } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct TableAddr(pub usize);
impl Into<usize> for TableAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for TableAddr { fn from(a: usize) -> Self { TableAddr(a) } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct MemAddr(pub usize);
impl Into<usize> for MemAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for MemAddr { fn from(a: usize) -> Self { MemAddr(a) } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct GlobalAddr(pub usize);
impl Into<usize> for GlobalAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for GlobalAddr { fn from(a: usize) -> Self { GlobalAddr(a) } }

/// Address of a `ModuleInst` in the `Store`
///
/// This is a modification of the spec to make it easier to resolve cycles
/// between data structures.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ModuleAddr(pub usize);
impl Into<usize> for ModuleAddr { fn into(self) -> usize { self.0 } }
impl From<usize> for ModuleAddr { fn from(a: usize) -> Self { ModuleAddr(a) } }

#[derive(Clone)]
pub struct ModuleInst<'ast>

{
    pub types: &'ast [FuncType],
    pub funcaddrs: TypedIndexVec<FuncAddr, FuncIdx>,
    pub tableaddrs: TypedIndexVec<TableAddr, TableIdx>,
    pub memaddrs: TypedIndexVec<MemAddr, MemIdx>,
    pub globaladdrs: TypedIndexVec<GlobalAddr, GlobalIdx>,
    pub exports: Vec<ExportInst<'ast>>,
}

#[derive(Clone)]
pub enum FuncInst<'ast>

{
    Internal {
        type_: &'ast FuncType,
        module: ModuleAddr,
        code: &'ast Func,
    },
    Host {
        type_: &'ast FuncType,
        hostcode: HostFunc
    },
}
impl<'ast> FuncInst<'ast>  {
    pub fn type_(&self) -> &'ast FuncType {
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

#[derive(Clone, Debug)]
pub struct ExportInst<'ast>

{
    pub name: &'ast Name,
    pub value: ExternVal,
}

#[derive(Copy, Clone, Debug)]
pub enum ExternVal {
    Func(FuncAddr),
    Table(TableAddr),
    Mem(MemAddr),
    Global(GlobalAddr),
}

#[derive(Default)]
pub struct Stack<'instr> {
    data: Vec<StackElem<'instr>>,
    frame_indices: Vec<usize>,
    label_indices: Vec<usize>,
}

#[derive(Debug)]
pub struct StackExhaustion;
pub type StackResult = ::std::result::Result<(), StackExhaustion>;

impl<'instr> Stack<'instr> {
    const DEPTH_LIMIT: usize = 1000;

    fn depth_check(&self) -> StackResult {
        if self.data.len() >= Self::DEPTH_LIMIT {
            Err(StackExhaustion)
        } else {
            Ok(())
        }
    }

    pub fn new() -> Self { Self::default() }

    fn debug_printme(&self, msg: &str) {
        if DEBUG_EXECUTION {
            self.printme(msg);
        }
    }

    pub fn printme(&self, msg: &str) {
        println!("{}: Stack[", msg);
        for e in self.data.iter().rev() {
            match e {
                StackElem::Val(v) => println!("  {:?}", v),
                StackElem::Activation(Activation { n, frame, next_instr }) => {
                    println!("  Frame {} {:?} locals: {:?} next: {:?}", n, frame.module, frame.locals, next_instr)
                }
                StackElem::Label(Label { n, branch_target, next_instr }) => {
                    println!("  Label {} target: {:?} next: {:?}", n, branch_target, next_instr)
                }
            }
        }
        println!("]\n");
    }

    pub fn push_val(&mut self, val: Val) -> StackResult {
        self.depth_check()?;
        self.data.push(StackElem::Val(val));
        self.debug_printme("push_val");
        Ok(())
    }
    pub fn push_label(&mut self, n: usize, branch_target: &'instr [Instr], next_instr: &'instr [Instr]) -> StackResult {
        self.depth_check()?;
        self.label_indices.push(self.data.len());
        self.data.push(StackElem::Label(Label {
            n,
            branch_target,
            next_instr,
        }));
        self.debug_printme("push_label");
        Ok(())
    }
    pub fn push_frame(&mut self, n: usize, frame: Frame, next_instr: &'instr [Instr]) -> StackResult {
        self.depth_check()?;
        self.frame_indices.push(self.data.len());
        self.data.push(StackElem::Activation(Activation {
            n,
            frame,
            next_instr,
        }));
        self.debug_printme("push_frame");
        Ok(())
    }

    pub fn top(&self) -> Option<&StackElem> {
        self.data.last()
    }

    pub fn pop_frame(&mut self) -> Activation<'instr> {
        self.frame_indices.pop();
        let r = if let Some(StackElem::Activation(a)) = self.data.pop() {
            a
        } else {
            panic!("No Frame at top of stack")
        };
        self.debug_printme("pop_frame");
        r
    }

    pub fn pop_val(&mut self) -> Val {
        let r = if let Some(StackElem::Val(val)) = self.data.pop() {
            val
        } else {
            panic!("No Val at top of stack")
        };
        self.debug_printme("pop_val");
        r
    }

    pub fn pop_label(&mut self) -> Label<'instr> {
        self.label_indices.pop();
        let r = if let Some(StackElem::Label(l)) = self.data.pop() {
            l
        } else {
            panic!("No Label at top of stack")
        };
        self.debug_printme("pop_label");
        r
    }

    pub fn lth_label(&self, l: LabelIdx) -> &Label<'instr> {
        let len = self.label_indices.len();
        let pos = self.label_indices[len - 1 - (l.0 as usize)];
        if let StackElem::Label(l) = &self.data[pos] {
            l
        } else {
            panic!("No Label l at position pos of stack")
            // TODO: more useful debug messages
        }
    }

    pub fn peek_val(&mut self) -> Val {
        if let Some(&StackElem::Val(val)) = self.data.last() {
            val
        } else {
            panic!("No Val at top of stack")
        }
    }

    pub fn current_activation(&mut self) -> &mut Activation<'instr> {
        let cfi = *self.frame_indices.last().expect("No Frame at top of stack");
        if let StackElem::Activation (ref mut a) = self.data[cfi] {
            a
        } else {
            panic!("No Frame at top of stack")
        }
    }

    pub fn current_label(&self) -> &Label<'instr> {
        let cli = *self.label_indices.last().expect("No Label at top of stack");
        if let StackElem::Label(ref l) = self.data[cli] {
            l
        } else {
            panic!("No Label at top of stack")
        }
    }

    pub fn current_frame(&mut self) -> &mut Frame {
        &mut self.current_activation().frame
    }

    pub fn current_frame_arity(&mut self) -> usize {
        self.current_activation().n
    }

    pub fn label_count(&self) -> usize {
        self.label_indices.len()
    }

    pub fn top_ctrl_entry(&self) -> TopCtrlEntry {
        let fi = self.frame_indices.last();
        let li = self.label_indices.last();
        match (fi, li) {
            (Some(fi), Some(li)) if fi > li => TopCtrlEntry::Activation,
            (Some(fi), Some(li)) if fi < li => TopCtrlEntry::Label,
            (Some(_), None) => TopCtrlEntry::Activation,
            (None, Some(_)) => TopCtrlEntry::Label,
            (None, None) => TopCtrlEntry::None,
            (Some(_), Some(_)) => unreachable!(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn depth(&self) -> usize {
        self.data.len()
    }

    pub fn unwind_to(&mut self, depth: usize) {
        self.data.truncate(depth);
        while let Some(idx) = self.label_indices.last().cloned() {
            if idx >= depth {
                self.label_indices.pop();
            } else {
                break;
            }
        }
        while let Some(idx) = self.frame_indices.last().cloned() {
            if idx >= depth {
                self.frame_indices.pop();
            } else {
                break;
            }
        }
    }
}

pub enum TopCtrlEntry {
    Label,
    Activation,
    None,
}

#[derive(PartialEq)]
pub struct Label<'instr> {
    pub n: usize, // NB. Can only be 0 or 1
    pub branch_target: &'instr [Instr],

    // NB: Added to make instr execution less error prone
    pub next_instr: &'instr [Instr],
}

#[derive(PartialEq)]
pub struct Activation<'instr> {
    pub n: usize, // NB. Can only be 0 or 1
    pub frame: Frame,

    // NB: Added to make instr execution less error prone
    pub next_instr: &'instr [Instr],
}

#[derive(PartialEq)]
pub enum StackElem<'instr> {
    Val(Val),
    Label(Label<'instr>),
    Activation(Activation<'instr>),
}

impl<'instr> StackElem<'instr> {
    pub fn is_val(&self) -> bool {
        if let StackElem::Val(_) = self { true } else { false }
    }
}

#[derive(PartialEq)]
pub struct Frame {
    pub locals: TypedIndexVec<Val, LocalIdx>,
    pub module: ModuleAddr,
}
