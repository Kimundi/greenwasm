use super::types::Wec;
use super::types::FuncType;
use super::types::ValType;
use super::types::TableType;
use super::types::MemType;
use super::types::GlobalType;
use super::types::Byte;
use super::types::Name;
use super::types::U32;

use super::instructions::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub types:   Wec<FuncType>,
    pub funcs:   Wec<Func>,
    pub tables:  Wec<Table>,
    pub mems:    Wec<Mem>,
    pub globals: Wec<Global>,
    pub elem:    Wec<Elem>,
    pub data:    Wec<Data>,
    pub start:   Option<Start>,
    pub imports: Wec<Import>,
    pub exports: Wec<Export>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct TypeIdx(pub U32);
impl Into<usize> for TypeIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for TypeIdx { fn from(a: usize) -> Self { TypeIdx(a as u32) } }

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct FuncIdx(pub U32);
impl Into<usize> for FuncIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for FuncIdx { fn from(a: usize) -> Self { FuncIdx(a as u32) } }

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct TableIdx(pub U32);
impl Into<usize> for TableIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for TableIdx { fn from(a: usize) -> Self { TableIdx(a as u32) } }

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct MemIdx(pub U32);
impl Into<usize> for MemIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for MemIdx { fn from(a: usize) -> Self { MemIdx(a as u32) } }

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct GlobalIdx(pub U32);
impl Into<usize> for GlobalIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for GlobalIdx { fn from(a: usize) -> Self { GlobalIdx(a as u32) } }

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct LocalIdx(pub U32);
impl Into<usize> for LocalIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for LocalIdx { fn from(a: usize) -> Self { LocalIdx(a as u32) } }

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct LabelIdx(pub U32);
impl Into<usize> for LabelIdx { fn into(self) -> usize { self.0 as usize } }
impl From<usize> for LabelIdx { fn from(a: usize) -> Self { LabelIdx(a as u32) } }


#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub type_: TypeIdx,
    pub locals: Wec<ValType>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Table {
    pub type_: TableType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Mem {
    pub type_: MemType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Global {
    pub type_: GlobalType,
    pub init: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Elem {
    pub table: TableIdx,
    pub offset: Expr,
    pub init: Wec<FuncIdx>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Data {
    pub data: MemIdx,
    pub offset: Expr,
    pub init: Wec<Byte>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Start {
    pub func: FuncIdx,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Export {
    pub name: Name,
    pub desc: ExportDesc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub desc: ImportDesc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
