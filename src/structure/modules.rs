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

#[derive(Debug, PartialEq)]
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

pub type TypeIdx = U32;
pub type FuncIdx = U32;
pub type TableIdx = U32;
pub type MemIdx = U32;
pub type GlobalIdx = U32;
pub type LocalIdx = U32;
pub type LabelIdx = U32;

#[derive(Debug, PartialEq)]
pub struct Func {
    pub type_: TypeIdx,
    pub locals: Wec<ValType>,
    pub body: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Table {
    pub type_: TableType,
}

#[derive(Debug, PartialEq)]
pub struct Mem {
    pub type_: MemType,
}

#[derive(Debug, PartialEq)]
pub struct Global {
    pub type_: GlobalType,
    pub init: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Elem {
    pub table: TableIdx,
    pub offset: Expr,
    pub init: Wec<FuncIdx>,
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub data: MemIdx,
    pub offset: Expr,
    pub init: Wec<Byte>,
}

#[derive(Debug, PartialEq)]
pub struct Start {
    pub func: FuncIdx,
}

#[derive(Debug, PartialEq)]
pub struct Export {
    pub name: Name,
    pub desc: ExportDesc,
}

#[derive(Debug, PartialEq)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, PartialEq)]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub desc: ImportDesc,
}

#[derive(Debug, PartialEq)]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
