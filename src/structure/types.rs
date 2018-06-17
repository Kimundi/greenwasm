//! Types according to the _structure_ section of the spec

pub mod conventions;

// 2.1.3 Vectors

/// vec(A)
// TODO: Ensure size < 2^32-1
pub type Wec<A> = Vec<A>;

// 2.2 Values
// 2.2.1 Bytes

pub type Byte = u8;

// 2.2.2 Integers

pub type U8 = u8;
pub type S8 = i8;
pub type I8 = U8;

pub type U16 = u16;
pub type S16 = i16;
pub type I16 = U16;

pub type U32 = u32;
pub type S32 = i32;
pub type I32 = U32;

pub type U64 = u64;
pub type S64 = i64;
pub type I64 = U64;

// TODO: Make sure the native types behaves according to the standard
// in regard to rounding, subnormal values, NaNs, etc.
pub type F32 = f32;
pub type F64 = f64;

// TODO: Ensure size < 2^32-1
pub type Name = String;
pub type Codepoint = char;

pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

// TODO. What does the [] notation in the grammar spec mean exactly?
// Eg, `resulttype​::=​[valtype?]​`

pub type ResultType = Option<ValType>;

pub struct FuncType {
    pub args: Wec<ValType>,
    pub results: Wec<ValType>,
}

pub struct Limits {
    pub min: U32,
    pub max: Option<U32>,
}

pub struct MemType {
    pub limits: Limits,
}

pub struct TableType {
    pub limits: Limits,
    pub elemtype: ElemType,
}

pub enum ElemType {
    AnyFunc,
}

pub struct GlobalType {
    pub mutability: Mut,
    pub valtype: ValType,
}

pub enum Mut {
    Const,
    Var,
}

pub enum ExternType {
    Func(FuncType),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
