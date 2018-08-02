use super::types::U32;
use super::types::I32;
use super::types::I64;
use super::types::F32;
use super::types::F64;
use super::types::ResultType;
use super::types::Wec;

use super::modules::LocalIdx;
use super::modules::GlobalIdx;
use super::modules::LabelIdx;
use super::modules::FuncIdx;
use super::modules::TypeIdx;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Memarg {
    pub offset: U32,
    pub align: U32,
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub body: Vec<Instr>,
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    // numeric instructions
    I32Const(I32),
    I64Const(I64),
    F32Const(F32),
    F64Const(F64),

    I32Clz,
    I32Ctz,
    I32Popcnt,

    I64Clz,
    I64Ctz,
    I64Popcnt,

    F32Abs,
    F32Neg,
    F32Sqrt,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,

    F64Abs,
    F64Neg,
    F64Sqrt,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,

    I32Add,
    I32Sub,
    I32Mul,
    I32DivU,
    I32DivS,
    I32RemU,
    I32RemS,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrU,
    I32ShrS,
    I32Rotl,
    I32Rotr,

    I64Add,
    I64Sub,
    I64Mul,
    I64DivU,
    I64DivS,
    I64RemU,
    I64RemS,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrU,
    I64ShrS,
    I64Rotl,
    I64Rotr,

    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32CopySign,

    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64CopySign,

    I32EqZ,
    I64EqZ,

    I32Eq,
    I32Ne,
    I32LtU,
    I32LtS,
    I32GtU,
    I32GtS,
    I32LeU,
    I32LeS,
    I32GeU,
    I32GeS,

    I64Eq,
    I64Ne,
    I64LtU,
    I64LtS,
    I64GtU,
    I64GtS,
    I64LeU,
    I64LeS,
    I64GeU,
    I64GeS,

    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    I32WrapI64,
    I64ExtendUI32,
    I64ExtendSI32,

    I32TruncUF32,
    I32TruncUF64,
    I32TruncSF32,
    I32TruncSF64,
    I64TruncUF32,
    I64TruncUF64,
    I64TruncSF32,
    I64TruncSF64,

    F32DemoteF64,
    F64PromoteF32,

    F32ConvertUI32,
    F64ConvertUI32,
    F32ConvertSI32,
    F64ConvertSI32,
    F32ConvertUI64,
    F64ConvertUI64,
    F32ConvertSI64,
    F64ConvertSI64,

    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,

    // parametric instructions
    Drop,
    Select,

    // variable instructions
    GetLocal(LocalIdx),
    SetLocal(LocalIdx),
    TeeLocal(LocalIdx),
    GetGlobal(GlobalIdx),
    SetGlobal(GlobalIdx),

    // memory instructions
    I32Load(Memarg),
    I64Load(Memarg),
    F32Load(Memarg),
    F64Load(Memarg),

    I32Store(Memarg),
    I64Store(Memarg),
    F32Store(Memarg),
    F64Store(Memarg),

    I32Load8U(Memarg),
    I32Load8S(Memarg),
    I64Load8U(Memarg),
    I64Load8S(Memarg),

    I32Load16U(Memarg),
    I32Load16S(Memarg),
    I64Load16U(Memarg),
    I64Load16S(Memarg),

    I64Load32U(Memarg),
    I64Load32S(Memarg),

    I32Store8(Memarg),
    I64Store8(Memarg),
    I32Store16(Memarg),
    I64Store16(Memarg),
    I64Store32(Memarg),

    CurrentMemory,
    GrowMemory,

    // control instructions
    Nop,
    Unreachable,
    Block(ResultType, Vec<Instr>),
    Loop(ResultType, Vec<Instr>),
    IfElse(ResultType, Vec<Instr>, Vec<Instr>),
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable(Wec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx),
}
