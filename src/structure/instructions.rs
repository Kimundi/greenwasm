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

pub enum IUnop {
    Clz, Ctz, Popcnt,
}

pub enum IBinop {
    Add, Sub, Mul, DivU, DivS, RemU, RemS,
    And, Or, Xor, Shl, ShrU, ShrS, Rotl, Rotr,
}

pub enum FUnop {
    Abs, Neg, Sqrt, Ceil, Floor, Trunc, Nearest,
}

pub enum FBinop {
    Add, Sub, Mul, Div, Min, Max, CopySign,
}

pub enum ITestop {
    EqZ,
}

pub enum IRelop {
    Eq, Ne, LtU, LtS, GtU, GtS, LeU, LeS, GeU, GeS,
}

pub enum FRelop {
    Eq, Ne, Lt, Gt, Le, Ge,
}

pub struct Memarg {
    pub offset: U32,
    pub align: U32,
}

pub struct Expr {
    pub body: Vec<Instr>,
}

pub enum Instr {
    // numeric instructions
    I32Const(I32),      I64Const(I64),    F32Const(F32),    F64Const(F64),
    I32Unop(IUnop),     I64Unop(IUnop),   F32Unop(FUnop),   F64Unop(FUnop),
    I32Binop(IBinop),   I64Binop(IBinop), F32Binop(FBinop), F64Binop(FBinop),
    I32Testop(ITestop), I64Testop(ITestop),
    I32Relop(IRelop),   I64Relop(IRelop), F32Relop(FRelop), F64Relop(FRelop),

    I32WrapI64, I64ExtendUI32, I64ExtendSI32,

    I32TruncUF32, I64TruncUF32, I32TruncSF32, I64TruncSF32,
    I32TruncUF64, I64TruncUF64, I32TruncSF64, I64TruncSF64,

    F32DemoteF64, F64PromoteF32,

    F32ConvertUI32, F64ConvertUi32, F32ConvertSI32, F64ConvertSI32,
    F32ConvertUI64, F64ConvertUI64, F32ConvertSI64, F64ConvertSI64,

    I32ReinterpretF32, I64ReinterpretF64, F32ReinterpretI32, F64ReinterpretI64,

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
    I32Load(Memarg), I64Load(Memarg), F32Load(Memarg), F64Load(Memarg),
    I32Store(Memarg), I64Store(Memarg), F32Store(Memarg), F64Store(Memarg),

    I32Load8U(Memarg), I64Load8U(Memarg), I32Load8S(Memarg), I64Load8S(Memarg),
    I32Load16U(Memarg), I64Load16U(Memarg), I32Load16S(Memarg), I64Load16S(Memarg),
    I64Load32U(Memarg), I64Load32S(Memarg),

    I32Store8(Memarg), I64Store8(Memarg),
    I32Store16(Memarg), I64Store16(Memarg),
    I64Store32(Memarg),

    CurrentMemory,
    GrowMemory,

    // control instructions
    Nop,
    Unreachable,
    Block(ResultType, Vec<Instr>),
    Loop(ResultType, Vec<Instr>),
    If(ResultType, Vec<Instr>, Vec<Instr>),
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable(Wec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx),
}
