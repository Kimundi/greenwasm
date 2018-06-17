use super::types::U32;
use super::types::I32;
use super::types::I64;
use super::types::F32;
use super::types::F64;
use super::types::ResultType;
use super::types::Wec;
use super::types::ValType;

use super::modules::LocalIdx;
use super::modules::GlobalIdx;
use super::modules::LabelIdx;
use super::modules::FuncIdx;
use super::modules::TypeIdx;

#[derive(Copy, Clone)]
pub enum IUnop {
    Clz, Ctz, Popcnt,
}

#[derive(Copy, Clone)]
pub enum IBinop {
    Add, Sub, Mul, DivU, DivS, RemU, RemS,
    And, Or, Xor, Shl, ShrU, ShrS, Rotl, Rotr,
}

#[derive(Copy, Clone)]
pub enum FUnop {
    Abs, Neg, Sqrt, Ceil, Floor, Trunc, Nearest,
}

#[derive(Copy, Clone)]
pub enum FBinop {
    Add, Sub, Mul, Div, Min, Max, CopySign,
}

#[derive(Copy, Clone)]
pub enum ITestop {
    EqZ,
}

#[derive(Copy, Clone)]
pub enum IRelop {
    Eq, Ne, LtU, LtS, GtU, GtS, LeU, LeS, GeU, GeS,
}

#[derive(Copy, Clone)]
pub enum FRelop {
    Eq, Ne, Lt, Gt, Le, Ge,
}

#[derive(Copy, Clone)]
pub struct Memarg {
    pub offset: U32,
    pub align: U32,
}

pub struct Expr {
    pub body: Vec<Instr>,
}

/// Helper types to encode instructions more concise
mod helper {
    use super::*;

    #[derive(Copy, Clone)]
    pub enum TConst {
        I32(I32),
        I64(I64),
        F32(F32),
        F64(F64),
    }
    impl TConst {
        pub fn ty(&self) -> ValType {
            match *self {
                TConst::I32(_) => ValType::I32,
                TConst::I64(_) => ValType::I64,
                TConst::F32(_) => ValType::F32,
                TConst::F64(_) => ValType::F64,
            }
        }
    }

    #[derive(Copy, Clone)]
    pub enum Ixx {
        I32,
        I64,
    }
    impl Ixx {
        pub fn ty(&self) -> ValType {
            match *self {
                Ixx::I32 => ValType::I32,
                Ixx::I64 => ValType::I64,
            }
        }
    }

    #[derive(Copy, Clone)]
    pub enum Fxx {
        F32,
        F64,
    }
    impl Fxx {
        pub fn ty(&self) -> ValType {
            match *self {
                Fxx::F32 => ValType::F32,
                Fxx::F64 => ValType::F64,
            }
        }
    }

    #[derive(Copy, Clone)]
    pub enum Unop {
        IUnop(Ixx, IUnop),
        FUnop(Fxx, FUnop),
    }
    impl Unop {
        pub fn ty(&self) -> ValType {
            match *self {
                Unop::IUnop(x, _) => x.ty(),
                Unop::FUnop(x, _) => x.ty(),
            }
        }
    }

    #[derive(Copy, Clone)]
    pub enum Binop {
        IBinop(Ixx, IBinop),
        FBinop(Fxx, FBinop),
    }
    impl Binop {
        pub fn ty(&self) -> ValType {
            match *self {
                Binop::IBinop(x, _) => x.ty(),
                Binop::FBinop(x, _) => x.ty(),
            }
        }
    }
}
pub use self::helper::*;

pub enum Instr {
    // numeric instructions
    TConst(TConst),
    TUnop(Unop),
    TBinop(Binop),
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
