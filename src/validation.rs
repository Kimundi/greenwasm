use super::structure::types::FuncType;
use super::structure::types::TableType;
use super::structure::types::MemType;
use super::structure::types::GlobalType;
use super::structure::types::ValType;
use super::structure::types::ResultType;
use super::structure::types::Limits;

use super::structure::instructions::Instr;

#[derive(Default)]
pub struct Ctx {
    pub types: Vec<FuncType>,
    pub funcs: Vec<FuncType>,
    pub tables: Vec<TableType>,
    pub mems: Vec<MemType>,
    pub globals: Vec<GlobalType>,
    pub locals: Vec<ValType>,
    pub labels: Vec<ResultType>,
    pub return_: Option<ResultType>,
}

pub type VResult = Result<(), ValidationError>;
pub struct ValidationError {
    pub kind: ValidationErrorEnum,
}
use self::ValidationErrorEnum::*;

impl Ctx {
    fn error(&self, error: ValidationErrorEnum) -> VResult {
        Err(ValidationError {
            kind: error
        })
    }

    fn ok(&self) -> VResult {
        Ok(())
    }
}

pub enum ValidationErrorEnum {
    LimitMaxSmallerMin,
    FunctionTypeResultArityGreaterOne,
}

macro_rules! valid {
    ($self:ident, $name:ident: $type:ty, $b:block) => (
        pub fn $name(&$self, $name: &$type) -> VResult {
            $b
            $self.ok()
        }
    )
}

impl Ctx {
    valid!(self, limit: Limits, {
        if limit.max.unwrap_or(0) < limit.min {
            self.error(LimitMaxSmallerMin)?
        }
    });

    valid!(self, function_type: FuncType, {
        if function_type.results.len() > 1 {
            self.error(FunctionTypeResultArityGreaterOne)?
        }
    });

    valid!(self, table_type: TableType, {
        self.limit(&table_type.limits)?;
    });

    valid!(self, memory_type: MemType, {
        self.limit(&memory_type.limits)?;
    });

    valid!(self, _global_types: GlobalType, {
    });

    valid!(self, instruction: Instr, {
        use self::Instr::*;
        use self::ValType::*;

        macro_rules! fty{
            ($($a:expr),*;$($r:expr),*) => (FuncType {
                args: vec![$($a),*],
                results: vec![$($r),*],
            })
        }

        let ty = match *instruction {
            I32Const(_) => fty![; I32],
            I64Const(_) => fty![; I64],
            F32Const(_) => fty![; F32],
            F64Const(_) => fty![; F64],


            _ => panic!(),
        };
    });

    valid!(self, instruction_sequence: [Instr], {

    });
}
