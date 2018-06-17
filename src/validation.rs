use super::structure::types::FuncType;
use super::structure::types::TableType;
use super::structure::types::MemType;
use super::structure::types::GlobalType;
use super::structure::types::ValType;
use super::structure::types::ResultType;
use super::structure::types::Limits;

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

pub enum ValidationErrorEnum {
    LimitMaxSmallerMin,
}

impl Ctx {
    fn error(&self, error: ValidationErrorEnum) -> VResult {
        Err(ValidationError {
            kind: error
        })
    }

    fn ok(&self) -> VResult {
        Ok(())
    }

    pub fn limit(&self, limit: &Limits) -> VResult {
        if limit.max.unwrap_or(0) < limit.min {
            self.error(LimitMaxSmallerMin)?
        }

        self.ok()
    }
}
