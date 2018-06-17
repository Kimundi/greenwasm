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

impl Ctx {
    fn local_defined(&self, x: u32) -> VResult {
        unimplemented!()
    }
    fn local_type(&self, x: u32) -> ValType {
        unimplemented!()
    }
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

        enum AnyValType {
            I32,
            I64,
            F32,
            F64,
            Any(char),
        }
        impl From<ValType> for AnyValType {
            fn from(other: ValType) -> Self {
                match other {
                    I32 => AnyValType::I32,
                    I64 => AnyValType::I64,
                    F32 => AnyValType::F32,
                    F64 => AnyValType::F64,
                }
            }
        }
        let any = |t| AnyValType::Any(t);

        struct AnyFuncType {
            args: Vec<AnyValType>,
            results: Vec<AnyValType>,
        }

        macro_rules! fty{
            ($($a:expr),*;$($r:expr),*) => (AnyFuncType {
                args: vec![$($a.into()),*],
                results: vec![$($r.into()),*],
            })
        }

        let ty = match *instruction {
            // numeric instructions
            TConst(x)       => fty![               ; x.ty()],
            TUnop(x)        => fty![x.ty()         ; x.ty()],
            TBinop(x)       => fty![x.ty(), x.ty() ; x.ty()],
            IxxTestop(x, _) => fty![x.ty()         ; I32   ],
            TRelop(x)       => fty![x.ty(), x.ty() ; x.ty()],
            // cvtops
            TReinterpret(t)          => fty![t.from_ty() ; t.ty() ],
            IxxTruncFxx(t2, _, t1)   => fty![t1.ty()     ; t2.ty()],
            I32WrapI64               => fty![I64         ; I32    ],
            I64ExtendI32(_)          => fty![I32         ; I64    ],
            FxxConvertUxx(t2, _, t1) => fty![t1.ty()     ; t2.ty()],
            F32DemoteF64             => fty![F64         ; F32    ],
            F64PromoteF32            => fty![F32         ; F64    ],

            // parametric instructions
            Drop   => fty![any('t')                ;         ],
            Select => fty![any('t'), any('t'), I32 ; any('t')],

            // variable instructions
            GetLocal(x) => {
                self.local_defined(x)?;
                let t = self.local_type(x);
                fty![ ; t]
            }
            SetLocal(x) => {
                self.local_defined(x)?;
                let t = self.local_type(x);
                fty![t ; ]
            }
            TeeLocal(x) => {
                self.local_defined(x)?;
                let t = self.local_type(x);
                fty![t ; t]
            }

            _ => panic!(),
        };

        // TODO: What to do with type?

        panic!()
    });

    valid!(self, instruction_sequence: [Instr], {

    });
}
