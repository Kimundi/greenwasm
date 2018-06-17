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
    InstrSetGlobalNotVar,
    InstrLoadOveraligned,
    InstrStoreOveraligned,
}

impl Ctx {
    fn local(&self, x: u32) -> Result<ValType, ValidationError> {
        unimplemented!()
    }
    fn global(&self, x: u32) -> Result<GlobalType, ValidationError> {
        unimplemented!()
    }
    fn mem(&self, x: u32) -> Result<MemType, ValidationError> {
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
                let t = self.local(x)?;
                fty![ ; t]
            }
            SetLocal(x) => {
                let t = self.local(x)?;
                fty![t ; ]
            }
            TeeLocal(x) => {
                let t = self.local(x)?;
                fty![t ; t]
            }
            GetGlobal(x) => {
                let mut_t = self.global(x)?;
                let t = mut_t.valtype;
                fty![ ; t]
            }
            SetGlobal(x) => {
                let mut_t = self.global(x)?;
                let t = mut_t.valtype;
                use super::structure::types::Mut;
                if let Mut::Var = mut_t.mutability {
                    fty![t ; ]
                } else {
                    return self.error(InstrSetGlobalNotVar);
                }
            }

            // memory instructions
            ref load_store_instr @ TLoad(..) |
            ref load_store_instr @ IxxLoad8(..) |
            ref load_store_instr @ IxxLoad16(..) |
            ref load_store_instr @ I64Load32(..) |
            ref load_store_instr @ TStore(..) |
            ref load_store_instr @ IxxStore8(..) |
            ref load_store_instr @ IxxStore16(..) |
            ref load_store_instr @ I64Store32(..) => {
                use super::structure::instructions::Memarg;

                let validate = |t: ValType, memarg: Memarg, bit_width, e, r| {
                    self.mem(0)?;
                    let align = 1u32 << memarg.align;
                    if align > (bit_width / 8) {
                        self.error(e)?;
                    }
                    Ok(r)
                };
                let load = |t: ValType, memarg: Memarg, bit_width| {
                    validate(t, memarg, bit_width,
                             InstrLoadOveraligned, fty![I32 ; t])
                };
                let store = |t: ValType, memarg: Memarg, bit_width| {
                    validate(t, memarg, bit_width,
                             InstrStoreOveraligned, fty![I32, t ; ])
                };

                match *load_store_instr {
                    TLoad(t, memarg)        => load(t, memarg, t.bit_width())?,
                    IxxLoad8(t, _, memarg)  => load(t.ty(), memarg, 8)?,
                    IxxLoad16(t, _, memarg) => load(t.ty(), memarg, 16)?,
                    I64Load32(_, memarg)    => load(I64, memarg, 32)?,

                    TStore(t, memarg)       => store(t, memarg, t.bit_width())?,
                    IxxStore8(t, memarg)    => store(t.ty(), memarg, 8)?,
                    IxxStore16(t, memarg)   => store(t.ty(), memarg, 16)?,
                    I64Store32(memarg)      => store(I64, memarg, 32)?,

                    _ => unreachable!(),
                }
            }

            _ => unimplemented!(),
        };

        // TODO: What to do with type?

        unimplemented!()
    });

    valid!(self, instruction_sequence: [Instr], {

    });
}
