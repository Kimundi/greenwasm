use super::structure::types::FuncType;
use super::structure::types::TableType;
use super::structure::types::MemType;
use super::structure::types::GlobalType;
use super::structure::types::ValType;
use super::structure::types::ResultType;
use super::structure::types::Limits;
use super::structure::types::Mut;
use super::structure::types::ElemType;

use super::structure::instructions::Instr;
use super::structure::instructions::Expr;
use super::structure::instructions::Memarg;

use super::structure::modules::Func;
use super::structure::modules::Table;
use super::structure::modules::Mem;

pub type VResult<T> = Result<T, ValidationError>;
pub struct ValidationError {
    pub kind: ValidationErrorEnum,
}
pub enum ValidationErrorEnum {
    LimitMaxSmallerMin,
    FunctionTypeResultArityGreaterOne,
    InstrSetGlobalNotVar,
    InstrLoadOveraligned,
    InstrStoreOveraligned,
    InstrBrTableNotSameLabelType,
    InstrCallIndirectElemTypeNotAnyFunc,
    ConstExprGetGlobalNotConst,
    ConstExprIlligalInstruction,
}
use self::ValidationErrorEnum::*;

enum CtxMember<'a, T: 'a> {
    Set(T),
    Prepended(T, &'a CtxMember<'a, T>),
    Delegated(&'a CtxMember<'a, T>),
}

pub struct Ctx<'a> {
    types:   CtxMember<'a, Vec<FuncType>>,
    funcs:   CtxMember<'a, Vec<FuncType>>,
    tables:  CtxMember<'a, Vec<TableType>>,
    mems:    CtxMember<'a, Vec<MemType>>,
    globals: CtxMember<'a, Vec<GlobalType>>,
    locals:  CtxMember<'a, Vec<ValType>>,
    labels:  CtxMember<'a, Vec<ResultType>>,
    return_: CtxMember<'a, ResultType>,
}

impl<'a> Ctx<'a> {
    fn error(&self, error: ValidationErrorEnum) -> VResult<()> {
        Err(ValidationError {
            kind: error
        })
    }

    fn locals(&self, _x: u32) -> VResult<ValType> {
        unimplemented!()
    }
    fn globals(&self, _x: u32) -> VResult<GlobalType> {
        unimplemented!()
    }
    fn mems(&self, _x: u32) -> VResult<MemType> {
        unimplemented!()
    }
    fn labels(&self, _x: u32) -> VResult<ResultType> {
        unimplemented!()
    }
    fn return_(&self) -> VResult<ResultType> {
        unimplemented!()
    }
    fn funcs(&self, _x: u32) -> VResult<FuncType> {
        unimplemented!()
    }
    fn tables(&self, _x: u32) -> VResult<TableType> {
        unimplemented!()
    }
    fn types(&self, _x: u32) -> VResult<FuncType> {
        unimplemented!()
    }
    fn with(&'a self) -> Ctx<'a> {
        Ctx {
            types:   CtxMember::Delegated(&self.types),
            funcs:   CtxMember::Delegated(&self.funcs),
            tables:  CtxMember::Delegated(&self.tables),
            mems:    CtxMember::Delegated(&self.mems),
            globals: CtxMember::Delegated(&self.globals),
            locals:  CtxMember::Delegated(&self.locals),
            labels:  CtxMember::Delegated(&self.labels),
            return_: CtxMember::Delegated(&self.return_),
        }
    }
    fn prepend_label(mut self, label: ResultType) -> Ctx<'a> {
        if let CtxMember::Delegated(r) = self.labels {
            self.labels = CtxMember::Prepended(vec![label], r);
        } else {
            panic!("can only overwrite Delegated()");
        }
        self
    }
    fn set_locals(mut self, locals: Vec<ValType>) -> Ctx<'a> {
        if let CtxMember::Delegated(r) = self.locals {
            self.locals = CtxMember::Prepended(locals, r);
        } else {
            panic!("can only overwrite Delegated()");
        }
        self
    }
    fn set_label(mut self, label: ResultType) -> Ctx<'a> {
        if let CtxMember::Delegated(r) = self.labels {
            self.labels = CtxMember::Prepended(vec![label], r);
        } else {
            panic!("can only overwrite Delegated()");
        }
        self
    }
    fn set_return_(mut self, return_: ResultType) -> Ctx<'a> {
        if let CtxMember::Delegated(r) = self.return_ {
            self.return_ = CtxMember::Prepended(return_, r);
        } else {
            panic!("can only overwrite Delegated()");
        }
        self
    }
    // TODO: move out or change
    fn find_ty_prefix(&self, _t2: &[AnyValType], _t: &[AnyValType])
        -> VResult<Vec<AnyValType>>
    {
        unimplemented!()
    }
    // TODO: move out or change
    fn any_vec_to_option(&self, vec: Vec<AnyValType>) -> Option<AnyValType> {
        unimplemented!()
    }
    // TODO: move out or change
    fn vec_to_option(&self, vec: Vec<ValType>) -> Option<ValType> {
        unimplemented!()
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum AnyValType {
    I32,
    I64,
    F32,
    F64,
    Any(char),
    AnySeq(char),
    AnyOpt(char),
}
fn any(t: char) -> AnyValType {
    AnyValType::Any(t)
}
fn any_seq(t: char) -> AnyValType {
    AnyValType::AnySeq(t)
}
fn any_opt(t: char) -> AnyValType {
    AnyValType::AnyOpt(t)
}

impl From<ValType> for AnyValType {
    fn from(other: ValType) -> Self {
        match other {
            ValType::I32 => AnyValType::I32,
            ValType::I64 => AnyValType::I64,
            ValType::F32 => AnyValType::F32,
            ValType::F64 => AnyValType::F64,
        }
    }
}

trait AnyValTypeBuilder<T> {
    fn append(self, e: T) -> Self;
}
impl AnyValTypeBuilder<ValType> for Vec<AnyValType> {
    fn append(mut self, e: ValType) -> Self {
        self.push(e.into());
        self
    }
}
impl AnyValTypeBuilder<ResultType> for Vec<AnyValType> {
    fn append(mut self, e: ResultType) -> Self {
        self.extend(e.map(|x| -> AnyValType { x.into() }));
        self
    }
}
impl AnyValTypeBuilder<AnyValType> for Vec<AnyValType> {
    fn append(mut self, e: AnyValType) -> Self {
        self.push(e);
        self
    }
}
impl AnyValTypeBuilder<Vec<AnyValType>> for Vec<AnyValType> {
    fn append(mut self, e: Vec<AnyValType>) -> Self {
        self.extend(e);
        self
    }
}
impl AnyValTypeBuilder<Vec<ValType>> for Vec<AnyValType> {
    fn append(mut self, e: Vec<ValType>) -> Self {
        self.extend(e.into_iter().map(|x| -> AnyValType { x.into() }));
        self
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct AnyFuncType {
    args: Vec<AnyValType>,
    results: Vec<AnyValType>,
}
impl From<FuncType> for AnyFuncType {
    fn from(FuncType { args, results }: FuncType) -> Self {
        AnyFuncType {
            args: args.into_iter().map(|x| x.into()).collect(),
            results: results.into_iter().map(|x| x.into()).collect(),
        }
    }
}

pub type AnyResultType = Option<AnyValType>;

macro_rules! ty {
    ($($a:expr),*;$($r:expr),*) => (AnyFuncType {
        args:    { let v = vec![]; $( let v = v.append($a); )* v },
        results: { let v = vec![]; $( let v = v.append($r); )* v },
    })
}

trait MustBeValidWith {
    fn must_by_valid_with(&self, expected: &Self) -> VResult<()>;
}

impl MustBeValidWith for AnyFuncType {
    fn must_by_valid_with(&self, expected: &Self) -> VResult<()> {
        unimplemented!()
    }
}

impl MustBeValidWith for AnyResultType {
    fn must_by_valid_with(&self, expected: &Self) -> VResult<()> {
        unimplemented!()
    }
}

macro_rules! valid_with {
    (($ctx:ident, $name:ident: $type:ty) -> $rt:ty $b:block) => (
        pub fn $name($ctx: &Ctx, $name: &$type) -> VResult<$rt> {
            let ty = $b;
            Ok(ty)
        }
    )
}

pub mod validate {
    use super::*;

    valid_with!((c, limit: Limits) -> () {
        if limit.max.unwrap_or(0) < limit.min {
            c.error(LimitMaxSmallerMin)?
        }
    });

    valid_with!((c, function_type: FuncType) -> () {
        if function_type.results.len() > 1 {
            c.error(FunctionTypeResultArityGreaterOne)?
        }
    });

    valid_with!((c, table_type: TableType) -> () {
        validate::limit(c, &table_type.limits)?;
    });

    valid_with!((c, memory_type: MemType) -> () {
        validate::limit(c, &memory_type.limits)?;
    });

    valid_with!((c, _global_types: GlobalType) -> () {
    });

    valid_with!((c, instruction: Instr) -> AnyFuncType {
        use self::Instr::*;
        use self::ValType::*;

        let ty = match *instruction {
            // numeric instructions
            TConst(x)       => ty![               ; x.ty()],
            TUnop(x)        => ty![x.ty()         ; x.ty()],
            TBinop(x)       => ty![x.ty(), x.ty() ; x.ty()],
            IxxTestop(x, _) => ty![x.ty()         ; I32   ],
            TRelop(x)       => ty![x.ty(), x.ty() ; x.ty()],
            // cvtops
            TReinterpret(t)          => ty![t.from_ty() ; t.ty() ],
            IxxTruncFxx(t2, _, t1)   => ty![t1.ty()     ; t2.ty()],
            I32WrapI64               => ty![I64         ; I32    ],
            I64ExtendI32(_)          => ty![I32         ; I64    ],
            FxxConvertUxx(t2, _, t1) => ty![t1.ty()     ; t2.ty()],
            F32DemoteF64             => ty![F64         ; F32    ],
            F64PromoteF32            => ty![F32         ; F64    ],

            // parametric instructions
            Drop   => ty![any('t')                ;         ],
            Select => ty![any('t'), any('t'), I32 ; any('t')],

            // variable instructions
            GetLocal(x) => {
                let t = c.locals(x)?;
                ty![ ; t]
            }
            SetLocal(x) => {
                let t = c.locals(x)?;
                ty![t ; ]
            }
            TeeLocal(x) => {
                let t = c.locals(x)?;
                ty![t ; t]
            }
            GetGlobal(x) => {
                let mut_t = c.globals(x)?;
                let t = mut_t.valtype;
                ty![ ; t]
            }
            SetGlobal(x) => {
                let mut_t = c.globals(x)?;
                let t = mut_t.valtype;
                if mut_t.mutability != Mut::Var  {
                    c.error(InstrSetGlobalNotVar)?;
                }
                ty![t ; ]
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
                let validate = |t: ValType, memarg: Memarg, bit_width, e, r| {
                    c.mems(0)?;
                    let align = 1u32 << memarg.align;
                    if align > (bit_width / 8) {
                        c.error(e)?;
                    }
                    Ok(r)
                };
                let load = |t: ValType, memarg: Memarg, bit_width| {
                    validate(t, memarg, bit_width,
                             InstrLoadOveraligned, ty![I32 ; t])
                };
                let store = |t: ValType, memarg: Memarg, bit_width| {
                    validate(t, memarg, bit_width,
                             InstrStoreOveraligned, ty![I32, t ; ])
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
            CurrentMemory => { c.mems(0)?; ty![    ; I32] }
            GrowMemory    => { c.mems(0)?; ty![I32 ; I32] }

            // control instructions
            Nop => ty![ ; ],
            Unreachable => ty![any_seq('t') ; any_seq('u')],
            Block(resulttype, ref block) => {
                let c_ = c.with().prepend_label(resulttype);
                let ty = ty![ ; resulttype];
                validate::instruction_sequence(&c_, block)?.must_by_valid_with(&ty)?;
                ty
            }
            Loop(resulttype, ref block) => {
                let c_ = c.with().prepend_label(None);
                let ty = ty![ ; resulttype];
                validate::instruction_sequence(&c_, block)?.must_by_valid_with(&ty)?;
                ty
            }
            IfElse(resulttype, ref if_block, ref else_block) => {
                let c_ = c.with().prepend_label(resulttype);
                let ty = ty![ ; resulttype];
                validate::instruction_sequence(&c_, if_block)?.must_by_valid_with(&ty)?;
                validate::instruction_sequence(&c_, else_block)?.must_by_valid_with(&ty)?;
                ty![I32 ; resulttype]
            }
            Br(labelidx) => {
                let resulttype = c.labels(labelidx)?;
                ty![any_seq('t'), resulttype ; any_seq('u')]
            }
            BrIf(labelidx) => {
                let resulttype = c.labels(labelidx)?;
                ty![resulttype, I32; resulttype]
            }
            BrTable(ref labelindices, labelidx_n) => {
                let resulttype = c.labels(labelidx_n)?;
                for &li in labelindices {
                    let resulttype_i = c.labels(li)?;
                    if resulttype_i != resulttype {
                        c.error(InstrBrTableNotSameLabelType)?;
                    }
                }
                ty![any_seq('t'), resulttype, I32 ; any_seq('u')]
            }
            Return => {
                let resulttype = c.return_()?;
                ty![any_seq('t'), resulttype ; any_seq('u')]
            }
            Call(x) => {
                c.funcs(x)?.into()
            }
            CallIndirect(x) => {
                let TableType {
                    limits,
                    elemtype,
                } = c.tables(0)?;
                if elemtype != ElemType::AnyFunc {
                    c.error(InstrCallIndirectElemTypeNotAnyFunc)?;
                }
                let ty = c.types(x)?;
                ty![ty.args, I32 ; ty.results]
            }
        };

        ty
    });

    valid_with!((c, instruction_sequence: [Instr]) -> AnyFuncType {
        let mut instrs_ty = ty![any_seq('t') ; any_seq('t')];

        for instr_n in instruction_sequence {
            let AnyFuncType {
                args:    t1,
                results: t2,
            } = instrs_ty;
            let AnyFuncType {
                args:    t,
                results: t3,
            } = validate::instruction(&c, instr_n)?;
            let t0 = c.find_ty_prefix(&t2, &t)?;
            instrs_ty = ty![t1 ; t0, t3];
        }

        instrs_ty
    });

    valid_with!((c, expr: Expr) -> AnyResultType {
        let instrs_ty = validate::instruction_sequence(&c, &expr.body)?;
        instrs_ty.must_by_valid_with(&ty![ ; any_opt('t')])?;
        c.any_vec_to_option(instrs_ty.results)
    });

    valid_with!((c, const_expr: Expr) -> AnyResultType {
        for instr in &const_expr.body {
            use self::Instr::*;
            match *instr {
                TConst(_) => (),
                GetGlobal(x) => {
                    if c.globals(x)?.mutability != Mut::Const {
                        c.error(ConstExprGetGlobalNotConst)?;
                    }
                }
                _ => {
                    c.error(ConstExprIlligalInstruction)?;
                }
            }
        }

        validate::expr(c, const_expr)?
    });

    valid_with!((c, func: Func) -> FuncType {
        let Func { type_: x, locals: t, body: expr } = func;
        let ty = c.types(*x)?;

        let locals = ty.args.iter().chain(t).cloned().collect();
        let result = c.vec_to_option(ty.results.clone());

        let c_ = c.with()
            .set_locals(locals)
            .set_label(result)
            .set_return_(result);

        validate::expr(&c_, expr)?.must_by_valid_with(&result.map(|x| x.into()))?;

        ty
    });

    valid_with!((c, table: Table) -> TableType {
        validate::table_type(c, &table.type_)?;
        table.type_
    });

    valid_with!((c, mem: Mem) -> MemType {
        validate::memory_type(c, &mem.type_)?;
        mem.type_
    });
}
