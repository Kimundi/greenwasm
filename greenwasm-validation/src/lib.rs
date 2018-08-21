#![feature(macro_at_most_once_rep)]

use structure::types::FuncType;
use structure::types::TableType;
use structure::types::MemType;
use structure::types::GlobalType;
use structure::types::ValType;
use structure::types::ResultType;
use structure::types::Limits;
use structure::types::Mut;
use structure::types::ElemType;
use structure::types::ExternType;

use structure::instructions::Instr;
use structure::instructions::Expr;
use structure::instructions::Memarg;

use structure::modules::Func;
use structure::modules::Table;
use structure::modules::Mem;
use structure::modules::Global;
use structure::modules::Elem;
use structure::modules::Data;
use structure::modules::Start;
use structure::modules::Export;
use structure::modules::ExportDesc;
use structure::modules::Import;
use structure::modules::ImportDesc;
use structure::modules::Module;
use structure::modules::LocalIdx;
use structure::modules::FuncIdx;
use structure::modules::GlobalIdx;
use structure::modules::MemIdx;
use structure::modules::TableIdx;
use structure::modules::TypeIdx;
use structure::modules::LabelIdx;

pub type VResult<T> = Result<T, ValidationError>;
#[derive(Debug)]
pub struct ValidationError {
    pub kind: ValidationErrorEnum,
}
#[derive(Debug)]
pub enum ValidationErrorEnum {
    LimitMaxSmallerMin,
    FunctionTypeResultArityGreaterOne,
    InstrSetGlobalNotVar,
    InstrLoadOveraligned,
    InstrStoreOveraligned,
    InstrBrTableNotSameLabelType,
    InstrCallIndirectElemTypeNotAnyFunc,
    InstrWithTypeNotValid, // TODO: Exact error codes
    ConstExprGetGlobalNotConst,
    ConstExprIlligalInstruction,
    ElemElemTypeNotAnyFunc,
    ExportGlobalNotConst,
    ImportGlobalNotConst,
    ModulePrepassImportFuncTypeIdxDoesNotExist,
    ModuleTablesLengthNotOne,
    ModuleMemsLengthNotOne,
    ModuleExportDuplicateName,
    CtxLocalsIdxDoesNotExist,
    CtxGlobalsIdxDoesNotExist,
    CtxMemsIdxDoesNotExist,
    CtxFuncsIdxDoesNotExist,
    CtxTablesIdxDoesNotExist,
    CtxTypesIdxDoesNotExist,
    CtxLabelsIdxDoesNotExist,
    CtxReturnDoesNotExist,
    StartFunNotValidWithEmptyEmpty,
    MemoryBoundsOutside32Bit,
}
use self::ValidationErrorEnum::*;

/// A context member is a flattened linked list of lists on the stack
enum CtxMember<'a, T: 'a> {
    Unset,
    Set(T),
    Prepended(T, &'a CtxMember<'a, T>),
    Delegated(&'a CtxMember<'a, T>),
}


// TODO: Optimize memory layout later (no needless 1-elem allocations)
// TODO: Special case for return, which should only be a 0 or 1 list
pub struct Ctx<'a> {
    types:   CtxMember<'a, Vec<FuncType>>,
    funcs:   CtxMember<'a, Vec<FuncType>>,
    tables:  CtxMember<'a, Vec<TableType>>,
    mems:    CtxMember<'a, Vec<MemType>>,
    globals: CtxMember<'a, Vec<GlobalType>>,
    locals:  CtxMember<'a, Vec<ValType>>,
    labels:  CtxMember<'a, Vec<ResultType>>,
    return_: CtxMember<'a, Vec<ResultType>>,
}

macro_rules! ctx_set {
    ($fn_name:ident($self:ident, $var_name:ident: $var_type:ty)) => (
        ctx_set!($fn_name($self, $var_name: $var_type)
            -> |_| CtxMember::Set($var_name));
    );
    ($fn_name:ident($self:ident, $var_name:ident: $var_type:ty) -> $mapped:expr) => (
        fn $fn_name(mut $self, $var_name: $var_type) -> Self {
            if let CtxMember::Delegated(r) = $self.$var_name {
                $self.$var_name = $mapped(r);
            } else {
                panic!("can only overwrite Delegated()");
            }
            $self
        }
    )
}

macro_rules! ctx_idx {
    ($self:ident, $name:ident: $type:ty, $idxty:ty, $err:ident) => (
        fn $name(&$self, mut x: $idxty) -> VResult<$type> {
            use self::CtxMember::*;

            let mut cursor = &$self.$name;

            loop {
                match cursor {
                    Delegated(next) => {
                        cursor = next;
                    }
                    Set(v) if (x.0 as usize) < v.len() => {
                        return Ok(v[x.0 as usize].clone());
                    }
                    Prepended(v, _) if (x.0 as usize) < v.len() => {
                        return Ok(v[x.0 as usize].clone());
                    }
                    Prepended(v, next) if (x.0 as usize) >= v.len() => {
                        x.0 -= v.len() as u32;
                        cursor = next;
                    }
                    _ => $self.error($err)?,
                }
            }
        }
    )
}

impl<'a> Ctx<'a> {
    pub fn new() -> Self {
        Ctx {
            types:   CtxMember::Unset,
            funcs:   CtxMember::Unset,
            tables:  CtxMember::Unset,
            mems:    CtxMember::Unset,
            globals: CtxMember::Unset,
            locals:  CtxMember::Unset,
            labels:  CtxMember::Unset,
            return_: CtxMember::Unset,
        }
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

    fn error(&self, error: ValidationErrorEnum) -> VResult<()> {
        Err(ValidationError {
            kind: error
        })
    }

    ctx_idx!(self, locals: ValType,     LocalIdx,  CtxLocalsIdxDoesNotExist );
    ctx_idx!(self, globals: GlobalType, GlobalIdx, CtxGlobalsIdxDoesNotExist);
    ctx_idx!(self, mems: MemType,       MemIdx,    CtxMemsIdxDoesNotExist   );
    ctx_idx!(self, funcs: FuncType,     FuncIdx,   CtxFuncsIdxDoesNotExist  );
    ctx_idx!(self, tables: TableType,   TableIdx,  CtxTablesIdxDoesNotExist );
    ctx_idx!(self, types: FuncType,     TypeIdx,   CtxTypesIdxDoesNotExist  );
    ctx_idx!(self, labels: ResultType,  LabelIdx,  CtxLabelsIdxDoesNotExist );
    ctx_idx!(self, return_: ResultType, LabelIdx,  CtxReturnDoesNotExist    );

    ctx_set!(set_locals(self,  locals:  Vec<ValType>));
    ctx_set!(set_types(self,   types:   Vec<FuncType>));
    ctx_set!(set_funcs(self,   funcs:   Vec<FuncType>));
    ctx_set!(set_tables(self,  tables:  Vec<TableType>));
    ctx_set!(set_mems(self,    mems:    Vec<MemType>));
    ctx_set!(set_globals(self, globals: Vec<GlobalType>));

    ctx_set!(set_label(self, labels: ResultType)
        -> |_| CtxMember::Set(vec![labels]));

    ctx_set!(prepend_label(self, labels: ResultType)
        -> |r| CtxMember::Prepended(vec![labels], r));

    ctx_set!(set_return_(self, return_: ResultType)
        -> |_| CtxMember::Set(vec![return_]));

    fn length_mems(&self) -> u32 {
        use self::CtxMember::*;

        let mut len = 0;
        let mut cursor = &self.mems;
        loop {
            match cursor {
                Delegated(next) => {
                    cursor = next;
                }
                Prepended(v, next) => {
                    len += v.len() as u32;
                    cursor = next;
                }
                Set(v) => {
                    len += v.len() as u32;
                    return len;
                }
                Unset => {
                    return len;
                }
            }
        }
    }
    fn length_tables(&self) -> u32 {
        use self::CtxMember::*;

        let mut len = 0;
        let mut cursor = &self.tables;
        loop {
            match cursor {
                Delegated(next) => {
                    cursor = next;
                }
                Prepended(v, next) => {
                    len += v.len() as u32;
                    cursor = next;
                }
                Set(v) => {
                    len += v.len() as u32;
                    return len;
                }
                Unset => {
                    return len;
                }
            }
        }
    }
}

macro_rules! valid_with {
    (($ctx:ident, $name:ident: $type:ty $(,$arg:ident: $argty:ty)*) -> $rt:ty $b:block) => (
        pub fn $name($ctx: &Ctx, $name: &$type $(,$arg: $argty)*) -> VResult<$rt> {
            let ty = $b;
            Ok(ty)
        }
    )
}

#[derive(Debug, PartialEq)]
pub struct ImportExportMapping {
    pub imports: Vec<ExternType>,
    pub exports: Vec<ExternType>,
}

pub struct Valid;

pub mod validate {
    use super::*;

    valid_with!((c, limit: Limits) -> Valid {
        if let Some(max) = limit.max {
            if max < limit.min {
                c.error(LimitMaxSmallerMin)?
            }
        }

        Valid
    });

    valid_with!((c, function_type: FuncType) -> Valid {
        if function_type.results.len() > 1 {
            c.error(FunctionTypeResultArityGreaterOne)?
        }

        Valid
    });

    valid_with!((c, table_type: TableType) -> Valid {
        validate::limit(c, &table_type.limits)?;

        Valid
    });

    valid_with!((c, memory_type: MemType) -> Valid {
        validate::limit(c, &memory_type.limits)?;

        // NB: This check is requires by the testsuite and seems sensible,
        // but is not actually written in the spec at the point of writing this

        let min = memory_type.limits.min;
        let max = memory_type.limits.max.unwrap_or(min);

        if min > 65536 || max > 65536 {
            c.error(MemoryBoundsOutside32Bit)?;
        }

        Valid
    });

    valid_with!((c, global_type: GlobalType) -> Valid {
        let _ = c;
        let _ = global_type;

        Valid
    });

    // Stack based instruction validator as presented in the spec appendix:

    #[derive(Debug, PartialEq, Copy, Clone)]
    enum ValTypeOrUnknown {
        ValType(ValType),
        Unknown
    }
    use self::ValTypeOrUnknown::Unknown;

    #[derive(Debug, PartialEq, Clone)]
    struct CtrlFrame {
        label_types: Vec<ValType>,
        end_types: Vec<ValType>,
        height: u32,
        unreachable: bool,
    }

    struct Stack<T> {
        data: Vec<T>
    }

    impl<T> Stack<T> {
        fn new() -> Self { Stack { data: Vec::new() } }
        fn pop(&mut self) -> T {
            self.data.pop().unwrap()
        }
        fn push(&mut self, v: T) {
            self.data.push(v);
        }
        fn at(&self, i: u32) -> &T {
            let i = i as usize;
            let l = self.data.len();
            &self.data[l - i - 1]
        }
        fn at_mut(&mut self, i: u32) -> &mut T {
            let i = i as usize;
            let l = self.data.len();
            &mut self.data[l - i - 1]
        }
        fn size(&self) -> u32 {
            let l = self.data.len();
            // NB: As far as I know, this limit should not exist according
            // to the spec, but casting to u32 here makes the code more readable
            // at the usage sites.
            //
            // Since this only fails if there are more than u32::MAX
            // nested control instructions in a function,
            // and the amount of instructions is limited to u32::MAX,
            // it is probably impossible to hit in practice anyway.
            //
            // See also CtrlFrame::height being defined as a u32
            assert!(l <= ::std::u32::MAX as usize);
            l as u32
        }
        fn resize(&mut self, s: u32) {
            self.data.truncate(s as usize);
        }
    }

    pub struct InstrCtx {
        opds: Stack<ValTypeOrUnknown>,
        ctrls: Stack<CtrlFrame>,
    }

    impl InstrCtx {
        fn new() -> Self {
            InstrCtx{
                ctrls: Stack::new(),
                opds: Stack::new(),
            }
        }

        fn push_opd(&mut self, type_: ValTypeOrUnknown) {
            self.opds.push(type_);
        }

        fn pop_opd(&mut self) -> VResult<ValTypeOrUnknown> {
            if self.opds.size() ==  self.ctrls.at(0).height
                &&  self.ctrls.at(0).unreachable
            {
                return Ok(Unknown);
            }
            if self.opds.size() == self.ctrls.at(0).height {
                self.error()?;
            }
            return Ok(self.opds.pop());
        }

        fn pop_opd_expect(&mut self, expect: ValTypeOrUnknown) -> VResult<ValTypeOrUnknown> {
            let actual = self.pop_opd()?;
            if actual == Unknown { return Ok(expect); }
            if expect == Unknown { return Ok(actual); }
            if actual != expect {
                self.error()?;
            }
            return Ok(actual);
        }

        fn push_opds(&mut self, types: &[ValType]) {
            for t in types {
                self.push_opd(ValTypeOrUnknown::ValType(*t));
            }
        }

        fn pop_opds(&mut self, types: &[ValType]) -> VResult<Valid> {
            for t in types.iter().rev() {
                self.pop_opd_expect(ValTypeOrUnknown::ValType(*t))?;
            }
            Ok(Valid)
        }

        fn push_ctrl(&mut self, label: &[ValType], out: &[ValType]) {
            let frame = CtrlFrame {
                label_types: label.to_owned(),
                end_types: out.to_owned(),
                height: self.opds.size(),
                unreachable: false,
            };
            self.ctrls.push(frame);
        }

        fn pop_ctrl(&mut self) -> VResult<Vec<ValType>> {
            if self.ctrls.size() == 0 {
                self.error()?;
            }
            let frame = self.ctrls.at(0).clone(); // TODO: Bad clone
            self.pop_opds(&frame.end_types)?;
            if self.opds.size() != frame.height {
                self.error()?;
            }
            self.ctrls.pop();
            return Ok(frame.end_types);
        }

        fn unreachable(&mut self) {
            self.opds.resize(self.ctrls.at(0).height);
            self.ctrls.at_mut(0).unreachable = true;
        }

        fn error(&mut self) -> VResult<Valid> {
            Err(ValidationError{
                kind: InstrWithTypeNotValid
            })
        }

        fn simple_instr(&mut self, args: &[ValType], results: &[ValType]) -> VResult<Valid> {
            self.pop_opds(args)?;
            self.push_opds(results);
            Ok(Valid)
        }

        fn end_instr(&mut self) -> VResult<Valid> {
            let results = self.pop_ctrl()?;
            self.push_opds(&results);
            Ok(Valid)
        }
    }

    valid_with!((c, instruction: Instr, ic: &mut InstrCtx) -> Valid {
        use self::Instr::*;
        use self::ValType::*;

        macro_rules! ity {
            (:$ci:expr; $($arg:expr),* ; $($result:expr),*) => (
                {
                    let ic = $ci;
                    ic.simple_instr(&[$($arg),*], &[$($result),*])?;
                    Valid
                }
            );
            ($($arg:expr),* ; $($result:expr),*) => (
                ity![:ic; $($arg),* ; $($result),*]
            )
        }

        let ty = match *instruction {
            // numeric instructions
            I32Const(_) => ity![ ; I32],
            I64Const(_) => ity![ ; I64],
            F32Const(_) => ity![ ; F32],
            F64Const(_) => ity![ ; F64],

            // unop
            | I32Clz
            | I32Ctz
            | I32Popcnt
            => ity![I32 ; I32],
            | I64Clz
            | I64Ctz
            | I64Popcnt
            => ity![I64 ; I64],
            | F32Abs
            | F32Neg
            | F32Sqrt
            | F32Ceil
            | F32Floor
            | F32Trunc
            | F32Nearest
            => ity![F32 ; F32],
            | F64Abs
            | F64Neg
            | F64Sqrt
            | F64Ceil
            | F64Floor
            | F64Trunc
            | F64Nearest
            => ity![F64 ; F64],

            // binop
            | I32Add
            | I32Sub
            | I32Mul
            | I32DivU
            | I32DivS
            | I32RemU
            | I32RemS
            | I32And
            | I32Or
            | I32Xor
            | I32Shl
            | I32ShrU
            | I32ShrS
            | I32Rotl
            | I32Rotr
            => ity![I32, I32 ; I32],
            | I64Add
            | I64Sub
            | I64Mul
            | I64DivU
            | I64DivS
            | I64RemU
            | I64RemS
            | I64And
            | I64Or
            | I64Xor
            | I64Shl
            | I64ShrU
            | I64ShrS
            | I64Rotl
            | I64Rotr
            => ity![I64, I64 ; I64],
            | F32Add
            | F32Sub
            | F32Mul
            | F32Div
            | F32Min
            | F32Max
            | F32CopySign
            => ity![F32, F32 ; F32],
            | F64Add
            | F64Sub
            | F64Mul
            | F64Div
            | F64Min
            | F64Max
            | F64CopySign
            => ity![F64, F64 ; F64],

            // testop
            I32EqZ => ity![I32 ; I32],
            I64EqZ => ity![I64 ; I32],

            // relop
            | I32Eq
            | I32Ne
            | I32LtU
            | I32LtS
            | I32GtU
            | I32GtS
            | I32LeU
            | I32LeS
            | I32GeU
            | I32GeS
            => ity![I32, I32 ; I32],
            | I64Eq
            | I64Ne
            | I64LtU
            | I64LtS
            | I64GtU
            | I64GtS
            | I64LeU
            | I64LeS
            | I64GeU
            | I64GeS
            => ity![I64, I64 ; I32],
            | F32Eq
            | F32Ne
            | F32Lt
            | F32Gt
            | F32Le
            | F32Ge
            => ity![F32, F32 ; I32],
            | F64Eq
            | F64Ne
            | F64Lt
            | F64Gt
            | F64Le
            | F64Ge
            => ity![F64, F64 ; I32],

            // cvtops
            I32ReinterpretF32 => ity![F32 ; I32],
            I64ReinterpretF64 => ity![F64 ; I64],
            F32ReinterpretI32 => ity![I32 ; F32],
            F64ReinterpretI64 => ity![I64 ; F64],

            I32TruncUF32 | I32TruncSF32 => ity![F32 ; I32],
            I32TruncUF64 | I32TruncSF64 => ity![F64 ; I32],
            I64TruncUF32 | I64TruncSF32 => ity![F32 ; I64],
            I64TruncUF64 | I64TruncSF64 => ity![F64 ; I64],

            I32WrapI64    => ity![I64 ; I32],
            I64ExtendUI32 => ity![I32 ; I64],
            I64ExtendSI32 => ity![I32 ; I64],

            F32ConvertUI32 | F32ConvertSI32 => ity![I32 ; F32],
            F32ConvertUI64 | F32ConvertSI64 => ity![I64 ; F32],
            F64ConvertUI32 | F64ConvertSI32 => ity![I32 ; F64],
            F64ConvertUI64 | F64ConvertSI64 => ity![I64 ; F64],

            F32DemoteF64  => ity![F64 ; F32],
            F64PromoteF32 => ity![F32 ; F64],

            // parametric instructions
            Drop => {
                ic.pop_opd()?;
                Valid
            },
            Select => {
                ic.pop_opd_expect(ValTypeOrUnknown::ValType(I32))?;
                let t1 = ic.pop_opd()?;
                let t2 = ic.pop_opd_expect(t1)?;
                ic.push_opd(t2);

                Valid
            },

            // variable instructions
            GetLocal(x) => {
                let t = c.locals(x)?;
                ity![ ; t]
            }
            SetLocal(x) => {
                let t = c.locals(x)?;
                ity![t ; ]
            }
            TeeLocal(x) => {
                let t = c.locals(x)?;
                ity![t ; t]
            }
            GetGlobal(x) => {
                let mut_t = c.globals(x)?;
                let t = mut_t.valtype;
                ity![ ; t]
            }
            SetGlobal(x) => {
                let mut_t = c.globals(x)?;
                let t = mut_t.valtype;
                if mut_t.mutability != Mut::Var  {
                    c.error(InstrSetGlobalNotVar)?;
                }
                ity![t ; ]
            }

            // memory instructions
            ref load_store_instr @ I32Load(..) |
            ref load_store_instr @ I64Load(..) |
            ref load_store_instr @ F32Load(..) |
            ref load_store_instr @ F64Load(..) |
            ref load_store_instr @ I32Load8U(..) |
            ref load_store_instr @ I32Load8S(..) |
            ref load_store_instr @ I64Load8U(..) |
            ref load_store_instr @ I64Load8S(..) |
            ref load_store_instr @ I32Load16U(..) |
            ref load_store_instr @ I32Load16S(..) |
            ref load_store_instr @ I64Load16U(..) |
            ref load_store_instr @ I64Load16S(..) |
            ref load_store_instr @ I64Load32U(..) |
            ref load_store_instr @ I64Load32S(..) |
            ref load_store_instr @ I32Store(..) |
            ref load_store_instr @ I64Store(..) |
            ref load_store_instr @ F32Store(..) |
            ref load_store_instr @ F64Store(..) |
            ref load_store_instr @ I32Store8(..) |
            ref load_store_instr @ I64Store8(..) |
            ref load_store_instr @ I32Store16(..) |
            ref load_store_instr @ I64Store16(..) |
            ref load_store_instr @ I64Store32(..) => {
                let validate = |memarg: Memarg, bit_width: u32, e, r| {
                    c.mems(MemIdx(0))?;
                    let align = 1u32 << memarg.align;
                    if align > (bit_width / 8) {
                        c.error(e)?;
                    }
                    Ok(r)
                };
                let load = |ic: &mut InstrCtx, t: ValType, memarg: Memarg, bit_width| {
                    validate(memarg, bit_width,
                             InstrLoadOveraligned, ity![:ic; I32 ; t])
                };
                let store = |ic: &mut InstrCtx, t: ValType, memarg: Memarg, bit_width| {
                    validate(memarg, bit_width,
                             InstrStoreOveraligned, ity![:ic; I32, t ; ])
                };

                match *load_store_instr {
                    I32Load8U(memarg)       => load(ic, I32, memarg, 8)?,
                    I32Load8S(memarg)       => load(ic, I32, memarg, 8)?,
                    I32Load16U(memarg)      => load(ic, I32, memarg, 16)?,
                    I32Load16S(memarg)      => load(ic, I32, memarg, 16)?,
                    I32Load(memarg)         => load(ic, I32, memarg, 32)?,

                    I64Load8U(memarg)       => load(ic, I64, memarg, 8)?,
                    I64Load8S(memarg)       => load(ic, I64, memarg, 8)?,
                    I64Load16U(memarg)      => load(ic, I64, memarg, 16)?,
                    I64Load16S(memarg)      => load(ic, I64, memarg, 16)?,
                    I64Load32U(memarg)      => load(ic, I64, memarg, 32)?,
                    I64Load32S(memarg)      => load(ic, I64, memarg, 32)?,
                    I64Load(memarg)         => load(ic, I64, memarg, 64)?,

                    F32Load(memarg)         => load(ic, F32, memarg, 32)?,
                    F64Load(memarg)         => load(ic, F64, memarg, 64)?,

                    I32Store8(memarg)       => store(ic, I32, memarg, 8)?,
                    I32Store16(memarg)      => store(ic, I32, memarg, 16)?,
                    I32Store(memarg)        => store(ic, I32, memarg, 32)?,

                    I64Store8(memarg)       => store(ic, I64, memarg, 8)?,
                    I64Store16(memarg)      => store(ic, I64, memarg, 16)?,
                    I64Store32(memarg)      => store(ic, I64, memarg, 32)?,
                    I64Store(memarg)        => store(ic, I64, memarg, 64)?,

                    F32Store(memarg)        => store(ic, F32, memarg, 32)?,
                    F64Store(memarg)        => store(ic, F64, memarg, 64)?,

                    _ => unreachable!(),
                }
            }
            CurrentMemory => { c.mems(MemIdx(0))?; ity![    ; I32] }
            GrowMemory    => { c.mems(MemIdx(0))?; ity![I32 ; I32] }

            // control instructions
            Nop => ity![ ; ],
            Unreachable => {
                ic.unreachable();
                Valid
            },
            Block(resulttype, ref block) => {
                let c_ = c.with().prepend_label(resulttype);

                // "block" event
                let a = &resulttype;
                ic.push_ctrl(&a, &a);

                validate::instruction_sequence(&c_, block, ic)?;

                // "end" event
                ic.end_instr()?;

                Valid
            }
            Loop(resulttype, ref block) => {
                let c_ = c.with().prepend_label(None.into());

                // "loop" event
                let a = &resulttype;
                ic.push_ctrl(&[], &a);

                validate::instruction_sequence(&c_, block, ic)?;

                // "end" event
                ic.end_instr()?;

                Valid
            }
            IfElse(resulttype, ref if_block, ref else_block) => {
                let c_ = c.with().prepend_label(resulttype);

                // "if" event
                ic.pop_opd_expect(ValTypeOrUnknown::ValType(I32))?;
                let a = &resulttype;
                ic.push_ctrl(&a, &a);

                validate::instruction_sequence(&c_, if_block, ic)?;

                // "else" event
                let results = ic.pop_ctrl()?;
                ic.push_ctrl(&results, &results);

                validate::instruction_sequence(&c_, else_block, ic)?;

                // "end" event
                ic.end_instr()?;

                Valid
            }
            Br(labelidx) => {
                let resulttype = c.labels(labelidx)?;

                let n = labelidx.0;
                if !(n < ic.ctrls.size()) {
                    ic.error()?;
                }
                let tmp = ic.ctrls.at(n).label_types.to_owned(); // TODO: Bad copy
                assert_eq!(*resulttype, tmp[..]);
                ic.pop_opds(&tmp)?;
                ic.unreachable();

                Valid
            }
            BrIf(labelidx) => {
                let resulttype = c.labels(labelidx)?;

                let n = labelidx.0;
                if !(n < ic.ctrls.size()) {
                    ic.error()?;
                }
                ic.pop_opd_expect(ValTypeOrUnknown::ValType(I32))?;
                let tmp = ic.ctrls.at(n).label_types.to_owned(); // TODO: Bad copy
                assert_eq!(*resulttype, tmp[..]);
                ic.pop_opds(&tmp)?;
                ic.push_opds(&tmp);

                Valid
            }
            BrTable(ref labelindices, labelidx_n) => {
                let resulttype = c.labels(labelidx_n)?;

                let ns = labelindices;
                let m = labelidx_n.0;

                if !(m < ic.ctrls.size()) {
                    ic.error()?;
                }
                for &LabelIdx(n) in ns {
                    if !(n < ic.ctrls.size())
                    || ic.ctrls.at(n).label_types != ic.ctrls.at(m).label_types
                    {
                        ic.error()?;
                    }
                }
                ic.pop_opd_expect(ValTypeOrUnknown::ValType(I32))?;
                let tmp = ic.ctrls.at(m).label_types.to_owned(); // TODO: Bad copy
                assert_eq!(*resulttype, tmp[..]);
                ic.pop_opds(&tmp)?;
                ic.unreachable();

                Valid
            }
            Return => {
                // TODO: Possible bug ambiguity in c.return?
                // Empty result != missing result
                // See note on 3.3.5.9 in spec

                let resulttype = c.return_(LabelIdx(0))?;

                // TODO: Not sure if implemented correctly
                // Following the wording on 2.4.5., returns is equivalent
                // to br(max_label)

                let n = ic.ctrls.size() - 1;
                let tmp = ic.ctrls.at(n).label_types.to_owned(); // TODO: Bad copy
                assert_eq!(*resulttype, tmp[..]);
                ic.pop_opds(&tmp)?;
                ic.unreachable();

                Valid
            }
            Call(x) => {
                let f = c.funcs(x)?;

                ic.simple_instr(&f.args, &f.results)?;

                Valid
            }
            CallIndirect(x) => {
                let TableType {
                    limits: _,
                    elemtype,
                } = c.tables(TableIdx(0))?;
                if elemtype != ElemType::AnyFunc {
                    c.error(InstrCallIndirectElemTypeNotAnyFunc)?;
                }
                let ty = c.types(x)?;

                ic.pop_opd_expect(ValTypeOrUnknown::ValType(I32))?;
                ic.pop_opds(&ty.args)?;
                ic.push_opds(&ty.results);

                Valid
            }
        };

        ty
    });

    valid_with!((c, instruction_sequence: [Instr], ic: &mut InstrCtx) -> Valid {
        for instr_n in instruction_sequence {
            validate::instruction(&c, instr_n, ic)?;
        }
        Valid
    });

    valid_with!((c, expr: Expr, with: ResultType) -> Valid {
        let mut ic = InstrCtx::new();

        // "block" event
        let a = &with;
        ic.push_ctrl(&a, &a);

        // TODO: According to the wording of the spec, pushing
        // the with label is unneeded, but it seems weird not to do it...

        // let c_ = c.with().prepend_label(with);
        validate::instruction_sequence(&c, &expr.body, &mut ic)?;

        // "end" event
        ic.end_instr()?;

        Valid
    });

    valid_with!((c, const_expr: Expr) -> Valid {
        for instr in &const_expr.body {
            use self::Instr::*;
            match *instr {
                I32Const(_) => (),
                I64Const(_) => (),
                F32Const(_) => (),
                F64Const(_) => (),
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

        Valid
    });

    valid_with!((c, func: Func) -> FuncType {
        let Func { type_: x, locals: t, body: expr } = func;
        let ty = c.types(*x)?;

        let locals = ty.args.iter().chain(t).cloned().collect();

        assert!(ty.results.len() <= 1); // should be enforced by type validation
        let t2_opt = ty.results.get(0).cloned();
        let result: ResultType = t2_opt.into();

        let c_ = c.with()
            .set_locals(locals)
            .set_label(result)
            .set_return_(result);

        // TODO: The wording for function validation and expression
        // validation conflict in wether
        // the expr should be valid with [t2?] or t2?.
        //
        // Verify this from official sources at some point.
        validate::expr(&c_, expr, result)?;

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

    valid_with!((c, global: Global) -> GlobalType {
        let Global {
            type_,
            init: expr
        } = global;

        let t = type_.valtype;

        validate::global_type(c, &type_)?;
        validate::expr(c, &expr, t.into())?;
        validate::const_expr(c, &expr)?;

        *type_
    });

    valid_with!((c, elem: Elem) -> Valid {
        let Elem {
            table: x,
            offset: expr,
            init: y
        } = elem;

        let TableType {
            limits: _,
            elemtype
        } = c.tables(*x)?;

        if elemtype != ElemType::AnyFunc {
            c.error(ElemElemTypeNotAnyFunc)?;
        }

        validate::expr(c, expr, ValType::I32.into())?;
        validate::const_expr(c, expr)?;
        for yi in y {
            c.funcs(*yi)?;
        }

        Valid
    });

    valid_with!((c, data: Data) -> Valid {
        let Data {
            data: x,
            offset: expr,
            init: _,
        } = data;

        c.mems(*x)?;
        validate::expr(c, expr, ValType::I32.into())?;
        validate::const_expr(c, expr)?;

        Valid
    });

    valid_with!((c, start: Start) -> Valid {
        let Start {
            func: x
        } = start;

        let ty: FuncType = c.funcs(*x)?;

        if !(ty.args.is_empty() && ty.results.is_empty()) {
            c.error(StartFunNotValidWithEmptyEmpty)?;
        }

        Valid
    });

    valid_with!((c, export: Export) -> ExternType {
        match export.desc {
            ExportDesc::Func(x) => {
                let func = c.funcs(x)?;

                ExternType::Func(func)
            }
            ExportDesc::Table(x) => {
                let table = c.tables(x)?;

                ExternType::Table(table)
            }
            ExportDesc::Mem(x) => {
                let mem = c.mems(x)?;

                ExternType::Mem(mem)
            }
            ExportDesc::Global(x) => {
                let global = c.globals(x)?;

                if global.mutability != Mut::Const {
                    c.error(ExportGlobalNotConst)?;
                }

                ExternType::Global(global)
            }
        }
    });

    valid_with!((c, import: Import) -> ExternType {
        match import.desc {
            ImportDesc::Func(x) => {
                let func_type = c.types(x)?;

                ExternType::Func(func_type)
            }
            ImportDesc::Table(tabletype) => {
                validate::table_type(c, &tabletype)?;

                ExternType::Table(tabletype)
            }
            ImportDesc::Mem(memtype) => {
                validate::memory_type(c, &memtype)?;

                ExternType::Mem(memtype)
            }
            ImportDesc::Global(globaltype) => {
                validate::global_type(c, &globaltype)?;

                if globaltype.mutability != Mut::Const {
                    c.error(ImportGlobalNotConst)?;
                }

                ExternType::Global(globaltype)
            }
        }
    });

    valid_with!((empty_c, module_prepass: Module) -> () {
        let types = &module_prepass.types;
        let imports = &module_prepass.imports;
        for import in imports {
            if let ImportDesc::Func(x) = import.desc {
                if types.get(x.0 as usize).is_none() {
                    empty_c.error(ModulePrepassImportFuncTypeIdxDoesNotExist)?;
                }
            }
        }
    });

    fn import_filter_funcs<'a>(types: &'a [FuncType],
                               imports: &'a [Import])
        -> impl Iterator<Item=&'a FuncType> + 'a
    {
        imports.iter().filter_map(move |import| {
            match import.desc {
                ImportDesc::Func(x) => {
                    // TODO: Catch None case early in validation
                    types.get(x.0 as usize)
                }
                _ => None,
            }
        })
    }

    fn import_filter_tables<'a>(imports: &'a [Import])
        -> impl Iterator<Item=&'a TableType> + 'a
    {
        imports.iter().filter_map(move |import| {
            match import.desc {
                ImportDesc::Table(ref x) => {
                    Some(x)
                }
                _ => None,
            }
        })
    }

    fn import_filter_mems<'a>(imports: &'a [Import])
        -> impl Iterator<Item=&'a MemType> + 'a
    {
        imports.iter().filter_map(move |import| {
            match import.desc {
                ImportDesc::Mem(ref x) => {
                    Some(x)
                }
                _ => None,
            }
        })
    }

    fn import_filter_globals<'a>(imports: &'a [Import])
        -> impl Iterator<Item=&'a GlobalType> + 'a
    {
        imports.iter().filter_map(move |import| {
            match import.desc {
                ImportDesc::Global(ref x) => {
                    Some(x)
                }
                _ => None,
            }
        })
    }

    valid_with!((empty_c, module: Module) -> ImportExportMapping {
        let Module {
            types,
            funcs,
            tables,
            mems,
            globals,
            elem,
            data,
            start,
            imports,
            exports,
        } = module;

        validate::module_prepass(empty_c, module)?;

        let c = {
            let functypes = types;

            let funcs_it   = import_filter_funcs(functypes, imports);
            let tables_it  = import_filter_tables(imports);
            let mems_it    = import_filter_mems(imports);
            let globals_it = import_filter_globals(imports);

            // TODO: Catch None case early in validation
            let fts = funcs.iter().flat_map(|x| {
                functypes.get(x.type_.0 as usize)
            });
            let tts = tables.iter() .map(|x| &x.type_);
            let mts = mems.iter()   .map(|x| &x.type_);
            let gts = globals.iter().map(|x| &x.type_);

            let conc_funcs   = funcs_it  .chain(fts);
            let conc_tables  = tables_it .chain(tts);
            let conc_mems    = mems_it   .chain(mts);
            let conc_globals = globals_it.chain(gts);

            &empty_c.with()
            .set_types(types.clone().into())
            .set_funcs(conc_funcs.cloned().collect())
            .set_tables(conc_tables.cloned().collect())
            .set_mems(conc_mems.cloned().collect())
            .set_globals(conc_globals.cloned().collect())
        };

        let c_ = &empty_c.with()
            .set_globals(import_filter_globals(&imports).cloned().collect());

        for functypei in types {
            let Valid = validate::function_type(c, functypei)?;
        }

        for funci in funcs {
            let _fti = validate::func(c, funci)?;
        }

        for tablei in tables {
            let _tabletypei = validate::table(c, tablei)?;
        }

        for memi in mems {
            let _memtypei = validate::mem(c, memi)?;
        }

        for globali in globals {
            let _globaltypei = validate::global(c_, globali)?;
        }

        for elemi in elem {
            let Valid = validate::elem(c, elemi)?;
        }

        for datai in data {
            let Valid = validate::data(c, datai)?;
        }

        if let Some(ref start) = start {
            let Valid = validate::start(c, start)?;
        }

        let mut its = Vec::new();
        for importi in imports {
            let iti = validate::import(c, importi)?;
            its.push(iti);
        }

        let mut ets = Vec::new();
        for exporti in exports {
            let eti = validate::export(c, exporti)?;
            ets.push(eti);
        }

        if c.length_tables() > 1 {
            c.error(ModuleTablesLengthNotOne)?;
        }

        if c.length_mems() > 1 {
            c.error(ModuleMemsLengthNotOne)?;
        }

        {
            let mut export_names: Vec<_>
                = exports.iter().map(|x| &x.name[..]).collect();
            export_names.sort();
            for w in export_names.windows(2) {
                if let [a, b] = w {
                    if a == b {
                        c.error(ModuleExportDuplicateName)?;
                    }
                }
            }
        }

        ImportExportMapping {
            imports: its,
            exports: ets,
        }
    });
}

pub struct ValidatedModule {
    module: Module,
    import_export_mapping: ImportExportMapping
}
impl ::std::ops::Deref for ValidatedModule {
    type Target = Module;
    fn deref(&self) -> &Self::Target {
        &self.module
    }
}
impl ValidatedModule {
    pub fn import_export_mapping(&self) -> &ImportExportMapping {
        &self.import_export_mapping
    }
}

pub fn validate_module(module: Module) -> VResult<ValidatedModule> {
    let import_export_mapping = validate::module(&Ctx::new(), &module)?;
    Ok(ValidatedModule {
        module,
        import_export_mapping,
    })
}
