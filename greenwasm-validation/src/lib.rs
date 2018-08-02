extern crate greenwasm_structure as structure;

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
}
use self::ValidationErrorEnum::*;

enum CtxMember<'a, T: 'a> {
    Unset,
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
    labels:  CtxMember<'a, ResultType>,
    return_: CtxMember<'a, ResultType>,
}

macro_rules! ctx_set {
    ($fn_name:ident($self:ident, $var_name:ident: $var_type:ty)) => (
        ctx_set!($fn_name($self, $var_name: $var_type) -> $var_name);
    );
    ($fn_name:ident($self:ident, $var_name:ident: $var_type:ty) -> $mapped:expr) => (
        fn $fn_name(mut $self, $var_name: $var_type) -> Self {
            if let CtxMember::Delegated(_) = $self.$var_name {
                $self.$var_name = CtxMember::Set($mapped);
            } else {
                panic!("can only overwrite Delegated()");
            }
            $self
        }
    )
}

macro_rules! ctx_idx {
    ($self:ident, $name:ident: $type:ty, $err:ident, $f:expr, $g:expr) => (
        fn $name(&$self, mut x: u32) -> VResult<$type> {
            use self::CtxMember::*;

            let len = $f;
            let get = $g;

            let mut cursor = &$self.$name;

            loop {
                match cursor {
                    Delegated(next) => {
                        cursor = next;
                    }
                    Set(v) if (x as usize) < len(v) => {
                        return Ok(get(v, x as usize));
                    }
                    Prepended(v, _) if (x as usize) < len(v) => {
                        return Ok(get(v, x as usize));
                    }
                    Prepended(v, next) if (x as usize) >= len(v) => {
                        x -= len(v) as u32;
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

    fn error(&self, error: ValidationErrorEnum) -> VResult<()> {
        Err(ValidationError {
            kind: error
        })
    }

    fn _index<T: Copy>(v: &[T], x: usize) -> T { v[x] }
    fn _index_clone<T: Clone>(v: &[T], x: usize) -> T { v[x].clone() }
    fn _unwrap<T: Copy>(v: &T, _: usize) -> T { *v }

    ctx_idx!(self, locals: ValType,     CtxLocalsIdxDoesNotExist,  <[_]>::len, Self::_index);
    ctx_idx!(self, globals: GlobalType, CtxGlobalsIdxDoesNotExist, <[_]>::len, Self::_index);
    ctx_idx!(self, mems: MemType,       CtxMemsIdxDoesNotExist,    <[_]>::len, Self::_index);
    ctx_idx!(self, funcs: FuncType,     CtxFuncsIdxDoesNotExist,   <[_]>::len, Self::_index_clone);
    ctx_idx!(self, tables: TableType,   CtxTablesIdxDoesNotExist,  <[_]>::len, Self::_index);
    ctx_idx!(self, types: FuncType,     CtxTypesIdxDoesNotExist,   <[_]>::len, Self::_index_clone);
    ctx_idx!(self, labels: ResultType,  CtxLabelsIdxDoesNotExist,   |_| 1,     Self::_unwrap);
    ctx_idx!(self, return_: ResultType, CtxReturnDoesNotExist,      |_| 1,     Self::_unwrap);

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
            self.labels = CtxMember::Prepended(label, r);
        } else {
            panic!("can only overwrite Delegated()");
        }
        self
    }

    ctx_set!(set_locals(self, locals: Vec<ValType>));
    ctx_set!(set_label(self, labels: ResultType));
    ctx_set!(set_return_(self, return_: ResultType));

    ctx_set!(set_types(self, types: Vec<FuncType>));
    ctx_set!(set_funcs(self, funcs: Vec<FuncType>));
    ctx_set!(set_tables(self, tables: Vec<TableType>));
    ctx_set!(set_mems(self, mems: Vec<MemType>));
    ctx_set!(set_globals(self, globals: Vec<GlobalType>));

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

fn any_vec_to_option(vec: &[AnyValType]) -> Option<AnyValType> {
    assert!(vec.len() <= 1);
    vec.get(0).cloned()
}

fn vec_to_option(vec: &[ValType]) -> Option<ValType> {
    assert!(vec.len() <= 1);
    vec.get(0).cloned()
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum AnyValType {
    I32,
    I64,
    F32,
    F64,
    Any,
    AnyConstr(char),
    AnySeq,
    AnySeqConstr(char),
    AnyOpt,
}
fn any() -> AnyValType {
    AnyValType::Any
}
fn any_seq() -> AnyValType {
    AnyValType::AnySeq
}
fn any_constr(t: char) -> AnyValType {
    AnyValType::AnyConstr(t)
}
fn any_seq_constr(t: char) -> AnyValType {
    AnyValType::AnySeqConstr(t)
}
fn any_opt() -> AnyValType {
    AnyValType::AnyOpt
}
impl ::std::fmt::Debug for AnyValType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::AnyValType::*;
        match *self {
            I32 => write!(f, "i32"),
            I64 => write!(f, "i64"),
            F32 => write!(f, "f32"),
            F64 => write!(f, "f64"),
            Any => write!(f, ""),
            AnySeq => write!(f, "*"),
            AnyConstr(t) => write!(f, "{}", t),
            AnySeqConstr(t) => write!(f, "{}*", t),
            AnyOpt => write!(f, "?"),
        }
    }
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
impl ::std::fmt::Debug for AnyFuncType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?} -> {:?}", self.args, self.results)
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
        println!("{:?} must be valid with {:?}", self, expected);
        unimplemented!()
    }
}

impl MustBeValidWith for AnyResultType {
    fn must_by_valid_with(&self, expected: &Self) -> VResult<()> {
        println!("{:?} must be valid with {:?}", self, expected);
        unimplemented!()
    }
}

impl<'a> Ctx<'a> {
    fn find_ty_prefix(&self, t2: &[AnyValType], t: &[AnyValType])
        -> VResult<Vec<AnyValType>>
    {
        println!("find_ty_prefix: find t0 such that {:?} == [t0*]++{:?}", t2, t);



        unimplemented!()
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

pub struct ImportExportMapping {
    pub from: Vec<ExternType>,
    pub to: Vec<ExternType>,
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
        height: usize,
        unreachable: bool,
    }

    pub struct InstrCtx {
        opds: Vec<ValTypeOrUnknown>,
        ctrls: Vec<CtrlFrame>,
    }

    impl InstrCtx {
        fn new() -> Self {
            InstrCtx{
                ctrls: unimplemented!(),
                opds: unimplemented!(),
            }
        }

        fn push_opd(&mut self, type_: ValTypeOrUnknown) {
            self.opds.push(type_);
        }

        fn pop_opd(&mut self) -> ValTypeOrUnknown {
            if self.opds.len() ==  self.ctrls.last().unwrap().height
                &&  self.ctrls.last().unwrap().unreachable
            {
                return Unknown;
            }
            if self.opds.len() == self.ctrls.last().unwrap().height {
                self.error()
            }
            return self.opds.pop().unwrap();
        }

        fn pop_opd_expect(&mut self, expect: ValTypeOrUnknown) -> ValTypeOrUnknown {
            let actual = self.pop_opd();
            if actual == Unknown { return expect; }
            if expect == Unknown { return actual; }
            if actual != expect {
                self.error()
            }
            return actual;
        }

        fn push_opds(&mut self, types: &[ValType]) {
            for t in types {
                self.push_opd(ValTypeOrUnknown::ValType(*t));
            }
        }

        fn pop_opds(&mut self, types: &[ValType]) {
            for t in types.iter().rev() {
                self.pop_opd_expect(ValTypeOrUnknown::ValType(*t));
            }
        }

        fn push_ctrl(&mut self, label: &[ValType], out: &[ValType]) {
            let frame = CtrlFrame {
                label_types: label.to_owned(),
                end_types: out.to_owned(),
                height: self.opds.len(),
                unreachable: false,
            };
            self.ctrls.push(frame);
        }

        fn pop_ctrl(&mut self) -> Vec<ValType> {
            if self.ctrls.is_empty() {
                self.error();
            }
            let frame = self.ctrls.last().unwrap().clone(); // TODO: Bad clone
            self.pop_opds(&frame.end_types);
            if self.opds.len() != frame.height {
                self.error();
            }
            self.ctrls.pop();
            return frame.end_types;
        }

        fn unreachable(&mut self) {
            self.opds.resize(self.ctrls.last().unwrap().height, Unknown);
            self.ctrls.last_mut().unwrap().unreachable = true;
        }

        fn error(&mut self) {
            unimplemented!()
        }

        fn simple_instr(&mut self, args: &[ValType], results: &[ValType]) {
            self.pop_opds(args);
            self.push_opds(results);
        }
    }

    valid_with!((c, instruction: Instr, ic: &mut InstrCtx) -> AnyFuncType {
        use self::Instr::*;
        use self::ValType::*;

        let ty = match *instruction {
            // numeric instructions
            I32Const(_) => ty![ ; I32],
            I64Const(_) => ty![ ; I64],
            F32Const(_) => ty![ ; F32],
            F64Const(_) => ty![ ; F64],

            // unop
            | I32Clz
            | I32Ctz
            | I32Popcnt
            => ty![I32 ; I32],
            | I64Clz
            | I64Ctz
            | I64Popcnt
            => ty![I64 ; I64],
            | F32Abs
            | F32Neg
            | F32Sqrt
            | F32Ceil
            | F32Floor
            | F32Trunc
            | F32Nearest
            => ty![F32 ; F32],
            | F64Abs
            | F64Neg
            | F64Sqrt
            | F64Ceil
            | F64Floor
            | F64Trunc
            | F64Nearest
            => ty![F64 ; F64],

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
            => ty![I32, I32 ; I32],
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
            => ty![I64, I64 ; I64],
            | F32Add
            | F32Sub
            | F32Mul
            | F32Div
            | F32Min
            | F32Max
            | F32CopySign
            => ty![F32, F32 ; F32],
            | F64Add
            | F64Sub
            | F64Mul
            | F64Div
            | F64Min
            | F64Max
            | F64CopySign
            => ty![F64, F64 ; F64],

            // testop
            I32EqZ => ty![I32 ; I32],
            I64EqZ => ty![I64 ; I32],

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
            => ty![I32, I32 ; I32],
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
            => ty![I64, I64 ; I32],
            | F32Eq
            | F32Ne
            | F32Lt
            | F32Gt
            | F32Le
            | F32Ge
            => ty![F32, F32 ; I32],
            | F64Eq
            | F64Ne
            | F64Lt
            | F64Gt
            | F64Le
            | F64Ge
            => ty![F64, F64 ; I32],

            // cvtops
            I32ReinterpretF32 => ty![F32 ; I32],
            I64ReinterpretF64 => ty![F64 ; I64],
            F32ReinterpretI32 => ty![I32 ; F32],
            F64ReinterpretI64 => ty![I64 ; F64],

            I32TruncUF32 | I32TruncSF32 => ty![F32 ; I32],
            I32TruncUF64 | I32TruncSF64 => ty![F64 ; I32],
            I64TruncUF32 | I64TruncSF32 => ty![F32 ; I64],
            I64TruncUF64 | I64TruncSF64 => ty![F64 ; I64],

            I32WrapI64    => ty![I64 ; I32],
            I64ExtendUI32 => ty![I32 ; I64],
            I64ExtendSI32 => ty![I32 ; I64],

            F32ConvertUI32 | F32ConvertSI32 => ty![I32 ; F32],
            F32ConvertUI64 | F32ConvertSI64 => ty![I64 ; F32],
            F64ConvertUI32 | F64ConvertSI32 => ty![I32 ; F64],
            F64ConvertUI64 | F64ConvertSI64 => ty![I64 ; F64],

            F32DemoteF64  => ty![F64 ; F32],
            F64PromoteF32 => ty![F32 ; F64],

            // parametric instructions
            Drop   => ty![any() ; ],
            Select => ty![any_constr('b'), any_constr('b'), I32 ; any_constr('b')],

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
                    c.mems(0)?;
                    let align = 1u32 << memarg.align;
                    if align > (bit_width / 8) {
                        c.error(e)?;
                    }
                    Ok(r)
                };
                let load = |t: ValType, memarg: Memarg, bit_width| {
                    validate(memarg, bit_width,
                             InstrLoadOveraligned, ty![I32 ; t])
                };
                let store = |t: ValType, memarg: Memarg, bit_width| {
                    validate(memarg, bit_width,
                             InstrStoreOveraligned, ty![I32, t ; ])
                };

                match *load_store_instr {
                    I32Load8U(memarg)       => load(I32, memarg, 8)?,
                    I32Load8S(memarg)       => load(I32, memarg, 8)?,
                    I32Load16U(memarg)      => load(I32, memarg, 16)?,
                    I32Load16S(memarg)      => load(I32, memarg, 16)?,
                    I32Load(memarg)         => load(I32, memarg, 32)?,

                    I64Load8U(memarg)       => load(I64, memarg, 8)?,
                    I64Load8S(memarg)       => load(I64, memarg, 8)?,
                    I64Load16U(memarg)      => load(I64, memarg, 16)?,
                    I64Load16S(memarg)      => load(I64, memarg, 16)?,
                    I64Load32U(memarg)      => load(I64, memarg, 32)?,
                    I64Load32S(memarg)      => load(I64, memarg, 32)?,
                    I64Load(memarg)         => load(I64, memarg, 64)?,

                    F32Load(memarg)         => load(F32, memarg, 32)?,
                    F64Load(memarg)         => load(F64, memarg, 64)?,

                    I32Store8(memarg)       => store(I32, memarg, 8)?,
                    I32Store16(memarg)      => store(I32, memarg, 16)?,
                    I32Store(memarg)        => store(I32, memarg, 32)?,

                    I64Store8(memarg)       => store(I64, memarg, 8)?,
                    I64Store16(memarg)      => store(I64, memarg, 16)?,
                    I64Store32(memarg)      => store(I64, memarg, 32)?,
                    I64Store(memarg)        => store(I64, memarg, 64)?,

                    F32Store(memarg)        => store(F32, memarg, 32)?,
                    F64Store(memarg)        => store(F64, memarg, 64)?,

                    _ => unreachable!(),
                }
            }
            CurrentMemory => { c.mems(0)?; ty![    ; I32] }
            GrowMemory    => { c.mems(0)?; ty![I32 ; I32] }

            // control instructions
            Nop => ty![ ; ],
            Unreachable => ty![any_seq() ; any_seq()],
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
                ty![any_seq(), resulttype ; any_seq()]
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
                ty![any_seq(), resulttype, I32 ; any_seq()]
            }
            Return => {
                let resulttype = c.return_(0)?;
                ty![any_seq(), resulttype ; any_seq()]
            }
            Call(x) => {
                c.funcs(x)?.into()
            }
            CallIndirect(x) => {
                let TableType {
                    limits: _,
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
        let mut ic = InstrCtx::new();
        let mut instrs_ty = ty![any_seq_constr('k') ; any_seq_constr('k')];

        for instr_n in instruction_sequence {
            let AnyFuncType {
                args:    t1,
                results: t2,
            } = instrs_ty;
            let AnyFuncType {
                args:    t,
                results: t3,
            } = validate::instruction(&c, instr_n, &mut ic)?;
            let t0 = c.find_ty_prefix(&t2, &t)?;
            instrs_ty = ty![t1 ; t0, t3];
        }

        instrs_ty
    });

    valid_with!((c, expr: Expr) -> AnyResultType {
        let instrs_ty = validate::instruction_sequence(&c, &expr.body)?;
        instrs_ty.must_by_valid_with(&ty![ ; any_opt()])?;
        any_vec_to_option(&instrs_ty.results)
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
        let result = vec_to_option(&ty.results);

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

    valid_with!((c, global: Global) -> GlobalType {
        let Global {
            type_,
            init: expr
        } = global;

        let t = type_.valtype;

        validate::global_type(c, &type_)?;
        validate::expr(c, &expr)?.must_by_valid_with(&Some(t.into()))?;
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

        validate::expr(c, expr)?.must_by_valid_with(&Some(AnyValType::I32))?;
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
        validate::expr(c, expr)?.must_by_valid_with(&Some(AnyValType::I32))?;
        validate::const_expr(c, expr)?;

        Valid
    });

    valid_with!((c, start: Start) -> Valid {
        let Start {
            func: x
        } = start;

        let ty: AnyFuncType = c.funcs(*x)?.into();

        ty.must_by_valid_with(&ty![ ; ])?;

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
                if types.get(x as usize).is_none() {
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
                    types.get(x as usize)
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

            let import_funcs   = import_filter_funcs(&functypes, &imports);
            let import_tables  = import_filter_tables(&imports);
            let import_mems    = import_filter_mems(&imports);
            let import_globals = import_filter_globals(&imports);

            let conc_funcs   = import_funcs  .chain(functypes);
            let conc_tables  = import_tables .chain(tables.iter() .map(|x| &x.type_));
            let conc_mems    = import_mems   .chain(mems.iter()   .map(|x| &x.type_));
            let conc_globals = import_globals.chain(globals.iter().map(|x| &x.type_));

            &empty_c.with()
            .set_types(types.clone())
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

        let mut externtype = Vec::new();
        for importi in imports {
            let externtypei = validate::import(c, importi)?;
            externtype.push(externtypei);
        }

        let mut externtype_ = Vec::new();
        for exporti in exports {
            let externtypei_ = validate::export(c, exporti)?;
            externtype_.push(externtypei_);
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
                unreachable!()
            }
        }

        ImportExportMapping {
            from: externtype,
            to: externtype_,
        }
    });
}
