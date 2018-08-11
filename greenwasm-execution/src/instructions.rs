use std::borrow::BorrowMut;
use std::marker::PhantomData;

use structure::types::*;
use structure::modules::*;
use structure::instructions::*;

use crate::runtime_structure::*;
use crate::numerics::*;
use crate::structure_references::*;
use crate::modules::*;

// TODO: More central location
pub struct ExecCtx<Ref, Sto, Stk> {
    pub store: Sto,
    pub stack: Stk,
    _marker: PhantomData<Ref>,
}

#[derive(Debug)]
pub struct Trap;
type EResult<T> = ::std::result::Result<T, Trap>;

trait ValCast {
    fn assert_val_type(val: Val) -> Self;
    fn to_val(self) -> Val;
}
impl ValCast for I32 {
    #[inline(always)]
    fn assert_val_type(val: Val) -> Self {
        if let Val::I32(v) = val {
            v
        } else {
            panic!("Expected a value of type I32");
        }
    }
    #[inline(always)]
    fn to_val(self) -> Val { Val::I32(self) }
}
impl ValCast for I64 {
    #[inline(always)]
    fn assert_val_type(val: Val) -> Self {
        if let Val::I64(v) = val {
            v
        } else {
            panic!("Expected a value of type I64");
        }
    }
    #[inline(always)]
    fn to_val(self) -> Val { Val::I64(self) }
}
impl ValCast for F32 {
    #[inline(always)]
    fn assert_val_type(val: Val) -> Self {
        if let Val::F32(v) = val {
            v
        } else {
            panic!("Expected a value of type F32");
        }
    }
    #[inline(always)]
    fn to_val(self) -> Val { Val::F32(self) }
}
impl ValCast for F64 {
    #[inline(always)]
    fn assert_val_type(val: Val) -> Self {
        if let Val::F64(v) = val {
            v
        } else {
            panic!("Expected a value of type F64");
        }
    }
    #[inline(always)]
    fn to_val(self) -> Val { Val::F64(self) }
}
trait MemOp<T>: Sized where T: ValCast {
    const SIZE_OF: usize = ::std::mem::size_of::<Self>();

    fn from_mem(b: &[u8]) -> Self;
    fn extend(self) -> T;

    fn to_mem(b: &mut [u8], v: Self);
    fn wrap(t: T) -> Self;
}

// TODO: fix le/be mess once nightly is updated

macro_rules! mem_op {
    (a: $memty:ty, $castty:ty, $opty:ty) => {
        impl MemOp<$opty> for $memty {
            #[inline(always)]
            fn from_mem(b: &[u8]) -> Self {
                let mut arr = [0; <Self as MemOp<$opty>>::SIZE_OF];
                arr.copy_from_slice(b);
                Self::from_le(Self::from_bytes(arr))
            }
            #[inline(always)]
            fn extend(self) -> $opty {
                self as $castty as $opty
            }
            #[inline(always)]
            fn to_mem(b: &mut [u8], v: Self) {
                b.copy_from_slice(&v.to_le().to_bytes());
            }
            #[inline(always)]
            fn wrap(t: $opty) -> Self {
                t as $castty as $memty
            }
        }
    };
    (b: $memty:ty, $castty:ty, $opty:ty) => {
        impl MemOp<$opty> for $memty {
            #[inline(always)]
            fn from_mem(b: &[u8]) -> Self {
                let mut arr = [0; <Self as MemOp<$opty>>::SIZE_OF];
                arr.copy_from_slice(b);
                Self::from_le_bytes(arr)
            }
            #[inline(always)]
            fn extend(self) -> $opty {
                self as $castty as $opty
            }
            #[inline(always)]
            fn to_mem(b: &mut [u8], v: Self) {
                b.copy_from_slice(&v.to_le_bytes());
            }
            #[inline(always)]
            fn wrap(t: $opty) -> Self {
                t as $castty as $memty
            }
        }
    };
    (c: $memty:ty, $castty:ty, $opty:ty) => {
        impl MemOp<$opty> for $memty {
            #[inline(always)]
            fn from_mem(b: &[u8]) -> Self {
                let v = <$castty as MemOp<$castty>>::from_mem(b);
                <$memty>::from_bits(v)
            }
            #[inline(always)]
            fn extend(self) -> $opty {
                self
            }
            #[inline(always)]
            fn to_mem(b: &mut [u8], v: Self) {
                <$castty as MemOp<$castty>>::to_mem(b, v.to_bits());
            }
            #[inline(always)]
            fn wrap(t: $opty) -> Self {
                t
            }
        }
    }
}

mem_op!(a: I32, I32, I32);

mem_op!(a: u8,  u32, I32);
mem_op!(a: u16, u32, I32);

mem_op!(b: i8,  i32, I32);
mem_op!(b: i16, i32, I32);

mem_op!(a: I64, I64, I64);

mem_op!(a: u8,  u64, I64);
mem_op!(a: u16, u64, I64);
mem_op!(a: u32, u64, I64);

mem_op!(b: i8,  i64, I64);
mem_op!(b: i16, i64, I64);
mem_op!(b: i32, i64, I64);

mem_op!(c: F32, u32, F32);
mem_op!(c: F64, u64, F64);


impl<'instrs, Ref, Sto, Stk> ExecCtx<Ref, Sto, Stk>
    where Sto: BorrowMut<Store<Ref>>,
          Stk: BorrowMut<Stack<'instrs>>,
          Ref: StructureReference,
{
    #[inline(always)]
    pub fn new(store: Sto, stack: Stk) -> Self {
        ExecCtx {
            store,
            stack,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn store(&mut self) -> &mut Store<Ref> {
        self.store.borrow_mut()
    }

    #[inline(always)]
    pub fn stack(&mut self) -> &mut Stack {
        self.stack.borrow_mut()
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> Val {
        self.execute_instrs(&expr.body);
        let v = self.stack.borrow_mut().pop_val();
        v
    }

    #[inline(always)]
    fn constop<T: ValCast>(stack: &mut Stack, v: T) {
        stack.push_val(v.to_val());
    }

    #[inline(always)]
    fn unop<T: ValCast, F: FnOnce(T) -> T>(stack: &mut Stack, unop: F) {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = unop(c1);
        stack.push_val(c.to_val());
    }

    #[inline(always)]
    fn binop<T: ValCast, F: FnOnce(T, T) -> T>(stack: &mut Stack, binop: F) {
        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = binop(c1, c2);
        stack.push_val(c.to_val());
    }

    #[inline(always)]
    fn partial_binop<T: ValCast, F: FnOnce(T, T) -> Partial<T>>(stack: &mut Stack, binop: F) -> EResult<()> {
        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = binop(c1, c2);

        if let Partial::Val(c) = c {
            stack.push_val(c.to_val());
            Ok(())
        } else {
            Err(Trap)
        }
    }

    #[inline(always)]
    fn testop<T: ValCast, F: FnOnce(T) -> I32>(stack: &mut Stack, testop: F) {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = testop(c1);
        stack.push_val(c.to_val());
    }

    #[inline(always)]
    fn relop<T: ValCast, F: FnOnce(T, T) -> I32>(stack: &mut Stack, relop: F) {
        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = relop(c1, c2);
        stack.push_val(c.to_val());
    }

    #[inline(always)]
    fn cvtop<T: ValCast, U: ValCast, F: FnOnce(T) -> U>(stack: &mut Stack, cvtop: F) {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = cvtop(c1);
        stack.push_val(c.to_val());
    }

    #[inline(always)]
    fn partial_cvtop<T: ValCast, U: ValCast, F: FnOnce(T) -> Partial<U>>(stack: &mut Stack, cvtop: F) -> EResult<()> {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = cvtop(c1);

        if let Partial::Val(c) = c {
            stack.push_val(c.to_val());
            Ok(())
        } else {
            Err(Trap)
        }
    }

    #[inline(always)]
    fn loadop<T: ValCast, M: MemOp<T>>(stack: &mut Stack,
                                       store: &mut Store<Ref>,
                                       memarg: Memarg) -> EResult<()> {
        let a = stack.current_frame().module;
        let a = store.modules[a].memaddrs[MemIdx(0)];
        let mem = &store.mems[a];
        let val = stack.pop_val();
        let i = I32::assert_val_type(val);
        // NB: Explicitly use u64 to make calculations correct under 32 bit systems
        let ea = (i as u64) + (memarg.offset as u64);
        if (ea + M::SIZE_OF as u64) > (mem.data.len() as u64) {
            Err(Trap)?;
        }
        let ea = ea as usize;

        let bs = &mem.data[ea..(ea + M::SIZE_OF)];
        let v = M::from_mem(bs);
        let c = M::extend(v);

        stack.push_val(c.to_val());

        Ok(())
    }
    #[inline(always)]
    fn storeop<T: ValCast, M: MemOp<T>>(stack: &mut Stack,
                                        store: &mut Store<Ref>,
                                        memarg: Memarg) -> EResult<()> {
        let a = stack.current_frame().module;
        let a = store.modules[a].memaddrs[MemIdx(0)];
        let mem = &mut store.mems[a];

        let c = T::assert_val_type(stack.pop_val());
        let i = I32::assert_val_type(stack.pop_val());

        // NB: Explicitly use u64 to make calculations correct under 32 bit systems
        let ea = (i as u64) + (memarg.offset as u64);
        if (ea + M::SIZE_OF as u64) > (mem.data.len() as u64) {
            Err(Trap)?;
        }
        let ea = ea as usize;

        let bs = &mut mem.data[ea..(ea + M::SIZE_OF)];

        let n = M::wrap(c);
        M::to_mem(bs, n);

        Ok(())
    }

    pub fn enter_block<'instr>(stack: &mut Stack<'instr>,
                               current_instrs: &mut &'instr [Instr],
                               jump_target: &'instr [Instr],
                               label_n: usize,
                               label_cont: &'instr [Instr]) {
        stack.push_label(label_n, label_cont);
        *current_instrs = jump_target;
    }

    pub fn execute_instrs(&mut self, mut current_instrs: &[Instr]) -> EResult<()> {
        use self::Instr::*;
        let stack = self.stack.borrow_mut();
        let store = self.store.borrow_mut();

        let next_instr = |instrs: &mut &[Instr]| {
            if let Some(instr) = instrs.get(0) {
                *instrs = &instrs[1..];
                Some(instr)
            } else {
                None
            }
        };

        while let Some(instr) = next_instr(&mut current_instrs) {
            match *instr {
                // consts
                I32Const(v) => Self::constop(stack, v),
                I64Const(v) => Self::constop(stack, v),
                F32Const(v) => Self::constop(stack, v),
                F64Const(v) => Self::constop(stack, v),

                // unops
                I32Clz => Self::unop(stack, I32::iclz),
                I64Clz => Self::unop(stack, I64::iclz),

                I32Ctz => Self::unop(stack, I32::ictz),
                I64Ctz => Self::unop(stack, I64::ictz),

                I32Popcnt => Self::unop(stack, I32::ipopcnt),
                I64Popcnt => Self::unop(stack, I64::ipopcnt),

                F32Abs => Self::unop(stack, F32::fabs),
                F64Abs => Self::unop(stack, F64::fabs),

                F32Neg => Self::unop(stack, F32::fneg),
                F64Neg => Self::unop(stack, F64::fneg),

                F32Sqrt => Self::unop(stack, F32::fsqrt),
                F64Sqrt => Self::unop(stack, F64::fsqrt),

                F32Ceil => Self::unop(stack, F32::fceil),
                F64Ceil => Self::unop(stack, F64::fceil),

                F32Floor => Self::unop(stack, F32::ffloor),
                F64Floor => Self::unop(stack, F64::ffloor),

                F32Trunc => Self::unop(stack, F32::ftrunc),
                F64Trunc => Self::unop(stack, F64::ftrunc),

                F32Nearest => Self::unop(stack, F32::fnearest),
                F64Nearest => Self::unop(stack, F64::fnearest),

                // binops
                I32Add => Self::binop(stack, I32::iadd),
                I64Add => Self::binop(stack, I64::iadd),

                I32Sub => Self::binop(stack, I32::isub),
                I64Sub => Self::binop(stack, I64::isub),

                I32Mul => Self::binop(stack, I32::imul),
                I64Mul => Self::binop(stack, I64::imul),

                I32DivU => Self::partial_binop(stack, I32::idiv_u)?,
                I64DivU => Self::partial_binop(stack, I64::idiv_u)?,

                I32DivS => Self::partial_binop(stack, I32::idiv_s)?,
                I64DivS => Self::partial_binop(stack, I64::idiv_s)?,

                I32RemU => Self::partial_binop(stack, I32::irem_u)?,
                I64RemU => Self::partial_binop(stack, I64::irem_u)?,

                I32RemS => Self::partial_binop(stack, I32::irem_s)?,
                I64RemS => Self::partial_binop(stack, I64::irem_s)?,

                I32And => Self::binop(stack, I32::iand),
                I64And => Self::binop(stack, I64::iand),

                I32Or => Self::binop(stack, I32::ior),
                I64Or => Self::binop(stack, I64::ior),

                I32Xor => Self::binop(stack, I32::ixor),
                I64Xor => Self::binop(stack, I64::ixor),

                I32Shl => Self::binop(stack, I32::ishl),
                I64Shl => Self::binop(stack, I64::ishl),

                I32ShrU => Self::binop(stack, I32::ishr_u),
                I64ShrU => Self::binop(stack, I64::ishr_u),

                I32ShrS => Self::binop(stack, I32::ishr_s),
                I64ShrS => Self::binop(stack, I64::ishr_s),

                I32Rotl => Self::binop(stack, I32::irotl),
                I64Rotl => Self::binop(stack, I64::irotl),

                I32Rotr => Self::binop(stack, I32::irotr),
                I64Rotr => Self::binop(stack, I64::irotr),

                F32Add => Self::binop(stack, F32::fadd),
                F64Add => Self::binop(stack, F64::fadd),

                F32Sub => Self::binop(stack, F32::fsub),
                F64Sub => Self::binop(stack, F64::fsub),

                F32Mul => Self::binop(stack, F32::fmul),
                F64Mul => Self::binop(stack, F64::fmul),

                F32Div => Self::binop(stack, F32::fdiv),
                F64Div => Self::binop(stack, F64::fdiv),

                F32Min => Self::binop(stack, F32::fmin),
                F64Min => Self::binop(stack, F64::fmin),

                F32Max => Self::binop(stack, F32::fmax),
                F64Max => Self::binop(stack, F64::fmax),

                F32CopySign => Self::binop(stack, F32::fcopysign),
                F64CopySign => Self::binop(stack, F64::fcopysign),

                // testops
                I32EqZ => Self::testop(stack, I32::ieqz),
                I64EqZ => Self::testop(stack, I64::ieqz),

                // relops
                I32Eq => Self::relop(stack, I32::ieq),
                I64Eq => Self::relop(stack, I64::ieq),

                I32Ne => Self::relop(stack, I32::ine),
                I64Ne => Self::relop(stack, I64::ine),

                I32LtU => Self::relop(stack, I32::ilt_u),
                I64LtU => Self::relop(stack, I64::ilt_u),

                I32LtS => Self::relop(stack, I32::ilt_s),
                I64LtS => Self::relop(stack, I64::ilt_s),

                I32GtU => Self::relop(stack, I32::igt_u),
                I64GtU => Self::relop(stack, I64::igt_u),

                I32GtS => Self::relop(stack, I32::igt_s),
                I64GtS => Self::relop(stack, I64::igt_s),

                I32LeU => Self::relop(stack, I32::ile_u),
                I64LeU => Self::relop(stack, I64::ile_u),

                I32LeS => Self::relop(stack, I32::ile_s),
                I64LeS => Self::relop(stack, I64::ile_s),

                I32GeU => Self::relop(stack, I32::ige_u),
                I64GeU => Self::relop(stack, I64::ige_u),

                I32GeS => Self::relop(stack, I32::ige_s),
                I64GeS => Self::relop(stack, I64::ige_s),

                F32Eq => Self::relop(stack, F32::feq),
                F64Eq => Self::relop(stack, F64::feq),

                F32Ne => Self::relop(stack, F32::fne),
                F64Ne => Self::relop(stack, F64::fne),

                F32Lt => Self::relop(stack, F32::flt),
                F64Lt => Self::relop(stack, F64::flt),

                F32Gt => Self::relop(stack, F32::fgt),
                F64Gt => Self::relop(stack, F64::fgt),

                F32Le => Self::relop(stack, F32::fle),
                F64Le => Self::relop(stack, F64::fle),

                F32Ge => Self::relop(stack, F32::fge),
                F64Ge => Self::relop(stack, F64::fge),

                // cvtops
                I32WrapI64 => Self::cvtop(stack, wrap),

                I64ExtendUI32 => Self::cvtop(stack, extend_u),
                I64ExtendSI32 => Self::cvtop(stack, extend_s),

                I32TruncUF32 => Self::partial_cvtop(stack, trunc_u_f32_i32)?,
                I32TruncSF32 => Self::partial_cvtop(stack, trunc_s_f32_i32)?,

                I32TruncUF64 => Self::partial_cvtop(stack, trunc_u_f64_i32)?,
                I32TruncSF64 => Self::partial_cvtop(stack, trunc_s_f64_i32)?,

                I64TruncUF32 => Self::partial_cvtop(stack, trunc_u_f32_i64)?,
                I64TruncSF32 => Self::partial_cvtop(stack, trunc_s_f32_i64)?,

                I64TruncUF64 => Self::partial_cvtop(stack, trunc_u_f64_i64)?,
                I64TruncSF64 => Self::partial_cvtop(stack, trunc_s_f64_i64)?,

                F32DemoteF64 => Self::cvtop(stack, demote),
                F64PromoteF32 => Self::cvtop(stack, promote),

                F32ConvertUI32 => Self::cvtop(stack, convert_u_i32_f32),
                F32ConvertSI32 => Self::cvtop(stack, convert_s_i32_f32),

                F32ConvertUI64 => Self::cvtop(stack, convert_u_i64_f32),
                F32ConvertSI64 => Self::cvtop(stack, convert_s_i64_f32),

                F64ConvertUI32 => Self::cvtop(stack, convert_u_i32_f64),
                F64ConvertSI32 => Self::cvtop(stack, convert_s_i32_f64),

                F64ConvertUI64 => Self::cvtop(stack, convert_u_i64_f64),
                F64ConvertSI64 => Self::cvtop(stack, convert_s_i64_f64),

                I32ReinterpretF32 => Self::cvtop(stack, reinterpret_f32_i32),
                I64ReinterpretF64 => Self::cvtop(stack, reinterpret_f64_i64),
                F32ReinterpretI32 => Self::cvtop(stack, reinterpret_i32_f32),
                F64ReinterpretI64 => Self::cvtop(stack, reinterpret_i64_f64),

                // parametric instructions
                Drop => {
                    stack.pop_val();
                }
                Select => {
                    let c = stack.pop_val();
                    let c: I32 = ValCast::assert_val_type(c);

                    let val2 = stack.pop_val();
                    let val1 = stack.pop_val();

                    assert!(val2.ty() == val1.ty());

                    if c != 0 {
                        stack.push_val(val1);
                    } else {
                        stack.push_val(val2);
                    }
                }

                // variable instructions
                GetLocal(x) => {
                    let val = stack.current_frame().locals[x];
                    stack.push_val(val);
                }
                SetLocal(x) => {
                    let val = stack.pop_val();
                    stack.current_frame().locals[x] = val;
                }
                TeeLocal(x) => {
                    let val = stack.peek_val();
                    stack.current_frame().locals[x] = val;
                }
                GetGlobal(x) => {
                    let a = stack.current_frame().module;
                    let a = store.modules[a].globaladdrs[x];
                    let glob = &store.globals[a];
                    let val = glob.value;
                    stack.push_val(val);
                }
                SetGlobal(x) => {
                    let a = stack.current_frame().module;
                    let a = store.modules[a].globaladdrs[x];
                    let glob = &mut store.globals[a];
                    let val = stack.pop_val();
                    glob.value = val;
                }

                // memory instructions

                // loads
                I32Load8U(memarg) => Self::loadop::<I32, u8>(stack, store, memarg)?,
                I32Load8S(memarg) => Self::loadop::<I32, i8>(stack, store, memarg)?,
                I32Load16U(memarg) => Self::loadop::<I32, u16>(stack, store, memarg)?,
                I32Load16S(memarg) => Self::loadop::<I32, i16>(stack, store, memarg)?,
                I32Load(memarg) => Self::loadop::<I32, I32>(stack, store, memarg)?,

                I64Load8U(memarg) => Self::loadop::<I64, u8>(stack, store, memarg)?,
                I64Load8S(memarg) => Self::loadop::<I64, i8>(stack, store, memarg)?,
                I64Load16U(memarg) => Self::loadop::<I64, u16>(stack, store, memarg)?,
                I64Load16S(memarg) => Self::loadop::<I64, i16>(stack, store, memarg)?,
                I64Load32U(memarg) => Self::loadop::<I64, u32>(stack, store, memarg)?,
                I64Load32S(memarg) => Self::loadop::<I64, i32>(stack, store, memarg)?,
                I64Load(memarg) => Self::loadop::<I64, I64>(stack, store, memarg)?,

                F32Load(memarg) => Self::loadop::<F32, F32>(stack, store, memarg)?,
                F64Load(memarg) => Self::loadop::<F64, F64>(stack, store, memarg)?,

                // stores
                I32Store8(memarg) => Self::storeop::<I32, u8>(stack, store, memarg)?,
                I32Store16(memarg) => Self::storeop::<I32, u16>(stack, store, memarg)?,
                I32Store(memarg) => Self::storeop::<I32, I32>(stack, store, memarg)?,

                I64Store8(memarg) => Self::storeop::<I64, u8>(stack, store, memarg)?,
                I64Store16(memarg) => Self::storeop::<I64, u16>(stack, store, memarg)?,
                I64Store32(memarg) => Self::storeop::<I64, u32>(stack, store, memarg)?,
                I64Store(memarg) => Self::storeop::<I64, I64>(stack, store, memarg)?,

                F32Store(memarg) => Self::storeop::<F32, F32>(stack, store, memarg)?,
                F64Store(memarg) => Self::storeop::<F64, F64>(stack, store, memarg)?,

                // mem ctrl
                CurrentMemory => {
                    let a = stack.current_frame().module;
                    let a = store.modules[a].memaddrs[MemIdx(0)];
                    let mem = &store.mems[a];
                    let sz = mem.data.len() / WASM_PAGE_SIZE;
                    stack.push_val(Val::I32(sz as I32));
                }
                GrowMemory => {
                    let a = stack.current_frame().module;
                    let a = store.modules[a].memaddrs[MemIdx(0)];
                    let mem = &mut store.mems[a];
                    let sz = mem.data.len() / WASM_PAGE_SIZE;
                    let n = I32::assert_val_type(stack.pop_val());

                    // TODO: custom limits?
                    // TODO: What with growth that exceeds 32 bits?

                    // Either try alloc:
                    let result = allocation::grow_memory_by(mem, n as usize);
                    if result.is_ok() {
                        stack.push_val(Val::I32(sz as I32));
                    } else {
                        stack.push_val(Val::I32(-1i32 as I32));
                    }

                    // Or don't:
                    // stack.push_val(Val::I32(-1i32 as I32));
                }

                // control instructions
                Nop => {
                    /* nothing to see here, carry on */
                }
                Unreachable => {
                    Err(Trap)?;
                }
                Block(resultt, jump_target) => {
                    let n = resultt.len();
                    let cont = &current_instrs[1..];

                    Self::enter_block(stack, &mut current_instrs, &jump_target, n, cont);
                }
                Loop(resultt, jump_target) => {
                    let n = 0;
                    let cont = &current_instrs[..];

                    Self::enter_block(stack, &mut current_instrs, &jump_target, n, cont);
                }
                IfElse(resultt, jump_target_if, jump_target_else) => {
                    let c = I32::assert_val_type(stack.pop_val());
                    let n = resultt.len();
                    let cont = &current_instrs[1..];
                    if c != 0 {
                        Self::enter_block(stack, &mut current_instrs, &jump_target_if, n, cont);
                    } else {
                        Self::enter_block(stack, &mut current_instrs, &jump_target_else, n, cont);
                    }
                }
                Br(l) => {
                    assert!(stack.label_count() >= (l.0 as usize) + 1);
                    let (n, cont) = stack.lth_label(l);
                    assert!(n <= 1);
                    let mut vals = None;
                    if n == 1 {
                        vals = Some(stack.pop_val());
                    }
                    for _ in 0..(l.0 as usize + 1) {
                        while let Some(StackElem::Val(_)) = stack.top() {
                            stack.pop_val();
                        }
                        stack.pop_label();
                    }
                    if let Some(val) = vals {
                        stack.push_val(val);
                    }
                    current_instrs = cont;
                }

            }
        }

        Ok(())
    }


}
