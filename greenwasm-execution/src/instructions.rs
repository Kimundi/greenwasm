use structure::types::*;
use structure::modules::*;
use structure::instructions::*;

use crate::runtime_structure::*;
use crate::numerics::*;
use crate::modules::*;

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
    (int: $memty:ty, $castty:ty, $opty:ty) => {
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
    (float: $memty:ty, $castty:ty, $opty:ty) => {
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

mem_op!(int: I32, I32, I32);

mem_op!(int: u8,  u32, I32);
mem_op!(int: u16, u32, I32);

mem_op!(int: i8,  i32, I32);
mem_op!(int: i16, i32, I32);

mem_op!(int: I64, I64, I64);

mem_op!(int: u8,  u64, I64);
mem_op!(int: u16, u64, I64);
mem_op!(int: u32, u64, I64);

mem_op!(int: i8,  i64, I64);
mem_op!(int: i16, i64, I64);
mem_op!(int: i32, i64, I64);

mem_op!(float: F32, u32, F32);
mem_op!(float: F64, u64, F64);

// TODO: More central location
pub struct ExecCtx<'instr, 'ctx>
    where 'instr: 'ctx,
{
    pub store: &'ctx mut Store<'instr>,
    pub stack: &'ctx mut Stack<'instr>,
    ip: &'instr [Instr],
}

macro_rules! instrumented_instrs {
    (match $x:expr; $($p:pat => $e:expr),*) => {
        match $x {
            $($p => {
                if crate::DEBUG_EXECUTION { println!(stringify!($p)); }

                $e
            })*
        }
    }
}

/// This just exists to enfore a fetch operation after each instruction
#[must_use]
struct JumpWitness;

impl<'instr, 'ctx> ExecCtx<'instr, 'ctx>
    where 'instr: 'ctx,
{
    #[inline(always)]
    pub fn new(store: &'ctx mut Store<'instr>,
               stack: &'ctx mut Stack<'instr>) -> Self {
        ExecCtx {
            store,
            stack,
            ip: &[],
        }
    }

    pub fn evaluate_expr(&mut self, expr: &'instr Expr) -> EResult<Val> {
        if crate::DEBUG_EXECUTION { println!("eval expr..."); }

        let v = self.stack_cleaner(|s| {
            s.ip = &expr.body;
            s.execute_instrs_no_falloff()?;
            Ok(s.stack.pop_val())
        })?;

        if crate::DEBUG_EXECUTION { println!("eval expr DONE"); }
        Ok(v)
    }

    pub fn invoke(&mut self, a: FuncAddr) -> EResult<()> {
        if crate::DEBUG_EXECUTION { println!("invoke func..."); }

        self.stack_cleaner(|s| {
            // we need a valid next instruction for the return
            s.ip = &[Instr::Nop];

            let _: JumpWitness = s.invokeop(a)?;
            s.execute_instrs()?;

            Ok(())
        })?;

        if crate::DEBUG_EXECUTION { println!("invoke func DONE"); }
        Ok(())
    }

    // -------------------------------------------------------------------------

    fn stack_cleaner<T, F: FnOnce(&mut Self) -> EResult<T>>(&mut self, f: F) -> EResult<T> {
        let snapshot = self.stack.snapshot();
        match f(self) {
            Ok(x) => Ok(x),
            Err(x) => {
                self.stack.clear_snapshot(snapshot);
                Err(x)
            }
        }
    }

    #[inline(always)]
    fn constop<T: ValCast>(&mut self, v: T) -> JumpWitness {
        let stack = &mut *self.stack;

        stack.push_val(v.to_val());
        self.jump_next()
    }

    #[inline(always)]
    fn unop<T: ValCast, F: FnOnce(T) -> T>(&mut self, unop: F) -> JumpWitness {
        let stack = &mut *self.stack;

        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = unop(c1);
        stack.push_val(c.to_val());
        self.jump_next()
    }

    #[inline(always)]
    fn binop<T: ValCast, F: FnOnce(T, T) -> T>(&mut self, binop: F) -> JumpWitness {
        let stack = &mut *self.stack;

        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = binop(c1, c2);
        stack.push_val(c.to_val());
        self.jump_next()
    }

    #[inline(always)]
    fn partial_binop<T: ValCast, F: FnOnce(T, T) -> Partial<T>>(&mut self, binop: F) -> EResult<JumpWitness> {
        let stack = &mut *self.stack;

        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = binop(c1, c2);

        if let Partial::Val(c) = c {
            stack.push_val(c.to_val());
            Ok(self.jump_next())
        } else {
            Err(Trap)
        }
    }

    #[inline(always)]
    fn testop<T: ValCast, F: FnOnce(T) -> I32>(&mut self, testop: F) -> JumpWitness {
        let stack = &mut *self.stack;

        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = testop(c1);
        stack.push_val(c.to_val());
        self.jump_next()
    }

    #[inline(always)]
    fn relop<T: ValCast, F: FnOnce(T, T) -> I32>(&mut self, relop: F) -> JumpWitness {
        let stack = &mut *self.stack;

        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = relop(c1, c2);
        stack.push_val(c.to_val());
        self.jump_next()
    }

    #[inline(always)]
    fn cvtop<T: ValCast, U: ValCast, F: FnOnce(T) -> U>(&mut self, cvtop: F) -> JumpWitness {
        let stack = &mut *self.stack;

        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = cvtop(c1);
        stack.push_val(c.to_val());
        self.jump_next()
    }

    #[inline(always)]
    fn partial_cvtop<T: ValCast, U: ValCast, F: FnOnce(T) -> Partial<U>>(&mut self, cvtop: F) -> EResult<JumpWitness> {
        let stack = &mut *self.stack;

        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = cvtop(c1);

        if let Partial::Val(c) = c {
            stack.push_val(c.to_val());
            Ok(self.jump_next())
        } else {
            Err(Trap)
        }
    }

    #[inline(always)]
    fn loadop<T: ValCast, M: MemOp<T>>(&mut self, memarg: Memarg) -> EResult<JumpWitness> {
        let stack = &mut *self.stack;
        let store = &mut *self.store;

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

        Ok(self.jump_next())
    }
    #[inline(always)]
    fn storeop<T: ValCast, M: MemOp<T>>(&mut self, memarg: Memarg) -> EResult<JumpWitness> {
        let stack = &mut *self.stack;
        let store = &mut *self.store;

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

        Ok(self.jump_next())
    }

    #[inline(always)]
    fn enter_block(&mut self,
                   jump_target: &'instr [Instr],
                   label_n: usize,
                   label_cont: &'instr [Instr]) -> JumpWitness {
        let stack = &mut *self.stack;

        stack.push_label(label_n, label_cont, &self.ip[1..]);
        self.jump(jump_target)
    }

    #[inline(always)]
    fn brop(&mut self, l: LabelIdx) -> JumpWitness
    {
        let stack = &mut *self.stack;

        assert!(stack.label_count() >= (l.0 as usize) + 1);
        let Label { n, branch_target, .. } = *stack.lth_label(l);
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
        self.jump(branch_target)
    }

    fn invokeop(&mut self, a: FuncAddr) -> EResult<JumpWitness>
    {
        let stack = &mut *self.stack;

        let f = &self.store.funcs[a];
        Ok(match f {
            FuncInst::Internal { type_, module, code } => {
                let n = type_.args.len();
                let m = type_.results.len();

                assert!(m <= 1);

                let ts = &code.locals;
                let instrs = &*code.body.body;

                let mut local_vals = vec![];
                for _ in 0..n {
                    local_vals.push(stack.pop_val());
                }
                local_vals.reverse();

                for ty in ts {
                    local_vals.push(match ty {
                        ValType::I32 => Val::I32(0),
                        ValType::I64 => Val::I64(0),
                        ValType::F32 => Val::F32(0.0),
                        ValType::F64 => Val::F64(0.0),
                    });
                }
                let frame = Frame { module: *module, locals: local_vals.into() };
                stack.push_frame(m, frame, &self.ip[1..]);

                // NB: Next instructions are stored in the frame below the label
                stack.push_label(m, &[], &[]);

                self.jump(instrs)
            }
            FuncInst::Host { .. } => {
                println!("Host behavior not implemented yet");
                unimplemented!()
            }
        })
    }

    fn jump_next(&mut self) -> JumpWitness {
        self.ip = &self.ip[1..];
        JumpWitness
    }

    fn jump(&mut self, new_ip: &'instr [Instr]) -> JumpWitness {
        self.ip = new_ip;
        JumpWitness
    }

    fn next_instr(&mut self) -> Option<&'instr Instr> {
        if let Some(instr) = self.ip.get(0) {
            Some(instr)
        } else {
            None
        }
    }
    fn execute_instrs(&mut self) -> EResult<()> {
        loop {
            self.execute_instrs_no_falloff()?;
            if crate::DEBUG_EXECUTION { println!("fell off instruction stream"); }
            let _: JumpWitness = match self.stack.top_ctrl_entry() {
                TopCtrlEntry::Label => {
                    if crate::DEBUG_EXECUTION { println!("continue after ctrl instr"); }

                    let stack = &mut *self.stack;

                    // pop m vals from top
                    let mut vals = vec![];
                    while let Some(StackElem::Val(_)) = stack.top() {
                        vals.push(stack.pop_val());
                    }
                    let next_instr = stack.pop_label().next_instr;
                    while let Some(val) = vals.pop() {
                        stack.push_val(val);
                    }

                    self.jump(next_instr)
                }
                TopCtrlEntry::Activation => {
                    if crate::DEBUG_EXECUTION { println!("continue after call"); }

                    let stack = &mut *self.stack;

                    let n = stack.current_frame_arity();
                    assert!(n <= 1);
                    let mut vals = None;
                    if n == 1 {
                        vals = Some(stack.pop_val());
                    }
                    let frame = stack.pop_frame();
                    if let Some(val) = vals {
                        stack.push_val(val);
                    }

                    self.jump(frame.next_instr)
                }
                TopCtrlEntry::None => {
                    return Ok(())
                }
            };
        }
    }

    #[inline]
    fn execute_instrs_no_falloff(&mut self) -> EResult<()> {
        use self::Instr::*;

        while let Some(instr) = self.next_instr() {
            if crate::DEBUG_EXECUTION {
                println!("exec instr {:?} - {:?}", instr, self.ip);
            }

            let _: JumpWitness = instrumented_instrs! { match *instr;
                // consts
                I32Const(v) => self.constop(v),
                I64Const(v) => self.constop(v),
                F32Const(v) => self.constop(v),
                F64Const(v) => self.constop(v),

                // unops
                I32Clz => self.unop(I32::iclz),
                I64Clz => self.unop(I64::iclz),

                I32Ctz => self.unop(I32::ictz),
                I64Ctz => self.unop(I64::ictz),

                I32Popcnt => self.unop(I32::ipopcnt),
                I64Popcnt => self.unop(I64::ipopcnt),

                F32Abs => self.unop(F32::fabs),
                F64Abs => self.unop(F64::fabs),

                F32Neg => self.unop(F32::fneg),
                F64Neg => self.unop(F64::fneg),

                F32Sqrt => self.unop(F32::fsqrt),
                F64Sqrt => self.unop(F64::fsqrt),

                F32Ceil => self.unop(F32::fceil),
                F64Ceil => self.unop(F64::fceil),

                F32Floor => self.unop(F32::ffloor),
                F64Floor => self.unop(F64::ffloor),

                F32Trunc => self.unop(F32::ftrunc),
                F64Trunc => self.unop(F64::ftrunc),

                F32Nearest => self.unop(F32::fnearest),
                F64Nearest => self.unop(F64::fnearest),

                // binops
                I32Add => self.binop(I32::iadd),
                I64Add => self.binop(I64::iadd),

                I32Sub => self.binop(I32::isub),
                I64Sub => self.binop(I64::isub),

                I32Mul => self.binop(I32::imul),
                I64Mul => self.binop(I64::imul),

                I32DivU => self.partial_binop(I32::idiv_u)?,
                I64DivU => self.partial_binop(I64::idiv_u)?,

                I32DivS => self.partial_binop(I32::idiv_s)?,
                I64DivS => self.partial_binop(I64::idiv_s)?,

                I32RemU => self.partial_binop(I32::irem_u)?,
                I64RemU => self.partial_binop(I64::irem_u)?,

                I32RemS => self.partial_binop(I32::irem_s)?,
                I64RemS => self.partial_binop(I64::irem_s)?,

                I32And => self.binop(I32::iand),
                I64And => self.binop(I64::iand),

                I32Or => self.binop(I32::ior),
                I64Or => self.binop(I64::ior),

                I32Xor => self.binop(I32::ixor),
                I64Xor => self.binop(I64::ixor),

                I32Shl => self.binop(I32::ishl),
                I64Shl => self.binop(I64::ishl),

                I32ShrU => self.binop(I32::ishr_u),
                I64ShrU => self.binop(I64::ishr_u),

                I32ShrS => self.binop(I32::ishr_s),
                I64ShrS => self.binop(I64::ishr_s),

                I32Rotl => self.binop(I32::irotl),
                I64Rotl => self.binop(I64::irotl),

                I32Rotr => self.binop(I32::irotr),
                I64Rotr => self.binop(I64::irotr),

                F32Add => self.binop(F32::fadd),
                F64Add => self.binop(F64::fadd),

                F32Sub => self.binop(F32::fsub),
                F64Sub => self.binop(F64::fsub),

                F32Mul => self.binop(F32::fmul),
                F64Mul => self.binop(F64::fmul),

                F32Div => self.binop(F32::fdiv),
                F64Div => self.binop(F64::fdiv),

                F32Min => self.binop(F32::fmin),
                F64Min => self.binop(F64::fmin),

                F32Max => self.binop(F32::fmax),
                F64Max => self.binop(F64::fmax),

                F32CopySign => self.binop(F32::fcopysign),
                F64CopySign => self.binop(F64::fcopysign),

                // testops
                I32EqZ => self.testop(I32::ieqz),
                I64EqZ => self.testop(I64::ieqz),

                // relops
                I32Eq => self.relop(I32::ieq),
                I64Eq => self.relop(I64::ieq),

                I32Ne => self.relop(I32::ine),
                I64Ne => self.relop(I64::ine),

                I32LtU => self.relop(I32::ilt_u),
                I64LtU => self.relop(I64::ilt_u),

                I32LtS => self.relop(I32::ilt_s),
                I64LtS => self.relop(I64::ilt_s),

                I32GtU => self.relop(I32::igt_u),
                I64GtU => self.relop(I64::igt_u),

                I32GtS => self.relop(I32::igt_s),
                I64GtS => self.relop(I64::igt_s),

                I32LeU => self.relop(I32::ile_u),
                I64LeU => self.relop(I64::ile_u),

                I32LeS => self.relop(I32::ile_s),
                I64LeS => self.relop(I64::ile_s),

                I32GeU => self.relop(I32::ige_u),
                I64GeU => self.relop(I64::ige_u),

                I32GeS => self.relop(I32::ige_s),
                I64GeS => self.relop(I64::ige_s),

                F32Eq => self.relop(F32::feq),
                F64Eq => self.relop(F64::feq),

                F32Ne => self.relop(F32::fne),
                F64Ne => self.relop(F64::fne),

                F32Lt => self.relop(F32::flt),
                F64Lt => self.relop(F64::flt),

                F32Gt => self.relop(F32::fgt),
                F64Gt => self.relop(F64::fgt),

                F32Le => self.relop(F32::fle),
                F64Le => self.relop(F64::fle),

                F32Ge => self.relop(F32::fge),
                F64Ge => self.relop(F64::fge),

                // cvtops
                I32WrapI64 => self.cvtop(wrap),

                I64ExtendUI32 => self.cvtop(extend_u),
                I64ExtendSI32 => self.cvtop(extend_s),

                I32TruncUF32 => self.partial_cvtop(trunc_u_f32_i32)?,
                I32TruncSF32 => self.partial_cvtop(trunc_s_f32_i32)?,

                I32TruncUF64 => self.partial_cvtop(trunc_u_f64_i32)?,
                I32TruncSF64 => self.partial_cvtop(trunc_s_f64_i32)?,

                I64TruncUF32 => self.partial_cvtop(trunc_u_f32_i64)?,
                I64TruncSF32 => self.partial_cvtop(trunc_s_f32_i64)?,

                I64TruncUF64 => self.partial_cvtop(trunc_u_f64_i64)?,
                I64TruncSF64 => self.partial_cvtop(trunc_s_f64_i64)?,

                F32DemoteF64 => self.cvtop(demote),
                F64PromoteF32 => self.cvtop(promote),

                F32ConvertUI32 => self.cvtop(convert_u_i32_f32),
                F32ConvertSI32 => self.cvtop(convert_s_i32_f32),

                F32ConvertUI64 => self.cvtop(convert_u_i64_f32),
                F32ConvertSI64 => self.cvtop(convert_s_i64_f32),

                F64ConvertUI32 => self.cvtop(convert_u_i32_f64),
                F64ConvertSI32 => self.cvtop(convert_s_i32_f64),

                F64ConvertUI64 => self.cvtop(convert_u_i64_f64),
                F64ConvertSI64 => self.cvtop(convert_s_i64_f64),

                I32ReinterpretF32 => self.cvtop(reinterpret_f32_i32),
                I64ReinterpretF64 => self.cvtop(reinterpret_f64_i64),
                F32ReinterpretI32 => self.cvtop(reinterpret_i32_f32),
                F64ReinterpretI64 => self.cvtop(reinterpret_i64_f64),

                // parametric instructions
                Drop => {
                    let stack = &mut *self.stack;

                    stack.pop_val();
                    self.jump_next()
                },
                Select => {
                    let stack = &mut *self.stack;

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
                    self.jump_next()
                },

                // variable instructions
                GetLocal(x) => {
                    let stack = &mut *self.stack;

                    let val = stack.current_frame().locals[x];
                    stack.push_val(val);
                    self.jump_next()
                },
                SetLocal(x) => {
                    let stack = &mut *self.stack;

                    let val = stack.pop_val();
                    stack.current_frame().locals[x] = val;
                    self.jump_next()
                },
                TeeLocal(x) => {
                    let stack = &mut *self.stack;

                    let val = stack.peek_val();
                    stack.current_frame().locals[x] = val;
                    self.jump_next()
                },
                GetGlobal(x) => {
                    let stack = &mut *self.stack;
                    let store = &mut *self.store;

                    let a = stack.current_frame().module;
                    let a = store.modules[a].globaladdrs[x];
                    let glob = &store.globals[a];
                    let val = glob.value;
                    stack.push_val(val);
                    self.jump_next()
                },
                SetGlobal(x) => {
                    let stack = &mut *self.stack;
                    let store = &mut *self.store;

                    let a = stack.current_frame().module;
                    let a = store.modules[a].globaladdrs[x];
                    let glob = &mut store.globals[a];
                    let val = stack.pop_val();
                    glob.value = val;
                    self.jump_next()
                },

                // memory instructions

                // loads
                I32Load8U(memarg) => self.loadop::<I32, u8>(memarg)?,
                I32Load8S(memarg) => self.loadop::<I32, i8>(memarg)?,
                I32Load16U(memarg) => self.loadop::<I32, u16>(memarg)?,
                I32Load16S(memarg) => self.loadop::<I32, i16>(memarg)?,
                I32Load(memarg) => self.loadop::<I32, I32>(memarg)?,

                I64Load8U(memarg) => self.loadop::<I64, u8>(memarg)?,
                I64Load8S(memarg) => self.loadop::<I64, i8>(memarg)?,
                I64Load16U(memarg) => self.loadop::<I64, u16>(memarg)?,
                I64Load16S(memarg) => self.loadop::<I64, i16>(memarg)?,
                I64Load32U(memarg) => self.loadop::<I64, u32>(memarg)?,
                I64Load32S(memarg) => self.loadop::<I64, i32>(memarg)?,
                I64Load(memarg) => self.loadop::<I64, I64>(memarg)?,

                F32Load(memarg) => self.loadop::<F32, F32>(memarg)?,
                F64Load(memarg) => self.loadop::<F64, F64>(memarg)?,

                // stores
                I32Store8(memarg) => self.storeop::<I32, u8>(memarg)?,
                I32Store16(memarg) => self.storeop::<I32, u16>(memarg)?,
                I32Store(memarg) => self.storeop::<I32, I32>(memarg)?,

                I64Store8(memarg) => self.storeop::<I64, u8>(memarg)?,
                I64Store16(memarg) => self.storeop::<I64, u16>(memarg)?,
                I64Store32(memarg) => self.storeop::<I64, u32>(memarg)?,
                I64Store(memarg) => self.storeop::<I64, I64>(memarg)?,

                F32Store(memarg) => self.storeop::<F32, F32>(memarg)?,
                F64Store(memarg) => self.storeop::<F64, F64>(memarg)?,

                // mem ctrl
                CurrentMemory => {
                    let stack = &mut *self.stack;
                    let store = &mut *self.store;

                    let a = stack.current_frame().module;
                    let a = store.modules[a].memaddrs[MemIdx(0)];
                    let mem = &store.mems[a];
                    let sz = mem.data.len() / WASM_PAGE_SIZE;
                    stack.push_val(Val::I32(sz as I32));
                    self.jump_next()
                },
                GrowMemory => {
                    let stack = &mut *self.stack;
                    let store = &mut *self.store;

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

                    self.jump_next()
                },

                // control instructions
                Nop => {
                    /* nothing to see here, carry on */
                    self.jump_next()
                },
                Unreachable => {
                    Err(Trap)?
                },
                Block(resultt, ref jump_target) => {
                    let n = resultt.len();
                    let cont = &self.ip[1..];

                    self.enter_block(&jump_target, n, cont)
                },
                Loop(_, ref jump_target) => {
                    // TODO: Is it correct that result type is ignored here?
                    let n = 0;
                    let cont = self.ip;

                    self.enter_block(&jump_target, n, cont)
                },
                IfElse(resultt, ref jump_target_if, ref jump_target_else) => {
                    let stack = &mut *self.stack;

                    let c = I32::assert_val_type(stack.pop_val());
                    let n = resultt.len();
                    let cont = &self.ip[1..];
                    if c != 0 {
                        self.enter_block(&jump_target_if, n, cont)
                    } else {
                        self.enter_block(&jump_target_else, n, cont)
                    }
                },
                Br(l) => {
                    self.brop(l)
                },
                BrIf(l) => {
                    let stack = &mut *self.stack;

                    let c = I32::assert_val_type(stack.pop_val());
                    if c != 0 {
                        self.brop(l)
                    } else {
                        self.jump_next()
                    }
                },
                BrTable(ref ls, ln) => {
                    let stack = &mut *self.stack;

                    let i = I32::assert_val_type(stack.pop_val()) as usize;
                    if i < ls.len() {
                        let li = ls[i];
                        self.brop(li)
                    } else {
                        self.brop(ln)
                    }
                },
                Return => {
                    let stack = &mut *self.stack;

                    let n = stack.current_frame_arity();
                    assert!(n <= 1);
                    let mut vals = None;
                    if n == 1 {
                        vals = Some(stack.pop_val());
                    }
                    let next_instr;
                    loop {
                        match stack.top().unwrap() {
                            StackElem::Val(_) => {
                                stack.pop_val();
                            }
                            StackElem::Label { .. } => {
                                stack.pop_label();
                            }
                            StackElem::Activation { .. } => {
                                next_instr = stack.pop_frame().next_instr;
                                break;
                            }
                        }
                    }
                    if let Some(val) = vals {
                        stack.push_val(val);
                    }
                    self.jump(next_instr)
                },
                Call(x) => {
                    let stack = &mut *self.stack;
                    let store = &mut *self.store;

                    let a = stack.current_frame().module;
                    let a = store.modules[a].funcaddrs[x];
                    self.invokeop(a)?
                },
                CallIndirect(x) => {
                    let stack = &mut *self.stack;
                    let store = &mut *self.store;

                    let ma = stack.current_frame().module;
                    let ta = store.modules[ma].tableaddrs[TableIdx(0)];
                    let tab = &store.tables[ta];
                    let ft_expect = &store.modules[ma].types[x.0 as usize];
                    let i = I32::assert_val_type(stack.pop_val()) as usize;
                    if i >= tab.elem.len() {
                        Err(Trap)?;
                    }
                    if tab.elem[i].0.is_none() {
                        Err(Trap)?;
                    }
                    let a = tab.elem[i].0.unwrap();
                    let f = &store.funcs[a];
                    let ft_actual = f.type_();
                    if ft_expect != ft_actual {
                        Err(Trap)?;
                    }
                    self.invokeop(a)?
                }
            };
        }

        Ok(())
    }
}
