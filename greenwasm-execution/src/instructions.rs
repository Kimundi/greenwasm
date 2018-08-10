use std::borrow::BorrowMut;
use std::marker::PhantomData;

use structure::types::*;
use structure::modules::*;
use structure::instructions::*;

use crate::runtime_structure::*;
use crate::numerics::*;
use crate::structure_references::*;

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
    fn assert_val_type(val: Val) -> Self {
        if let Val::I32(v) = val {
            v
        } else {
            panic!("Expected a value of type I32");
        }
    }
    fn to_val(self) -> Val { Val::I32(self) }
}
impl ValCast for I64 {
    fn assert_val_type(val: Val) -> Self {
        if let Val::I64(v) = val {
            v
        } else {
            panic!("Expected a value of type I64");
        }
    }
    fn to_val(self) -> Val { Val::I64(self) }
}
impl ValCast for F32 {
    fn assert_val_type(val: Val) -> Self {
        if let Val::F32(v) = val {
            v
        } else {
            panic!("Expected a value of type F32");
        }
    }
    fn to_val(self) -> Val { Val::F32(self) }
}
impl ValCast for F64 {
    fn assert_val_type(val: Val) -> Self {
        if let Val::F64(v) = val {
            v
        } else {
            panic!("Expected a value of type F64");
        }
    }
    fn to_val(self) -> Val { Val::F64(self) }
}

impl<Ref, Sto, Stk> ExecCtx<Ref, Sto, Stk>
    where Sto: BorrowMut<Store<Ref>>,
          Stk: BorrowMut<Stack>,
          Ref: StructureReference,
{
    pub fn new(store: Sto, stack: Stk) -> Self {
        ExecCtx {
            store,
            stack,
            _marker: PhantomData,
        }
    }

    pub fn store(&mut self) -> &mut Store<Ref> {
        self.store.borrow_mut()
    }

    pub fn stack(&mut self) -> &mut Stack {
        self.stack.borrow_mut()
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> Val {
        self.execute_instrs(&expr.body);
        let v = self.stack.borrow_mut().pop_val();
        v
    }

    fn unop<T: ValCast, F: FnOnce(T) -> T>(stack: &mut Stack, unop: F) {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = unop(c1);
        stack.push_val(c.to_val());
    }

    fn binop<T: ValCast, F: FnOnce(T, T) -> T>(stack: &mut Stack, binop: F) {
        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = binop(c1, c2);
        stack.push_val(c.to_val());
    }

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

    fn testop<T: ValCast, F: FnOnce(T) -> I32>(stack: &mut Stack, testop: F) {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = testop(c1);
        stack.push_val(c.to_val());
    }

    fn relop<T: ValCast, F: FnOnce(T, T) -> I32>(stack: &mut Stack, relop: F) {
        let val2 = stack.pop_val();
        let c2 = T::assert_val_type(val2);

        let val1 = stack.pop_val();
        let c1 = T::assert_val_type(val1);

        let c = relop(c1, c2);
        stack.push_val(c.to_val());
    }

    fn cvtop<T: ValCast, U: ValCast, F: FnOnce(T) -> U>(stack: &mut Stack, cvtop: F) {
        let val = stack.pop_val();
        let c1 = T::assert_val_type(val);
        let c = cvtop(c1);
        stack.push_val(c.to_val());
    }

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

    pub fn execute_instrs(&mut self, instrs: &[Instr]) -> EResult<()> {
        use self::Instr::*;
        let stack = self.stack.borrow_mut();
        let store = self.store.borrow_mut();

        for instr in instrs {
            match *instr {
                // consts
                I32Const(v) => stack.push_val(Val::I32(v)),
                I64Const(v) => stack.push_val(Val::I64(v)),
                F32Const(v) => stack.push_val(Val::F32(v)),
                F64Const(v) => stack.push_val(Val::F64(v)),

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
                Drop => { stack.pop_val(); },
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
                },

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
            }
        }

        Ok(())
    }


}
