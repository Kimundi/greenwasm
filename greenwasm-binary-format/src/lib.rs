#![feature(int_to_from_bytes)]

#![recursion_limit="256"]

#![allow(non_snake_case, unused_imports)]

#[macro_use]
extern crate nom;

extern crate greenwasm_structure;

pub use nom::ErrorKind as NomErrorKind;

// TODO: Open PR in nom for where applicable
macro_rules! verify_ref (
  // Internal parser, do not use directly
  (__impl $i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => (
    {
      use nom::lib::std::result::Result::*;
      use nom::{Err,ErrorKind};

      let i_ = $i.clone();
      match $submac!(i_, $($args)*) {
        Err(e)     => Err(e),
        Ok((i, o)) => if $submac2!(&o, $($args2)*) {
          Ok((i, o))
        } else {
          Err(Err::Error(error_position!($i, ErrorKind::Verify)))
        }
      }
    }
  );
  ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => (
    verify_ref!(__impl $i, $submac!($($args)*), call!($g));
  );
  ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => (
    verify_ref!(__impl $i, $submac!($($args)*), $submac2!($($args2)*));
  );
  ($i:expr, $f:expr, $g:expr) => (
    verify_ref!(__impl $i, call!($f), call!($g));
  );
  ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => (
    verify_ref!(__impl $i, call!($f), $submac!($($args)*));
  );
);
macro_rules! btag {
    ($i:expr, $b:expr) => (tag!($i, &[$b][..]))
}
macro_rules! btagmap {
    ($i:expr, $b:expr, $r:expr) => (value!($i, $r, btag!($b)))
}

use nom::IResult;
use nom::types::CompleteByteSlice;

type Inp<'a> = CompleteByteSlice<'a>;

// 5.1.3. Vectors
use greenwasm_structure::types::Wec;
fn parse_vec<'a, F, B>(input: Inp<'a>, parse_b: F) -> IResult<Inp<'a>, Wec<B>>
    where F: Fn(Inp<'a>) -> IResult<Inp<'a>, B>
{
    do_parse!(input,
        n: apply!(parse_uN, 32)
        >> res: many_m_n!(n as usize, n as usize, parse_b)
        >> (res.into())
    )
}

// 5.2.1. Bytes
named!(parse_byte <Inp, u8>, map!(take!(1), |n| n[0]));

// 5.2.2. Integers
named_args!(parse_uN(N: u32) <Inp, u64>, alt!(
    do_parse!(
        n: verify!(parse_byte, |n| {
            // n < 2^7 ∧ n < 2^N
            let n = n as u128;
            let v27 = 1 << 7;
            let v2N = 1 << N;
            n < v27 && n < v2N
        })
        >> (n as u64)
    )
    | do_parse!(
        n: verify!(parse_byte, |n| {
            // n ≥ 2^7 ∧ N > 7
            let n = n as u128;
            let v27 = 1 << 7;
            n >= v27 && N > 7
        })
        >> m: apply!(parse_uN, N - 7)
        >> ((1 << 7) * m + ((n as u64) - (1 << 7)))
    )
));

named!(parse_u32 <Inp, u32>, map!(apply!(parse_uN, 32), |x| x as u32));
named!(parse_u64 <Inp, u64>, apply!(parse_uN, 64));

named_args!(parse_sN(N: u32) <Inp, i64>, alt!(
    do_parse!(
        n: verify!(parse_byte, |n| {
            // n < 2^6 ∧ n < 2^(N−1)
            let n = n as i128;
            let v26 = 1 << 6;
            let v2N1 = 1 << (N - 1);
            n < v26 && n < v2N1
        })
        >> (n as i64)
    )
    | do_parse!(
        n: verify!(parse_byte, |n| {
            // 2^6 ≤ n < 2^7 ∧ n ≥ 2^7 − 2^(N − 1)
            let n = n as i128;
            let v26 = 1 << 6;
            let v27 = 1 << 7;
            let v2N1 = 1 << (N - 1);
            v26 <= n && n < v27 && n >= (v27 - v2N1)
        })
        >> (n as i64 - (1 << 7))
    )
    | do_parse!(
        n: verify!(parse_byte, |n| {
            // n ≥ 2^7 ∧ N > 7
            let n = n as i128;
            let v27 = 1 << 7;
            n >= v27 && N > 7
        })
        >> m: apply!(parse_sN, N - 7)
        >> ((1 << 7) * m + ((n as i64) - (1 << 7)))
    )
));

named!(parse_s32 <Inp, i32>, map!(apply!(parse_sN, 32), |x| x as i32));
named!(parse_s64 <Inp, i64>, apply!(parse_sN, 64));

named!(parse_i32 <Inp, u32>, map!(parse_s32, |x| x as u32));
named!(parse_i64 <Inp, u64>, map!(parse_s64, |x| x as u64));

// 5.2.3. Floating-Point
named!(parse_f32 <Inp, f32>, do_parse!(
    bs: map!(take!(4), |s| { let mut b = [0; 4]; b.copy_from_slice(&**s); b })
    >> (f32::from_bits(u32::from_le_bytes(bs)))
));
named!(parse_f64 <Inp, f64>, do_parse!(
    bs: map!(take!(8), |s| { let mut b = [0; 8]; b.copy_from_slice(&**s); b })
    >> (f64::from_bits(u64::from_le_bytes(bs)))
));

// 5.2.4. Names
use greenwasm_structure::types::Name;
named!(parse_name <Inp, Name>, do_parse!(
    bs: map!(
        verify_ref!(
            map!(
                call!(parse_vec, parse_byte),
                |v| String::from_utf8(v.into())
            ),
            |res: &Result<_, _>| res.is_ok()
        ),
        |res| res.unwrap().into()
    )
    >> (bs)
));

// 5.3.1 Value Types
use greenwasm_structure::types::ValType;
named!(parse_valtype <Inp, ValType>, alt!(
    btagmap!(0x7f, ValType::I32)
    | btagmap!(0x7e, ValType::I64)
    | btagmap!(0x7d, ValType::F32)
    | btagmap!(0x7c, ValType::F64)
));

// 5.3.2 Result Types
use greenwasm_structure::types::ResultType;
named!(parse_blocktype <Inp, ResultType>, alt!(
    btagmap!(0x40, None.into())
    | map!(parse_valtype, |v| Some(v).into())
));

// 5.3.3 Function Types
use greenwasm_structure::types::FuncType;
named!(parse_functype <Inp, FuncType>, do_parse!(
    btag!(0x60)
    >> t1s: call!(parse_vec, parse_valtype)
    >> t2s: call!(parse_vec, parse_valtype)
    >> (FuncType {
        args: t1s,
        results: t2s,
    })
));

// 5.3.4 Limits
use greenwasm_structure::types::Limits;
named!(parse_limits <Inp, Limits>, alt!(
    do_parse!(
        btag!(0x00)
        >> n: parse_u32
        >> (Limits { min: n, max: None })
    )
    |do_parse!(
        btag!(0x01)
        >> n: parse_u32
        >> m: parse_u32
        >> (Limits { min: n, max: Some(m) })
    )
));

// 5.3.5 Memory Types
use greenwasm_structure::types::MemType;
named!(parse_memtype <Inp, MemType>, map!(parse_limits, |limits| MemType { limits }));

// 5.3.6. Table Types
use greenwasm_structure::types::TableType;
use greenwasm_structure::types::ElemType;
named!(parse_tabletype <Inp, TableType>, do_parse!(
    et: parse_elemtype
    >> lim: parse_limits
    >> (TableType{ limits: lim, elemtype: et })
));
named!(parse_elemtype <Inp, ElemType>, btagmap!(0x70, ElemType::AnyFunc));

// 5.3.7. Global Types
use greenwasm_structure::types::GlobalType;
use greenwasm_structure::types::Mut;
named!(parse_globaltype <Inp, GlobalType>, do_parse!(
    t: parse_valtype
    >> m: parse_mut
    >> (GlobalType{ mutability: m, valtype: t })
));
named!(parse_mut <Inp, Mut>, alt!(
    btagmap!(0x00, Mut::Const)
    | btagmap!(0x01, Mut::Var)
));

// 5.4. Instructions
use greenwasm_structure::instructions::Instr;

// NB: The canonical grammar is defined with recursion for nested control
// instructions. This can lead to stack overflows during parsing, so
// we modified the parser such that it builds up a stack of instructions
// in the heap instead.

#[derive(Debug)]
enum InstrEvent {
    Expr,
    Instr(Instr),
    Block(ResultType),
    Loop(ResultType),
    If(ResultType),
    Else,
    End,
}
impl Into<InstrEvent> for Instr {
    fn into(self) -> InstrEvent {
        InstrEvent::Instr(self)
    }
}
#[derive(Default)]
struct InstrStack {
    stack: Vec<(InstrEvent, Vec<Instr>)>,
}
#[derive(Debug)]
struct InstrStackError(&'static str);
impl InstrStack {
    fn new() -> Self { Default::default() }
    fn top(&mut self) -> &mut (InstrEvent, Vec<Instr>) {
        self.stack.last_mut().unwrap()
    }
    fn error(&mut self, msg: &'static str) -> Result<(), InstrStackError> {
        Err(InstrStackError(msg))
    }
    fn event(&mut self, e: InstrEvent) -> Result<Option<Vec<Instr>>, InstrStackError> {
        match e {
            InstrEvent::Expr => {
                assert!(self.stack.is_empty());
                self.stack.push((InstrEvent::Expr, vec![]));
            }
            InstrEvent::Instr(instr) => {
                assert!(!self.stack.is_empty());
                self.top().1.push(instr);
            }
            InstrEvent::Block(rt) => {
                self.stack.push((InstrEvent::Block(rt), vec![]));
            }
            InstrEvent::Loop(rt) => {
                self.stack.push((InstrEvent::Loop(rt), vec![]));
            }
            InstrEvent::If(rt) => {
                self.stack.push((InstrEvent::If(rt), vec![]));
            }
            InstrEvent::Else => {
                assert!(!self.stack.is_empty());
                if let InstrEvent::If(_) = self.top().0 {
                    self.stack.push((InstrEvent::Else, vec![]));
                } else {
                    self.error("else without previous if")?;
                }
            }
            InstrEvent::End => {
                let old_top = self.stack.pop()
                    .expect(concat!(
                        "stack bottom element will always be Expr, ",
                        "which ends the loop"
                    ));

                match old_top {
                    (InstrEvent::Expr, ins) => {
                        assert!(self.stack.is_empty());
                        return Ok(Some(ins));
                    }
                    (InstrEvent::Block(rt), ins) => {
                        self.top().1.push(Instr::Block(rt, ins));
                    }
                    (InstrEvent::Loop(rt), ins) => {
                        self.top().1.push(Instr::Loop(rt, ins));
                    }
                    (InstrEvent::If(rt), ins) => {
                        self.top().1.push(Instr::IfElse(rt, ins, vec![]));
                    }
                    (InstrEvent::Else, else_ins) => {
                        let old_top2 = self.stack.pop()
                            .expect(concat!(
                                "top level element will always be If"
                            ));
                        if let (InstrEvent::If(rt), if_ins) = old_top2 {
                            self.top().1.push(Instr::IfElse(rt, if_ins, else_ins));
                        } else {
                            unreachable!()
                        }
                    }
                    (InstrEvent::End, _) | (InstrEvent::Instr(_), _) => {
                        unreachable!()
                    }
                }
            }
        }

        Ok(None)
    }
}
fn parse_instrs_end(i: Inp) -> IResult<Inp, Vec<Instr>> {
    use nom::Err;

    let mut input = i;
    let mut stack = InstrStack::new();
    stack.event(InstrEvent::Expr).expect("stack has exactly one Expr at its bottom");

    loop {
        match parse_instr_event(input) {
            Ok((i, o)) => {
                // loop trip must always consume (otherwise infinite loops)
                if i == input {
                    return Err(Err::Error(error_position!(input, nom::ErrorKind::Custom(0))));
                }

                match stack.event(o) {
                    Ok(o) => {
                        input = i;

                        if let Some(ins) = o {
                            return Ok((input, ins));
                        }
                    }
                    Err(_e) => {
                        // TODO: Expose error message
                        return Err(Err::Error(error_position!(input, nom::ErrorKind::Custom(1))));
                    }
                }
            },
            Err(e) => {
                return Err(e);
            },
        }
    }
}

macro_rules! ins {
    ($i:expr, $b:expr, $r:expr; $($t:tt)*) => (
        do_parse!($i, btag!($b) >> $($t)* >> ($r.into()))
    );
    ($i:expr, $b:expr, $r:expr) => (
        do_parse!($i, btag!($b) >> ($r.into()))
    )
}
named!(parse_instr_event <Inp, InstrEvent>, alt!(
    // 5.4.1. Control Instructions
    ins!(0x00, Instr::Unreachable)
    | ins!(0x01, Instr::Nop)
    | ins!(0x02, InstrEvent::Block(rt);
        rt: parse_blocktype
    )
    | ins!(0x03, InstrEvent::Loop(rt);
        rt: parse_blocktype
    )
    | ins!(0x04, InstrEvent::If(rt);
        rt: parse_blocktype
    )
    | ins!(0x05, InstrEvent::Else)
    | ins!(0x0B, InstrEvent::End)
    | ins!(0x0c, Instr::Br(l);
        l: parse_labelidx
    )
    | ins!(0x0d, Instr::BrIf(l);
        l: parse_labelidx
    )
    | ins!(0x0e, Instr::BrTable(ls, lN);
        ls: call!(parse_vec, parse_labelidx)
        >> lN: parse_labelidx
    )
    | ins!(0x0f, Instr::Return)
    | ins!(0x10, Instr::Call(x);
        x: parse_funcidx
    )
    | ins!(0x11, Instr::CallIndirect(x);
        x: parse_typeidx
        >> btag!(0x00)
    )

    // 5.4.2. Parametric Instructions
    | ins!(0x1A, Instr::Drop)
    | ins!(0x1B, Instr::Select)

    // 5.4.3. Variable Instructions
    | ins!(0x20, Instr::GetLocal(x); x: parse_localidx)
    | ins!(0x21, Instr::SetLocal(x); x: parse_localidx)
    | ins!(0x22, Instr::TeeLocal(x); x: parse_localidx)
    | ins!(0x23, Instr::GetGlobal(x); x: parse_globalidx)
    | ins!(0x24, Instr::SetGlobal(x); x: parse_globalidx)

    // 5.4.4. Memory Instructions
    | ins!(0x28, Instr::I32Load(m); m: parse_memarg)
    | ins!(0x29, Instr::I64Load(m); m: parse_memarg)
    | ins!(0x2A, Instr::F32Load(m); m: parse_memarg)
    | ins!(0x2B, Instr::F64Load(m); m: parse_memarg)

    | ins!(0x2C, Instr::I32Load8S(m); m: parse_memarg)
    | ins!(0x2D, Instr::I32Load8U(m); m: parse_memarg)
    | ins!(0x2E, Instr::I32Load16S(m); m: parse_memarg)
    | ins!(0x2F, Instr::I32Load16U(m); m: parse_memarg)

    | ins!(0x30, Instr::I64Load8S(m); m: parse_memarg)
    | ins!(0x31, Instr::I64Load8U(m); m: parse_memarg)
    | ins!(0x32, Instr::I64Load16S(m); m: parse_memarg)
    | ins!(0x33, Instr::I64Load16U(m); m: parse_memarg)
    | ins!(0x34, Instr::I64Load32S(m); m: parse_memarg)
    | ins!(0x35, Instr::I64Load32U(m); m: parse_memarg)

    | ins!(0x36, Instr::I32Store(m); m: parse_memarg)
    | ins!(0x37, Instr::I64Store(m); m: parse_memarg)
    | ins!(0x38, Instr::F32Store(m); m: parse_memarg)
    | ins!(0x39, Instr::F64Store(m); m: parse_memarg)

    | ins!(0x3A, Instr::I32Store8(m); m: parse_memarg)
    | ins!(0x3B, Instr::I32Store16(m); m: parse_memarg)

    | ins!(0x3C, Instr::I64Store8(m); m: parse_memarg)
    | ins!(0x3D, Instr::I64Store16(m); m: parse_memarg)
    | ins!(0x3E, Instr::I64Store32(m); m: parse_memarg)

    | ins!(0x3F, Instr::CurrentMemory; verify!(parse_byte, |b| b == 0))
    | ins!(0x40, Instr::GrowMemory; verify!(parse_byte, |b| b == 0))

    // 5.4.5. Numeric Instructions
    | ins!(0x41, Instr::I32Const(n); n: parse_i32)
    | ins!(0x42, Instr::I64Const(n); n: parse_i64)
    | ins!(0x43, Instr::F32Const(z); z: parse_f32)
    | ins!(0x44, Instr::F64Const(z); z: parse_f64)

    | ins!(0x45, Instr::I32EqZ)
    | ins!(0x46, Instr::I32Eq)
    | ins!(0x47, Instr::I32Ne)
    | ins!(0x48, Instr::I32LtS)
    | ins!(0x49, Instr::I32LtU)
    | ins!(0x4A, Instr::I32GtS)
    | ins!(0x4B, Instr::I32GtU)
    | ins!(0x4C, Instr::I32LeS)
    | ins!(0x4D, Instr::I32LeU)
    | ins!(0x4E, Instr::I32GeS)
    | ins!(0x4F, Instr::I32GeU)

    | ins!(0x50, Instr::I64EqZ)
    | ins!(0x51, Instr::I64Eq)
    | ins!(0x52, Instr::I64Ne)
    | ins!(0x53, Instr::I64LtS)
    | ins!(0x54, Instr::I64LtU)
    | ins!(0x55, Instr::I64GtS)
    | ins!(0x56, Instr::I64GtU)
    | ins!(0x57, Instr::I64LeS)
    | ins!(0x58, Instr::I64LeU)
    | ins!(0x59, Instr::I64GeS)
    | ins!(0x5A, Instr::I64GeU)

    | ins!(0x5B, Instr::F32Eq)
    | ins!(0x5C, Instr::F32Ne)
    | ins!(0x5D, Instr::F32Lt)
    | ins!(0x5E, Instr::F32Gt)
    | ins!(0x5F, Instr::F32Le)
    | ins!(0x60, Instr::F32Ge)

    | ins!(0x61, Instr::F64Eq)
    | ins!(0x62, Instr::F64Ne)
    | ins!(0x63, Instr::F64Lt)
    | ins!(0x64, Instr::F64Gt)
    | ins!(0x65, Instr::F64Le)
    | ins!(0x66, Instr::F64Ge)

    | ins!(0x67, Instr::I32Clz)
    | ins!(0x68, Instr::I32Ctz)
    | ins!(0x69, Instr::I32Popcnt)
    | ins!(0x6A, Instr::I32Add)
    | ins!(0x6B, Instr::I32Sub)
    | ins!(0x6C, Instr::I32Mul)
    | ins!(0x6D, Instr::I32DivS)
    | ins!(0x6E, Instr::I32DivU)
    | ins!(0x6F, Instr::I32RemS)
    | ins!(0x70, Instr::I32RemU)
    | ins!(0x71, Instr::I32And)
    | ins!(0x72, Instr::I32Or)
    | ins!(0x73, Instr::I32Xor)
    | ins!(0x74, Instr::I32Shl)
    | ins!(0x75, Instr::I32ShrS)
    | ins!(0x76, Instr::I32ShrU)
    | ins!(0x77, Instr::I32Rotl)
    | ins!(0x78, Instr::I32Rotr)

    | ins!(0x79, Instr::I64Clz)
    | ins!(0x7A, Instr::I64Ctz)
    | ins!(0x7B, Instr::I64Popcnt)
    | ins!(0x7C, Instr::I64Add)
    | ins!(0x7D, Instr::I64Sub)
    | ins!(0x7E, Instr::I64Mul)
    | ins!(0x7F, Instr::I64DivS)
    | ins!(0x80, Instr::I64DivU)
    | ins!(0x81, Instr::I64RemS)
    | ins!(0x82, Instr::I64RemU)
    | ins!(0x83, Instr::I64And)
    | ins!(0x84, Instr::I64Or)
    | ins!(0x85, Instr::I64Xor)
    | ins!(0x86, Instr::I64Shl)
    | ins!(0x87, Instr::I64ShrS)
    | ins!(0x88, Instr::I64ShrU)
    | ins!(0x89, Instr::I64Rotl)
    | ins!(0x8A, Instr::I64Rotr)

    | ins!(0x8B, Instr::F32Abs)
    | ins!(0x8C, Instr::F32Neg)
    | ins!(0x8D, Instr::F32Ceil)
    | ins!(0x8E, Instr::F32Floor)
    | ins!(0x8F, Instr::F32Trunc)
    | ins!(0x90, Instr::F32Nearest)
    | ins!(0x91, Instr::F32Sqrt)
    | ins!(0x92, Instr::F32Add)
    | ins!(0x93, Instr::F32Sub)
    | ins!(0x94, Instr::F32Mul)
    | ins!(0x95, Instr::F32Div)
    | ins!(0x96, Instr::F32Min)
    | ins!(0x97, Instr::F32Max)
    | ins!(0x98, Instr::F32CopySign)

    | ins!(0x99, Instr::F64Abs)
    | ins!(0x9A, Instr::F64Neg)
    | ins!(0x9B, Instr::F64Ceil)
    | ins!(0x9C, Instr::F64Floor)
    | ins!(0x9D, Instr::F64Trunc)
    | ins!(0x9E, Instr::F64Nearest)
    | ins!(0x9F, Instr::F64Sqrt)
    | ins!(0xA0, Instr::F64Add)
    | ins!(0xA1, Instr::F64Sub)
    | ins!(0xA2, Instr::F64Mul)
    | ins!(0xA3, Instr::F64Div)
    | ins!(0xA4, Instr::F64Min)
    | ins!(0xA5, Instr::F64Max)
    | ins!(0xA6, Instr::F64CopySign)

    | ins!(0xA7, Instr::I32WrapI64)
    | ins!(0xA8, Instr::I32TruncSF32)
    | ins!(0xA9, Instr::I32TruncUF32)
    | ins!(0xAA, Instr::I32TruncSF64)
    | ins!(0xAB, Instr::I32TruncUF64)

    | ins!(0xAC, Instr::I64ExtendSI32)
    | ins!(0xAD, Instr::I64ExtendUI32)
    | ins!(0xAE, Instr::I64TruncSF32)
    | ins!(0xAF, Instr::I64TruncUF32)
    | ins!(0xB0, Instr::I64TruncSF64)
    | ins!(0xB1, Instr::I64TruncUF64)

    | ins!(0xB2, Instr::F32ConvertSI32)
    | ins!(0xB3, Instr::F32ConvertUI32)
    | ins!(0xB4, Instr::F32ConvertSI64)
    | ins!(0xB5, Instr::F32ConvertUI64)
    | ins!(0xB6, Instr::F32DemoteF64)

    | ins!(0xB7, Instr::F64ConvertSI32)
    | ins!(0xB8, Instr::F64ConvertUI32)
    | ins!(0xB9, Instr::F64ConvertSI64)
    | ins!(0xBA, Instr::F64ConvertUI64)
    | ins!(0xBB, Instr::F64PromoteF32)

    | ins!(0xBC, Instr::I32ReinterpretF32)
    | ins!(0xBD, Instr::I64ReinterpretF64)
    | ins!(0xBE, Instr::F32ReinterpretI32)
    | ins!(0xBF, Instr::F64ReinterpretI64)
));
use greenwasm_structure::instructions::Memarg;
named!(parse_memarg <Inp, Memarg>, do_parse!(
    a: parse_u32
    >> o: parse_u32
    >> (Memarg { offset: o, align: a })
));

// 5.4.6. Expressions
use greenwasm_structure::instructions::Expr;
named!(parse_expr <Inp, Expr>, do_parse!(
    ins: parse_instrs_end
    >> (Expr { body: ins })
));

// 5.5.1. Indices
use greenwasm_structure::modules::TypeIdx;
use greenwasm_structure::modules::FuncIdx;
use greenwasm_structure::modules::TableIdx;
use greenwasm_structure::modules::MemIdx;
use greenwasm_structure::modules::GlobalIdx;
use greenwasm_structure::modules::LocalIdx;
use greenwasm_structure::modules::LabelIdx;
named!(parse_typeidx <Inp, TypeIdx>,     map!(parse_u32, TypeIdx));
named!(parse_funcidx <Inp, FuncIdx>,     map!(parse_u32, FuncIdx));
named!(parse_tableidx <Inp, TableIdx>,   map!(parse_u32, TableIdx));
named!(parse_memidx <Inp, MemIdx>,       map!(parse_u32, MemIdx));
named!(parse_globalidx <Inp, GlobalIdx>, map!(parse_u32, GlobalIdx));
named!(parse_localidx <Inp, LocalIdx>,   map!(parse_u32, LocalIdx));
named!(parse_labelidx <Inp, LabelIdx>,   map!(parse_u32, LabelIdx));

// 5.5.1. Sections
fn parse_section<'a, F, B>(input: Inp<'a>, N: u8, parse_B: F) -> IResult<Inp<'a>, B>
    where F: Fn(Inp<'a>) -> IResult<Inp<'a>, B>
{
    do_parse!(input,
        btag!(N)
        >> cont: length_value!(
            parse_u32,
            exact!(parse_B)
        )
        >> (cont)
    )
}

// 5.5.3. Custom Section
#[derive(Debug, PartialEq)]
pub struct CustomSection {
    pub name: Name,
    pub bytes: Vec<u8>,
}
named!(parse_custom <Inp, CustomSection>, do_parse!(
    name: parse_name
    >> bytes: many0!(parse_byte)
    >> (CustomSection { name, bytes })
));
named!(parse_customsec <Inp, CustomSection>,
    call!(parse_section, 0, parse_custom)
);
named!(parse_customsecs <Inp, Vec<CustomSection>>,
    many0!(parse_customsec)
);

// 5.5.4. Type Section
named!(parse_type <Inp, Wec<FuncType>>, call!(parse_vec, parse_functype));
named!(parse_typesec <Inp, Wec<FuncType>>,
    map!(opt!(call!(parse_section, 1, parse_type)), |x| x.unwrap_or_default())
);

// 5.5.5. Import Section
use greenwasm_structure::modules::Import;
use greenwasm_structure::modules::ImportDesc;
named!(parse_import <Inp, Import>, do_parse!(
    module: parse_name
    >> name: parse_name
    >> desc: parse_importdesc
    >> (Import { module, name, desc })
));
named!(parse_imports <Inp, Wec<Import>>, call!(parse_vec, parse_import));
named!(parse_importsec <Inp, Wec<Import>>,
    map!(opt!(call!(parse_section, 2, parse_imports)), |x| x.unwrap_or_default())
);
named!(parse_importdesc <Inp, ImportDesc>, alt!(
      do_parse!(btag!(0x00) >> x:  parse_typeidx    >> (ImportDesc::Func(x)))
    | do_parse!(btag!(0x01) >> tt: parse_tabletype  >> (ImportDesc::Table(tt)))
    | do_parse!(btag!(0x02) >> mt: parse_memtype    >> (ImportDesc::Mem(mt)))
    | do_parse!(btag!(0x03) >> gt: parse_globaltype >> (ImportDesc::Global(gt)))
));

// 5.5.6. Function Section
named!(parse_func_ <Inp, Wec<TypeIdx>>, call!(parse_vec, parse_typeidx));
named!(parse_funcsec <Inp, Wec<TypeIdx>>,
    map!(opt!(call!(parse_section, 3, parse_func_)), |x| x.unwrap_or_default())
);

// 5.5.7. Table Section
use greenwasm_structure::modules::Table;
named!(parse_table <Inp, Table>, do_parse!(
    tt: parse_tabletype
    >> (Table { type_: tt })
));
named!(parse_tables <Inp, Wec<Table>>, call!(parse_vec, parse_table));
named!(parse_tablesec <Inp, Wec<Table>>,
    map!(opt!(call!(parse_section, 4, parse_tables)), |x| x.unwrap_or_default())
);

// 5.5.8. Memory Section
use greenwasm_structure::modules::Mem;
named!(parse_mem <Inp, Mem>, do_parse!(
    mt: parse_memtype
    >> (Mem { type_: mt })
));
named!(parse_mems <Inp, Wec<Mem>>, call!(parse_vec, parse_mem));
named!(parse_memsec <Inp, Wec<Mem>>,
    map!(opt!(call!(parse_section, 5, parse_mems)), |x| x.unwrap_or_default())
);

// 5.5.9. Global Section
use greenwasm_structure::modules::Global;
named!(parse_global <Inp, Global>, do_parse!(
    gt: parse_globaltype
    >> e: parse_expr
    >> (Global { type_: gt, init: e })
));
named!(parse_globals <Inp, Wec<Global>>, call!(parse_vec, parse_global));
named!(parse_globalsec <Inp, Wec<Global>>,
    map!(opt!(call!(parse_section, 6, parse_globals)), |x| x.unwrap_or_default())
);

// 5.5.10. Export Section
use greenwasm_structure::modules::Export;
use greenwasm_structure::modules::ExportDesc;
named!(parse_export <Inp, Export>, do_parse!(
    name: parse_name
    >> desc: parse_exportdesc
    >> (Export { name, desc })
));
named!(parse_exports <Inp, Wec<Export>>, call!(parse_vec, parse_export));
named!(parse_exportsec <Inp, Wec<Export>>,
    map!(opt!(call!(parse_section, 7, parse_exports)), |x| x.unwrap_or_default())
);
named!(parse_exportdesc <Inp, ExportDesc>, alt!(
      do_parse!(btag!(0x00) >> x: parse_funcidx    >> (ExportDesc::Func(x)))
    | do_parse!(btag!(0x01) >> x: parse_tableidx   >> (ExportDesc::Table(x)))
    | do_parse!(btag!(0x02) >> x: parse_memidx     >> (ExportDesc::Mem(x)))
    | do_parse!(btag!(0x03) >> x: parse_globalidx  >> (ExportDesc::Global(x)))
));

// 5.5.11. Start Section
use greenwasm_structure::modules::Start;
named!(parse_start <Inp, Start>, do_parse!(
    x: parse_funcidx
    >> (Start { func: x })
));
named!(parse_startsec <Inp, Option<Start>>,
    opt!(call!(parse_section, 8, parse_start))
);

// 5.5.12. Element Section
use greenwasm_structure::modules::Elem;
named!(parse_elem <Inp, Elem>, do_parse!(
    x: parse_tableidx
    >> e: parse_expr
    >> ys: call!(parse_vec, parse_funcidx)
    >> (Elem { table: x, offset: e, init: ys })
));
named!(parse_elems <Inp, Wec<Elem>>, call!(parse_vec, parse_elem));
named!(parse_elemsec <Inp, Wec<Elem>>,
    map!(opt!(call!(parse_section, 9, parse_elems)), |x| x.unwrap_or_default())
);

// 5.5.13. Code Section
#[derive(Debug, PartialEq)]
struct Code {
    locals: Wec<ValType>,
    body: Expr,
}
named!(parse_locals <Inp, (usize, ValType)>, do_parse!(
    n: parse_u32
    >> t: parse_valtype
    >> ((n as usize, t))
));
named!(parse_func <Inp, Code>, do_parse!(
    tss: call!(parse_vec, parse_locals)
    >> tss: verify_ref!(
        value!(tss),
        |tss: &Wec<(_, _)>|
            tss.iter().map(|x| x.0 as u64).sum::<u64>()
            <= ::greenwasm_structure::types::WEC_MAX_SIZE as u64
    )
    >> ts: map!(
        value!(tss),
        |tss| tss.into_iter()
                 .flat_map(|(n, t)| ::std::iter::repeat(t).take(n))
                 .collect::<Vec<_>>().into()
    )
    >> e: parse_expr
    >> (Code { locals: ts, body: e })
));
named!(parse_code <Inp, Code>, do_parse!(
    code: length_value!(
        parse_u32,
        exact!(parse_func)
    )
    >> (code)
));
named!(parse_codes <Inp, Wec<Code>>, call!(parse_vec, parse_code));
named!(parse_codesec <Inp, Wec<Code>>,
    map!(opt!(call!(parse_section, 10, parse_codes)), |x| x.unwrap_or_default())
);

// 5.5.14. Data Section
use greenwasm_structure::modules::Data;
named!(parse_data <Inp, Data>, do_parse!(
    x: parse_memidx
    >> e: parse_expr
    >> bs: call!(parse_vec, parse_byte)
    >> (Data { data: x, offset: e, init: bs })
));
named!(parse_datas <Inp, Wec<Data>>, call!(parse_vec, parse_data));
named!(parse_datasec <Inp, Wec<Data>>,
    map!(opt!(call!(parse_section, 11, parse_datas)), |x| x.unwrap_or_default())
);

// 5.5.15. Modules
use greenwasm_structure::modules::Module;
use greenwasm_structure::modules::Func;
named!(parse_magic <Inp, ()>,
    value!((), tag!(&[0x00, 0x61, 0x73, 0x6D][..]))
);
named!(parse_version <Inp, ()>,
    value!((), tag!(&[0x01, 0x00, 0x00, 0x00][..]))
);
named!(parse_module <Inp, (Module, Vec<CustomSection>)>, do_parse!(
    parse_magic
    >> parse_version

    >> cs00: parse_customsecs
    >> types: parse_typesec
    >> cs01: parse_customsecs
    >> imports: parse_importsec
    >> cs02: parse_customsecs
    >> typeindices_n: parse_funcsec
    >> cs03: parse_customsecs
    >> tables: parse_tablesec
    >> cs04: parse_customsecs
    >> mems: parse_memsec
    >> cs05: parse_customsecs
    >> globals: parse_globalsec
    >> cs06: parse_customsecs
    >> exports: parse_exportsec
    >> cs07: parse_customsecs
    >> start: parse_startsec
    >> cs08: parse_customsecs
    >> elem: parse_elemsec
    >> cs09: parse_customsecs
    >> codes_n: parse_codesec
    >> cs10: parse_customsecs
    >> data: parse_datasec
    >> cs11: parse_customsecs

    >> verify!(value!(()), |_| typeindices_n.len() == codes_n.len())

    >> ({
        let funcs = typeindices_n.into_iter().zip(codes_n)
            .map(|(type_, Code { locals, body })| {
                Func { type_, locals, body }
            })
            .collect();
        let customs = vec![
            cs00,
            cs01,
            cs02,
            cs03,
            cs04,
            cs05,
            cs06,
            cs07,
            cs08,
            cs09,
            cs10,
            cs11,
        ].into_iter().flat_map(|cs| cs).collect();

        (
            Module {
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
            },
            customs
        )
    })
));

#[derive(Debug)]
pub enum ParseError<'a> {
    NomError(::nom::Err<CompleteByteSlice<'a>, u32>)
}
pub fn parse_binary_format(b: &[u8]) -> Result<(Module, Vec<CustomSection>), ParseError> {
    let res = exact!(CompleteByteSlice(b), parse_module);
    match res {
        Ok((CompleteByteSlice(s), res)) => {
            assert!(s.len() == 0);
            Ok(res)
        }
        Err(x) => Err(ParseError::NomError(x))
    }
}

#[cfg(test)]
#[path="tests_binary_format.rs"]
mod tests;
