#![allow(non_snake_case)]

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
    ($i:expr, $b:expr, $r:expr) => (map!($i, btag!($b), |_| $r))
}

use nom::IResult;
use nom::types::CompleteByteSlice;

type Inp<'a> = CompleteByteSlice<'a>;

// 5.1.3. Vectors
fn parse_vec<'a, F, B>(input: Inp<'a>, mut parse_b: F) -> IResult<Inp<'a>, Vec<B>>
    where F: FnMut(Inp<'a>) -> IResult<Inp<'a>, B>
{
    do_parse!(input,
        n: apply!(parse_uN, 32)
        >> res: many_m_n!(n as usize, n as usize, parse_b)
        >> (res)
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

named!(pub parse_u32 <Inp, u32>, map!(apply!(parse_uN, 32), |x| x as u32));
named!(pub parse_u64 <Inp, u64>, apply!(parse_uN, 64));

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
    >> (f32::from_bits(u32::from_le(u32::from_bytes(bs))))
));
named!(parse_f64 <Inp, f64>, do_parse!(
    bs: map!(take!(8), |s| { let mut b = [0; 8]; b.copy_from_slice(&**s); b })
    >> (f64::from_bits(u64::from_le(u64::from_bytes(bs))))
));

// 5.2.4. Names
named!(parse_name <Inp, String>, do_parse!(
    bs: map!(
        verify_ref!(
            map!(
                call!(parse_vec, parse_byte),
                String::from_utf8
            ),
            |res: &Result<_, _>| res.is_ok()
        ),
        |res| res.unwrap()
    )
    >> (bs)
));

// 5.3.1 Value Types
use structure::types::ValType;
named!(parse_valtype <Inp, ValType>, alt!(
    btagmap!(0x7f, ValType::I32)
    | btagmap!(0x7e, ValType::I64)
    | btagmap!(0x7d, ValType::F32)
    | btagmap!(0x7c, ValType::F64)
));

// 5.3.2 Result Types
use structure::types::ResultType;
named!(parse_blocktype <Inp, ResultType>, alt!(
    btagmap!(0x40, None)
    | map!(parse_valtype, |v| Some(v))
));

// 5.3.3 Function Types
use structure::types::FuncType;
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
use structure::types::Limits;
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
use structure::types::MemType;
named!(parse_memtype <Inp, MemType>, map!(parse_limits, |limits| MemType { limits }));

// 5.3.6. Table Types
use structure::types::TableType;
use structure::types::ElemType;
named!(parse_tabletype <Inp, TableType>, do_parse!(
    et: parse_elemtype
    >> lim: parse_limits
    >> (TableType{ limits: lim, elemtype: et })
));
named!(parse_elemtype <Inp, ElemType>, btagmap!(0x70, ElemType::AnyFunc));

// 5.3.7. Global Types
use structure::types::GlobalType;
use structure::types::Mut;
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
macro_rules! ins {
    ($i:expr, $b:expr, $r:expr; $($t:tt)*) => (
        do_parse!($i, btag!($b) >> $($t)* >> ($r))
    );
    ($i:expr, $b:expr, $r:expr) => (
        do_parse!($i, btag!($b) >> ($r))
    )
}
use structure::instructions::Instr;
use structure::instructions::Ixx;
use structure::instructions::Sx;
use structure::instructions::TConst;
use structure::instructions::IUnop;
use structure::instructions::IBinop;
use structure::instructions::FUnop;
use structure::instructions::FBinop;
use structure::instructions::ITestop;
use structure::instructions::IRelop;
use structure::instructions::FRelop;
named!(parse_instr <Inp, Instr>, alt!(
    // 5.4.1. Control Instructions
    ins!(0x00, Instr::Unreachable)
    | ins!(0x01, Instr::Nop)
    | ins!(0x02, Instr::Block(rt, ins);
        rt: parse_blocktype
        >> ins: many0!(parse_instr)
        >> btag!(0x0b)
    )
    | ins!(0x03, Instr::Loop(rt, ins);
        rt: parse_blocktype
        >> ins: many0!(parse_instr)
        >> btag!(0x0b)
    )
    | ins!(0x04, Instr::IfElse(rt, ins1, ins2);
        rt: parse_blocktype
        >> ins1: many0!(parse_instr)
        >> ins2: map!(opt!(do_parse!(
            btag!(0x05)
            >> ins2: many0!(parse_instr)
            >> (ins2)
        )), |x| x.unwrap_or_default())
        >> btag!(0x0b)
    )
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
    | ins!(0x28, Instr::TLoad(ValType::I32, m); m: parse_memarg)
    | ins!(0x29, Instr::TLoad(ValType::I64, m); m: parse_memarg)
    | ins!(0x2A, Instr::TLoad(ValType::F32, m); m: parse_memarg)
    | ins!(0x2B, Instr::TLoad(ValType::F64, m); m: parse_memarg)

    | ins!(0x2C, Instr::IxxLoad8(Ixx::I32, Sx::S, m); m: parse_memarg)
    | ins!(0x2D, Instr::IxxLoad8(Ixx::I32, Sx::U, m); m: parse_memarg)
    | ins!(0x2E, Instr::IxxLoad16(Ixx::I32, Sx::S, m); m: parse_memarg)
    | ins!(0x2F, Instr::IxxLoad16(Ixx::I32, Sx::U, m); m: parse_memarg)

    | ins!(0x30, Instr::IxxLoad8(Ixx::I64, Sx::S, m); m: parse_memarg)
    | ins!(0x31, Instr::IxxLoad8(Ixx::I64, Sx::U, m); m: parse_memarg)
    | ins!(0x32, Instr::IxxLoad16(Ixx::I64, Sx::S, m); m: parse_memarg)
    | ins!(0x33, Instr::IxxLoad16(Ixx::I64, Sx::U, m); m: parse_memarg)
    | ins!(0x34, Instr::I64Load32(Sx::S, m); m: parse_memarg)
    | ins!(0x35, Instr::I64Load32(Sx::U, m); m: parse_memarg)

    | ins!(0x36, Instr::TStore(ValType::I32, m); m: parse_memarg)
    | ins!(0x37, Instr::TStore(ValType::I64, m); m: parse_memarg)
    | ins!(0x38, Instr::TStore(ValType::F32, m); m: parse_memarg)
    | ins!(0x39, Instr::TStore(ValType::F64, m); m: parse_memarg)

    | ins!(0x3A, Instr::IxxStore8(Ixx::I32, m); m: parse_memarg)
    | ins!(0x3B, Instr::IxxStore16(Ixx::I32, m); m: parse_memarg)

    | ins!(0x3C, Instr::IxxStore8(Ixx::I64, m); m: parse_memarg)
    | ins!(0x3D, Instr::IxxStore16(Ixx::I64, m); m: parse_memarg)
    | ins!(0x3E, Instr::I64Store32(m); m: parse_memarg)

    | ins!(0x3F, Instr::CurrentMemory)
    | ins!(0x40, Instr::GrowMemory)

    // 5.4.5. Numeric Instructions
    | ins!(0x41, Instr::TConst(TConst::I32(n)); n: parse_i32)
    | ins!(0x42, Instr::TConst(TConst::I64(n)); n: parse_i64)
    | ins!(0x43, Instr::TConst(TConst::F32(z)); z: parse_f32)
    | ins!(0x44, Instr::TConst(TConst::F64(z)); z: parse_f64)

    //| ins!(0x, Instr::IxxTestop(Ixx::i32, ITestop::EqZ)))
    //| ins!(0x, Instr::IxxTestop(Ixx::i32, ITestop::EqZ)))

));
use structure::instructions::Memarg;
named!(parse_memarg <Inp, Memarg>, do_parse!(
    a: parse_u32
    >> o: parse_u32
    >> (Memarg { offset: o, align: a })
));

// 5.5.1. Indices
use structure::modules::TypeIdx;
use structure::modules::FuncIdx;
use structure::modules::TableIdx;
use structure::modules::MemIdx;
use structure::modules::GlobalIdx;
use structure::modules::LocalIdx;
use structure::modules::LabelIdx;
named!(parse_typeidx <Inp, TypeIdx>, call!(parse_u32));
named!(parse_funcidx <Inp, FuncIdx>, call!(parse_u32));
named!(parse_tableidx <Inp, TableIdx>, call!(parse_u32));
named!(parse_memidx <Inp, MemIdx>, call!(parse_u32));
named!(parse_globalidx <Inp, GlobalIdx>, call!(parse_u32));
named!(parse_localidx <Inp, LocalIdx>, call!(parse_u32));
named!(parse_labelidx <Inp, LabelIdx>, call!(parse_u32));

#[cfg(test)]
#[path="tests_binary_format.rs"]
mod tests;
