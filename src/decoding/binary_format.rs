#![allow(non_snake_case)]

use nom::IResult;
use nom::types::CompleteByteSlice;
// TODO: Open PR for
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

#[cfg(test)]
#[path="tests_binary_format.rs"]
mod tests;
