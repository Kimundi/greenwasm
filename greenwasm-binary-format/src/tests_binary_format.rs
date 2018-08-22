#![allow(non_snake_case)]

use super::*;
use std::fmt::Debug;

enum CheckRes<T> {
    Failed,
    OkWith(T),
}
use self::CheckRes::*;

fn check<'a, T, F>(parse: F, input: &'a [u8], res: CheckRes<T>)
    where T: Debug + PartialEq,
          F: Fn(Inp<'a>) -> IResult<Inp<'a>, T>,
{
    let mut err = None;
    match parse(CompleteByteSlice(input)) {
        Ok((CompleteByteSlice(&[]), is)) => {
            if let OkWith(should) = res {
                if is != should {
                    err = Some(format!("Should have parsed with {:?}, but is {:?}", should, is));
                }
            } else {
                err = Some(format!("Should have failed, but parsed with {:?}", is));
            }
        }
        Ok((CompleteByteSlice(x), is)) => {
            err = Some(format!("Accepted without parsing all input. Remaining: {:?}, result: {:?}", x, is));
        }
        Err(x) => {
            if let OkWith(should) = res {
                err = Some(format!("Should have parsed with {:?}, but failed with {:?}", should, x));
            }
        }
    }
    if let Some(err) = err {
        panic!("\n\nERROR: {}\nInput:\n{:?}\n\n", err, input);
    } else {
        // println!("check OK");
    }
}

macro_rules! wec {
    ($($t:tt)*) => ({let v: Wec<_> = vec![$($t)*].into(); v})
}

fn test_parse_uN<F>(parse: F, bits: u32)
    where F: Fn(Inp) -> IResult<Inp, u64>,
{
    check(&parse, &[0x00], OkWith(0x00));

    check(&parse, &[0x00], OkWith(0x00));
    check(&parse, &[0x7f], OkWith(0x7f));
    check(&parse, &[0xff], Failed);

    check(&parse, &[0xff, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x01], OkWith(0xff));
    check(&parse, &[0xff, 0x80, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x80, 0x80, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
    if bits == 32 {
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x00], Failed);
    } else {
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], Failed);
    }

    check(&parse, &[0xff, 0xff, 0xff, 0x7f], OkWith(0b1111111_1111111_1111111_1111111));
    check(&parse, &[0b1_0011111, 0b1_0001111, 0b1_0000111, 0b0_0000011],
             OkWith(0b__0000011______0000111______0001111______0011111));
}

#[test]
fn test_parse_u32() {
    test_parse_uN(|inp| parse_u32(inp).map(|(i, x)| (i, x as u64)), 32);
}

#[test]
fn test_parse_u64() {
    test_parse_uN(parse_u64, 64);
}

fn test_parse_sN<F>(parse: F, bits: u32)
    where F: Fn(Inp) -> IResult<Inp, i64>,
{
    check(&parse, &[0x00], OkWith(0x00));

    check(&parse, &[0x00], OkWith(0x00));
    check(&parse, &[0x3f], OkWith(0x3f));
    check(&parse, &[0x7f], OkWith(-0x01));
    check(&parse, &[0xff], Failed);

    check(&parse, &[0xff, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x01], OkWith(0xff));
    check(&parse, &[0xff, 0x80, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x80, 0x80, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));

    check(&parse, &[0x7f], OkWith(-1));
    check(&parse, &[0xff, 0x7f], OkWith(-1));
    check(&parse, &[0xff, 0xff, 0x7f], OkWith(-1));
    check(&parse, &[0xff, 0xff, 0xff, 0x7f], OkWith(-1));
    check(&parse, &[0xff, 0xff, 0xff, 0xff, 0x7f], OkWith(-1));

    if bits == 32 {
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x00], Failed);
    } else {
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], Failed);
    }

    check(&parse, &[0xff, 0xff, 0xff, 0x7f], OkWith(-1));
    check(&parse, &[0b1_0011111, 0b1_0001111, 0b1_0000111, 0b0_0000011],
             OkWith(0b__0000011______0000111______0001111______0011111));
}

#[test]
fn test_parse_s32() {
    test_parse_sN(|inp| parse_s32(inp).map(|(i, x)| (i, x as i64)), 32);
}

#[test]
fn test_parse_s64() {
    test_parse_sN(parse_s64, 64);
}

fn test_parse_iN<F>(parse: F, bits: u32)
    where F: Fn(Inp) -> IResult<Inp, u64>,
{
    let minus1 = if bits == 32 {
        0xff_ff_ff_ff
    } else {
        0xff_ff_ff_ff_ff_ff_ff_ff
    };

    check(&parse, &[0x00], OkWith(0x00));

    check(&parse, &[0x00], OkWith(0x00));
    check(&parse, &[0x3f], OkWith(0x3f));
    check(&parse, &[0x7f], OkWith(minus1));
    check(&parse, &[0xff], Failed);

    check(&parse, &[0xff, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x01], OkWith(0xff));
    check(&parse, &[0xff, 0x80, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x80, 0x80, 0x00], OkWith(0x7f));
    check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));

    check(&parse, &[0x7f], OkWith(minus1));
    check(&parse, &[0xff, 0x7f], OkWith(minus1));
    check(&parse, &[0xff, 0xff, 0x7f], OkWith(minus1));
    check(&parse, &[0xff, 0xff, 0xff, 0x7f], OkWith(minus1));
    check(&parse, &[0xff, 0xff, 0xff, 0xff, 0x7f], OkWith(minus1));

    if bits == 32 {
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x00], Failed);
    } else {
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], OkWith(0x7f));
        check(&parse, &[0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00], Failed);
    }

    check(&parse, &[0xff, 0xff, 0xff, 0x7f], OkWith(minus1));
    check(&parse, &[0b1_0011111, 0b1_0001111, 0b1_0000111, 0b0_0000011],
             OkWith(0b__0000011______0000111______0001111______0011111));
}

#[test]
fn test_parse_i32() {
    test_parse_iN(|inp| parse_i32(inp).map(|(i, x)| (i, x as u64)), 32);
}

#[test]
fn test_parse_i64() {
    test_parse_iN(parse_i64, 64);
}

#[test]
fn test_parse_f32() {
    let x: f32 = -1459.78965;
    let b = x.to_bits().to_le_bytes();

    check(&parse_f32, &b, OkWith(x));
}

#[test]
fn test_parse_f64() {
    let x: f64 = -1459.78965;
    let b = x.to_bits().to_le_bytes();

    check(&parse_f64, &b, OkWith(x));
}

#[test]
fn test_parse_name() {
    let s = "hello wörldß";

    check(&parse_name, &{
        let mut v = vec![s.len() as u8];
        v.extend(s.bytes());
        v
    }, OkWith(s.into()));

    check(&parse_name, &{
        let mut v = vec![s.len() as u8 - 1];
        v.extend(s.bytes());
        v
    }, Failed);

    check(&parse_name, &{
        let mut v = vec![s.len() as u8 + 1];
        v.extend(s.bytes());
        v
    }, Failed);

    check(&parse_name, &vec![5, 0xff, 0xff, 0xff, 0xff, 0xff], Failed);
}

#[test]
fn test_parse_result_valtype() {
    check(&parse_blocktype, &[0x7f], OkWith(ValType::I32.into()));
    check(&parse_blocktype, &[0x7e], OkWith(ValType::I64.into()));
    check(&parse_blocktype, &[0x7d], OkWith(ValType::F32.into()));
    check(&parse_blocktype, &[0x7c], OkWith(ValType::F64.into()));
    check(&parse_blocktype, &[0x40], OkWith(None.into()));
}

#[test]
fn test_parse_functype() {
    check(&parse_functype, &[0x60, 1, 0x7f, 2, 0x7e, 0x7c], OkWith(
        FuncType {
            args: wec![ValType::I32],
            results: wec![ValType::I64, ValType::F64],
        }
    ));
}

#[test]
fn test_parse_limits() {
    check(&parse_limits, &[0x00, 0x00], OkWith(
        Limits {
            min: 0x00,
            max: None,
        }
    ));
    check(&parse_limits, &[0x00, 0xff, 0x01], OkWith(
        Limits {
            min: 0xff,
            max: None,
        }
    ));
    check(&parse_limits, &[0x01, 0x00, 0x00], OkWith(
        Limits {
            min: 0x00,
            max: Some(0x00),
        }
    ));
    check(&parse_limits, &[0x01, 0xff, 0x01, 0xff, 0x01], OkWith(
        Limits {
            min: 0xff,
            max: Some(0xff),
        }
    ));
}

#[test]
fn test_parse_customsecs() {
    check(&parse_customsec, &[
        0, 6, 3, 97, 98, 99, 0xff, 0xee,
    ], OkWith(
        CustomSection { name: "abc".into(), bytes: vec![0xff, 0xee] }
    ));

    check(&parse_customsec, &[
        0, 6, 3, 97, 98, 99, 0xff,
    ], Failed);

    check(&parse_customsec, &[
        0, 4, 3, 97, 98,
    ], Failed);

    check(&parse_customsecs, &[
        0, 6, 3, b'a', b'b', b'c', 0xff, 0xee,
    ], OkWith(vec![
        CustomSection { name: "abc".into(), bytes: vec![0xff, 0xee] },
    ]));

    check(&parse_customsecs, &[
        0, 6, 3, b'a', b'b', b'c', 0xff, 0xee,
        0, 8, 4, b'd', b'e', b'f', b'g', 0xa0, 0xb1, 0xc2
    ], OkWith(vec![
        CustomSection { name: "abc".into(), bytes: vec![0xff, 0xee] },
        CustomSection { name: "defg".into(), bytes: vec![0xa0, 0xb1, 0xc2] },
    ]));
}

#[test]
fn test_parse_code() {
    check(&parse_code, &[
        4,    // 4 bytes
        1,    // 1 compressed locals
        2,    // 2x f32 types
        0x7D,
        0x0B, // empty expression
    ], OkWith(
        Code {
            locals: wec![ValType::F32, ValType::F32],
            body: Expr { body: vec![] },
        }
    ));

    check(&parse_code, &[
        6,    // 6 bytes
        1,    // 1 compressed locals
        2,    // 2x f32 types
        0x7D,
        0x0B, // empty expression
        0x00, // trash bytes
        0x00,
    ], Failed);

    check(&parse_code, &[
        8,    // 4 bytes
        2,    // 2 compressed locals
        2,    // 2x f32 types
        0x7D,
        1,    // 1x i32 types
        0x7F,
        0x00,
        0x47,
        0x0B, // empty expression
    ], OkWith(
        Code {
            locals: wec![ValType::F32, ValType::F32, ValType::I32],
            body: Expr { body: vec![Instr::Unreachable, Instr::I32Ne] },
        }
    ));
}
