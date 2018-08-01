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
    let b = x.to_bits().to_le().to_bytes();

    check(&parse_f32, &b, OkWith(x));
}

#[test]
fn test_parse_f64() {
    let x: f64 = -1459.78965;
    let b = x.to_bits().to_le().to_bytes();

    check(&parse_f64, &b, OkWith(x));
}
