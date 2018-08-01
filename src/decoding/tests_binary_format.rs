use super::*;
use std::fmt::Debug;

enum CheckRes<T> {
    Failed,
    OkWith(T),
}
use self::CheckRes::*;

fn check<'a, T, F>(mut parse: F, input: &'a [u8], res: CheckRes<T>)
    where T: Debug + PartialEq,
          F: Fn(Inp<'a>) -> IResult<Inp<'a>, T>,
{
    match parse(CompleteByteSlice(input)) {
        Ok((CompleteByteSlice(&[]), is)) => {
            if let OkWith(should) = res {
                assert_eq!(is, should);
            } else {
                panic!("Should have failed, but parsed with {:?}", is);
            }
        }
        Ok((CompleteByteSlice(x), is)) => {
            panic!("Accepted without parsing all input. Remaining: {:?}, result: {:?}", x, is);
        }
        Err(x) => {
            if let OkWith(should) = res {
                panic!("Should have parsed with {:?}, but failed with {:?}", should, x);
            }
        }
    }
}

#[test]
fn test_parse_u32() {
    test_parse_uN(|inp| parse_u32(inp).map(|(i, x)| (i, x as u64)), 32);
}

#[test]
fn test_parse_u64() {
    test_parse_uN(parse_u64, 64);
}

fn test_parse_uN<F>(parse: F, bits: u32)
    where F: Fn(Inp) -> IResult<Inp, u64>,
{
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
