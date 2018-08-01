use nom::IResult;
use nom::types::CompleteByteSlice;

type Inp<'a> = CompleteByteSlice<'a>;

fn parse_vec<'a, F, B>(input: Inp<'a>, mut parse_b: F) -> IResult<Inp<'a>, Vec<B>>
    where F: FnMut(Inp<'a>) -> IResult<Inp<'a>, B>
{
    do_parse!(input,
        n: apply!(parse_uN, 32)
        >> res: many_m_n!(n as usize, n as usize, parse_b)
        >> (res)
    )
}

named!(parse_byte <Inp, u8>, map!(take!(1), |n| n[0]));

named_args!(parse_uN(N: u32) <Inp, u64>, alt!(
    verify!(map!(parse_byte, |n| n as u64), |n| (n < (1 << 7)) && ((n as u128) < (1 << N)))
    | do_parse!(
        n: verify!(map!(parse_byte, |n| n as u64), |n| (n >= (1 << 7)) && (N > 7))
        >> m: apply!(parse_uN, N - 7)
        >> ((1 << 7) * m + (n - (1 << 7)))
    )
));

named!(pub parse_u32 <Inp, u32>, map!(apply!(parse_uN, 32), |x| x as u32));
named!(pub parse_u64 <Inp, u64>, apply!(parse_uN, 64));



#[cfg(test)]
#[path="tests_binary_format.rs"]
mod tests;
