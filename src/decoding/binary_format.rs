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

named!(pub parse_s32 <Inp, i32>, map!(apply!(parse_sN, 32), |x| x as i32));
named!(pub parse_s64 <Inp, i64>, apply!(parse_sN, 64));


#[cfg(test)]
#[path="tests_binary_format.rs"]
mod tests;
