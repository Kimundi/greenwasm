use std::fmt;
use std::iter;

use super::*;

fn parser(s: &str) -> Parser {
    Parser {
        input: s,
        position: 0,
    }
}

fn check_case<T, F, P>(s: &str, f: &F, pred: &P, end_offset: usize)
    where T: fmt::Debug,
            F: Fn(&mut Parser) -> Result<T, ParserError>,
            P: Fn(&Result<T, ParserError>) -> bool,
{
    let mut p = parser(s);
    let r = f(&mut p);
    let r = r.and_then(|x| {
        let end_pos = s.len() - end_offset;
        if p.position != end_pos {
            p.error(Estr("DidNotParseToTheEnd"))
        } else {
            Ok(x)
        }
    });
    let validation = pred(&r);
    if !validation {
        if let Err(r) = r {
            let indent = iter::repeat(' ').take(r.position);
            let c: String = indent.chain(Some('^')).collect();
            panic!("Token stream:\n`{}`\n {}\n\nError: {:?}", s, c, r.error);
        } else {
            panic!("Token stream:\n`{}`\nResult: {:?}", s, r);
        }
    }
}

fn check<T, F, P>(s: &str, f: F, pred: P)
    where T: fmt::Debug,
            F: Fn(&mut Parser) -> Result<T, ParserError>,
            P: Fn(&Result<T, ParserError>) -> bool,
{
    check_case(s, &f, &pred, 0);
    check_case(&format!(" {}", s), &f, &pred, 0);
    check_case(&format!(" {} ", s), &f, &pred, 1);
    check_case(&format!("\n{}", s), &f, &pred, 0);
    check_case(&format!("\n{}\n", s), &f, &pred, 1);
    check_case(&format!("(;;){}", s), &f, &pred, 0);
    check_case(&format!("(;;){}(;;)", s), &f, &pred, 4);
    check_case(&format!("{};;", s), &f, &pred, 2);
}

fn is_ok_with<T: PartialEq>(v: T) -> impl Fn(&Result<T, ParserError>) -> bool {
    move |r: &Result<T, ParserError>| {
        if let Ok(q) = r {
            q == &v
        } else {
            false
        }
    }
}

#[test]
fn parse_u8() {
    parse_un(8);
}
#[test]
fn parse_u16() {
    parse_un(16);
}
#[test]
fn parse_u32() {
    parse_un(32);
}
#[test]
fn parse_u64() {
    parse_un(64);
}

fn parse_un(n: u32) {
    let parse_un = |p: &mut Parser| Parser::parse_un(p, n);

    check("", parse_un, Result::is_err);
    check("_", parse_un, Result::is_err);

    check("_", parse_un, Result::is_err);

    check("0", parse_un, is_ok_with(0));
    check("1", parse_un, is_ok_with(1));
    check("1_", parse_un, Result::is_err);

    check("02", parse_un, is_ok_with(2));
    check("12", parse_un, is_ok_with(12));
    check("1_2", parse_un, is_ok_with(12));
    check("1__2", parse_un, Result::is_err);

    check("0x", parse_un, Result::is_err);
    check("0x_", parse_un, Result::is_err);

    check("0x0", parse_un, is_ok_with(0));
    check("0x1", parse_un, is_ok_with(1));
    check("0x1_", parse_un, Result::is_err);

    check("0xa", parse_un, is_ok_with(10));
    check("0xa_", parse_un, Result::is_err);

    check("0xf", parse_un, is_ok_with(15));
    check("0xf_", parse_un, Result::is_err);

    check("0xA", parse_un, is_ok_with(10));
    check("0xA_", parse_un, Result::is_err);

    check("0xF", parse_un, is_ok_with(15));
    check("0xF_", parse_un, Result::is_err);

    check("0x02", parse_un, is_ok_with(2));
    check("0x12", parse_un, is_ok_with(18));
    check("0x1_2", parse_un, is_ok_with(18));
    check("0x1__2", parse_un, Result::is_err);

    let one_beyond = 1u128 << n;
    let max = one_beyond - 1;

    let one_beyond_ds = format!("{}", one_beyond);
    let max_ds = format!("{}", max);

    let one_beyond_hs = format!("0x{:x}", one_beyond);
    let max_hs = format!("0x{:x}", max);

    check(&one_beyond_ds, parse_un, Result::is_err);
    check(&max_ds, parse_un, is_ok_with(max as u64));

    check(&one_beyond_hs, parse_un, Result::is_err);
    check(&max_hs, parse_un, is_ok_with(max as u64));


}

#[test]
fn parse_s8() {
    parse_sn(8);
}
#[test]
fn parse_s16() {
    parse_sn(16);
}
#[test]
fn parse_s32() {
    parse_sn(32);
}
#[test]
fn parse_s64() {
    parse_sn(64);
}

fn parse_sn(n: u32) {
    let parse_sn = |p: &mut Parser| Parser::parse_sn(p, n);

    check("0", parse_sn, is_ok_with(0));
    check("-0", parse_sn, is_ok_with(0));
    check("+0", parse_sn, is_ok_with(0));

    check("- 0", parse_sn, Result::is_err);
    check("+ 0", parse_sn, Result::is_err);

    check("1", parse_sn, is_ok_with(1));
    check("-1", parse_sn, is_ok_with(-1));
    check("+1", parse_sn, is_ok_with(1));
}

#[test]
fn parse_i8() {
    parse_in(8);
}
#[test]
fn parse_i16() {
    parse_in(16);
}
#[test]
fn parse_i32() {
    parse_in(32);
}
#[test]
fn parse_i64() {
    parse_in(64);
}

fn parse_in(n: u32) {
    let parse_in = |p: &mut Parser| Parser::parse_in(p, n);

    check("1", parse_in, is_ok_with(1u64));
    check("+1", parse_in, is_ok_with(1u64));
    check("-1", parse_in, is_ok_with(-1i64 as u64));
}

#[test]
fn parse_f32() {
    parse_fn(32);
}
#[test]
fn parse_f64() {
    parse_fn(64);
}

fn parse_fn(n: u32) {
    let parse_fn = |p: &mut Parser| Parser::parse_fn(p, n);
    let ok = |v32: f32, v64: f64| {
        if n == 32 {
            is_ok_with(v32.to_bits() as u64)
        } else {
            is_ok_with(v64.to_bits())
        }
    };
    macro_rules! ok {
        ($e:expr) => (ok($e, $e))
    }

    check("1", parse_fn, ok!(1.0));
    check("+1", parse_fn, ok!(1.0));
    check("-1", parse_fn, ok!(-1.0));
    check("0x1", parse_fn, ok!(1.0));
    check("+0x1", parse_fn, ok!(1.0));
    check("-0x1", parse_fn, ok!(-1.0));

    check("10", parse_fn, ok!(10.0));
    check("+10", parse_fn, ok!(10.0));
    check("-10", parse_fn, ok!(-10.0));
    check("0x10", parse_fn, ok!(16.0));
    check("+0x10", parse_fn, ok!(16.0));
    check("-0x10", parse_fn, ok!(-16.0));

    check("10.", parse_fn, ok!(10.0));
    check("+10.", parse_fn, ok!(10.0));
    check("-10.", parse_fn, ok!(-10.0));
    check("0x10.", parse_fn, ok!(16.0));
    check("+0x10.", parse_fn, ok!(16.0));
    check("-0x10.", parse_fn, ok!(-16.0));

    check("10.5", parse_fn, ok!(10.5));
    check("+10.5", parse_fn, ok!(10.5));
    check("-10.5", parse_fn, ok!(-10.5));
    check("0x10.1", parse_fn, ok!(16.0625));
    check("+0x10.1", parse_fn, ok!(16.0625));
    check("-0x10.1", parse_fn, ok!(-16.0625));

    check("10e0", parse_fn, ok!(10.0));
    check("+10e0", parse_fn, ok!(10.0));
    check("-10e0", parse_fn, ok!(-10.0));
    check("0x10p0", parse_fn, ok!(16.0));
    check("+0x10p0", parse_fn, ok!(16.0));
    check("-0x10p0", parse_fn, ok!(-16.0));

    check("10e-0", parse_fn, ok!(10.0));
    check("+10e-0", parse_fn, ok!(10.0));
    check("-10e-0", parse_fn, ok!(-10.0));
    check("0x10p-0", parse_fn, ok!(16.0));
    check("+0x10p-0", parse_fn, ok!(16.0));
    check("-0x10p-0", parse_fn, ok!(-16.0));

    check("10e+0", parse_fn, ok!(10.0));
    check("+10e+0", parse_fn, ok!(10.0));
    check("-10e+0", parse_fn, ok!(-10.0));
    check("0x10p+0", parse_fn, ok!(16.0));
    check("+0x10p+0", parse_fn, ok!(16.0));
    check("-0x10p+0", parse_fn, ok!(-16.0));

    check("10.e0", parse_fn, ok!(10.0));
    check("+10.e0", parse_fn, ok!(10.0));
    check("-10.e0", parse_fn, ok!(-10.0));
    check("0x10.p0", parse_fn, ok!(16.0));
    check("+0x10.p0", parse_fn, ok!(16.0));
    check("-0x10.p0", parse_fn, ok!(-16.0));

    check("10.e-0", parse_fn, ok!(10.0));
    check("+10.e-0", parse_fn, ok!(10.0));
    check("-10.e-0", parse_fn, ok!(-10.0));
    check("0x10.p-0", parse_fn, ok!(16.0));
    check("+0x10.p-0", parse_fn, ok!(16.0));
    check("-0x10.p-0", parse_fn, ok!(-16.0));

    check("10.e+0", parse_fn, ok!(10.0));
    check("+10.e+0", parse_fn, ok!(10.0));
    check("-10.e+0", parse_fn, ok!(-10.0));
    check("0x10.p+0", parse_fn, ok!(16.0));
    check("+0x10.p+0", parse_fn, ok!(16.0));
    check("-0x10.p+0", parse_fn, ok!(-16.0));

    check("10.5", parse_fn, ok!(10.5));
    check("+10.5", parse_fn, ok!(10.5));
    check("-10.5", parse_fn, ok!(-10.5));
    check("0x10.1", parse_fn, ok!(16.0625));
    check("+0x10.1", parse_fn, ok!(16.0625));
    check("-0x10.1", parse_fn, ok!(-16.0625));

    check("10.e1", parse_fn, ok!(100.0));
    check("+10.e1", parse_fn, ok!(100.0));
    check("-10.e1", parse_fn, ok!(-100.0));
    check("0x10.p1", parse_fn, ok!(32.0));
    check("+0x10.p1", parse_fn, ok!(32.0));
    check("-0x10.p1", parse_fn, ok!(-32.0));

    check("10.e-1", parse_fn, ok!(1.0));
    check("+10.e-1", parse_fn, ok!(1.0));
    check("-10.e-1", parse_fn, ok!(-1.0));
    check("0x10.p-1", parse_fn, ok!(8.0));
    check("+0x10.p-1", parse_fn, ok!(8.0));
    check("-0x10.p-1", parse_fn, ok!(-8.0));

    check("10.e+1", parse_fn, ok!(100.0));
    check("+10.e+1", parse_fn, ok!(100.0));
    check("-10.e+1", parse_fn, ok!(-100.0));
    check("0x10.p+1", parse_fn, ok!(32.0));
    check("+0x10.p+1", parse_fn, ok!(32.0));
    check("-0x10.p+1", parse_fn, ok!(-32.0));

    check("+inf", parse_fn, ok!(1.0 / 0.0));
    check("-inf", parse_fn, ok!(-1.0 / 0.0));

    check("+nan", parse_fn, ok(
        f32::from_bits(0b0_11111111_10000000000000000000000),
        f64::from_bits(0b0_11111111111_1000000000000000000000000000000000000000000000000000),
    ));
    check("-nan", parse_fn, ok(
        f32::from_bits(0b1_11111111_10000000000000000000000),
        f64::from_bits(0b1_11111111111_1000000000000000000000000000000000000000000000000000),
    ));
    check("+nan:0xff", parse_fn, ok(
        f32::from_bits(0b0_11111111_00000000000000011111111),
        f64::from_bits(0b0_11111111111_0000000000000000000000000000000000000000000011111111),
    ));
    check("-nan:0xff", parse_fn, ok(
        f32::from_bits(0b1_11111111_00000000000000011111111),
        f64::from_bits(0b1_11111111111_0000000000000000000000000000000000000000000011111111),
    ));
}

#[test]
fn parse_string() {
    let parse_string = |p: &mut Parser| p.parse_string();
    let ok = |s: &str| is_ok_with(s.into());
    let okb = |s: &[u8]| is_ok_with(s.into());

    check(r#""hello""#, parse_string, ok("hello"));
    check(r#""hel\nlo""#, parse_string, ok("hel\nlo"));
    check(r#""hel\"lo""#, parse_string, ok("hel\"lo"));
    check(r#""hel\\lo""#, parse_string, ok("hel\\lo"));
    check(r#""hel\u{abc}lo""#, parse_string, ok("hel\u{abc}lo"));
    check(r#""\fe""#, parse_string, okb(b"\xfe"));
    check(r#""hel\felo""#, parse_string, okb(b"hel\xfelo"));

    check("\"0hel\x00lo\"", parse_string, Result::is_err);
    check(r#""hel\mlo""#, parse_string, Result::is_err);
    check(r#""hel\u{g}lo""#, parse_string, Result::is_err);
    check(r#""hel\u{g}lo"#, parse_string, Result::is_err);
    check(r#""hel\u{g"#, parse_string, Result::is_err);
    check(r#""\f""#, parse_string, Result::is_err);
    check(r#""\fg""#, parse_string, Result::is_err);
    check(r#""\gg""#, parse_string, Result::is_err);
}

#[test]
fn parse_name() {
    let parse_name = |p: &mut Parser| p.parse_name();

    check(r#""äöü""#, parse_name, Result::is_ok);
    check(r#""\ff""#, parse_name, Result::is_err);
}

#[test]
fn parse_id() {
    let parse_id = |p: &mut Parser| p.parse_id();

    check("$", parse_id, Result::is_err);
    check("$ ", parse_id, Result::is_err);
    check("$\"", parse_id, Result::is_err);
    check("$,", parse_id, Result::is_err);
    check("$;", parse_id, Result::is_err);
    check("$(", parse_id, Result::is_err);
    check("$)", parse_id, Result::is_err);
    check("${", parse_id, Result::is_err);
    check("$}", parse_id, Result::is_err);
    check("$[", parse_id, Result::is_err);
    check("$]", parse_id, Result::is_err);

    check("$0123456789", parse_id, is_ok_with("$0123456789".into()));
    check("$abcdefghijklmnopqrstuvwxyz", parse_id,
        is_ok_with("$abcdefghijklmnopqrstuvwxyz".into()));
    check("$ABCDEFGHIJKLMNOPQRSTUVWXYZ", parse_id,
        is_ok_with("$ABCDEFGHIJKLMNOPQRSTUVWXYZ".into()));
    check("$!#$%&'*+-./:<=>?@\\^_`|~", parse_id,
        is_ok_with("$!#$%&'*+-./:<=>?@\\^_`|~".into()));
}

#[test]
fn parse_valtype() {
    let parse_id = |p: &mut Parser| p.parse_valtype();

    check("asdf", parse_id, Result::is_err);
    check("i32", parse_id, is_ok_with(ValType::I32));
    check("i64", parse_id, is_ok_with(ValType::I64));
    check("f32", parse_id, is_ok_with(ValType::F32));
    check("f64", parse_id, is_ok_with(ValType::F64));
}

#[test]
fn parse_result() {
    let parse_id = |p: &mut Parser| p.parse_result();

    check("", parse_id, Result::is_err);
    check("( result i32 )", parse_id, is_ok_with((ValType::I32)));
    check("( result i64 )", parse_id, is_ok_with((ValType::I64)));
    check("( result f32 )", parse_id, is_ok_with((ValType::F32)));
    check("( result f64 )", parse_id, is_ok_with((ValType::F64)));
}

#[test]
fn parse_resulttype() {
    let parse_id = |p: &mut Parser| p.parse_resulttype();

    // TODO: figure out how to test
    //check("", parse_id, is_ok_with(None));
    check("( result i32 )", parse_id, is_ok_with(Some(ValType::I32)));
    check("( result i64 )", parse_id, is_ok_with(Some(ValType::I64)));
    check("( result f32 )", parse_id, is_ok_with(Some(ValType::F32)));
    check("( result f64 )", parse_id, is_ok_with(Some(ValType::F64)));
}

#[test]
fn parse_param() {
    let parse_id = |p: &mut Parser| p.parse_param();

    check("asdf", parse_id, Result::is_err);
    check("( param $test i32 )", parse_id, is_ok_with((ValType::I32)));
    check("( param i32 )", parse_id, is_ok_with((ValType::I32)));
}
