#[derive(Debug)]
pub enum ParserErrorEnum {
    UnbalancedBlockComment,
    UnexpectedEof,
    Unrecognized,
    Estr(&'static str),
}
use self::ParserErrorEnum::*;
#[derive(Debug)]
pub struct ParserError {
    position: usize,
    error: ParserErrorEnum,
}
pub struct Parser<'a> {
    input: &'a str,
    position: usize,
}
macro_rules! parse_fun {
    (:raw $name:ident($self:ident $(,$arg:ident: $at:ty)*) -> $t:ty $b:block) => (
        pub fn $name(&mut $self $(,$arg: $at)*) -> Result<$t, ParserError> {
            let r = (|| -> Result<$t, ParserError> {$b})();
            r
        }
    );
    ($name:ident($self:ident $(,$arg:ident: $at:ty)*) -> $t:ty $b:block) => (
        pub fn $name(&mut $self $(,$arg: $at)*) -> Result<$t, ParserError> {
            $self.skip()?;
            let prev_positon = $self.position;
            let r = (|| -> Result<$t, ParserError> {$b})();
            if r.is_err() {
                $self.position = prev_positon;
            }
            r
        }
    )
}

impl<'a> Parser<'a> {
    fn error<T>(&self, err: ParserErrorEnum) -> Result<T, ParserError> {
        Err(ParserError {
            position: self.position,
            error: err,
        })
    }
    fn unparsed(&self) -> &'a str { &self.input[self.position..] }
    fn unparsedb(&self) -> &'a [u8] { self.input[self.position..].as_bytes() }

    fn unparsed_one(&self) -> Option<char> { self.input[self.position..].chars().next() }
    fn unparsedb_one(&self) -> Option<u8> { self.input[self.position..].as_bytes().get(0).cloned() }

    fn step(&mut self, n: usize) {
        self.position += n;
    }
    fn is_token_end(&self) -> bool {
        match self.unparsedb() {
            | []
            | [b' ',    ..]
            | [b'\x09', ..]
            | [b'\x0a', ..]
            | [b'\x0d', ..]
            | [b';', b';', ..]
            | [b'(', b';', ..] => true,
            _ => false,
        }
    }
}

pub enum Keyword {}
trait ParseFloat: Copy + From<u8> + From<u16> {
    fn zero() -> Self;
    fn infinite() -> Self;
    fn signif() -> u32;
    fn nan(payload: u64) -> Self;
    fn mul(self, v: Self) -> Self;
    fn add(self, v: Self) -> Self;
    fn div(self, v: Self) -> Self;
    fn pow(self, v: Self) -> Self;
    fn neg(self) -> Self;
    fn is_inf(self) -> bool;
}
impl ParseFloat for f32 {
    fn zero() -> Self { 0.0 }
    fn infinite() -> Self { 1.0 / 0.0 }
    fn signif() -> u32 { 23 }
    fn nan(payload: u64) -> Self {
        let bits: u32 = Self::infinite().to_bits();
        let mask: u32 = (1u32 << Self::signif()) - 1;
        let bits = bits | (mask & (payload as u32));
        Self::from_bits(bits)
    }
    fn mul(self, v: Self) -> Self { self * v }
    fn add(self, v: Self) -> Self { self + v }
    fn div(self, v: Self) -> Self { self / v }
    fn pow(self, v: Self) -> Self { self.powf(v) }
    fn neg(self) -> Self { -self }
    fn is_inf(self) -> bool { self.is_infinite() }
}
impl ParseFloat for f64 {
    fn zero() -> Self { 0.0 }
    fn infinite() -> Self { 1.0 / 0.0 }
    fn signif() -> u32 { 52 }
    fn nan(payload: u64) -> Self {
        let bits: u64 = Self::infinite().to_bits();
        let mask: u64 = (1u64 << Self::signif()) - 1;
        let bits = bits | (mask & payload);
        Self::from_bits(bits)
    }
    fn mul(self, v: Self) -> Self { self * v }
    fn add(self, v: Self) -> Self { self + v }
    fn div(self, v: Self) -> Self { self / v }
    fn pow(self, v: Self) -> Self { self.powf(v) }
    fn neg(self) -> Self { -self }
    fn is_inf(self) -> bool { self.is_infinite() }
}

impl<'a> Parser<'a> {
    parse_fun!(:raw skip_block_comment(self) -> () {
        self.step(2);
        let mut nesting: usize = 1;

        loop {
            let up = self.unparsed().as_bytes().get(..2);
            match up {
                Some(b"(;") => {
                    self.step(2);
                    nesting += 1;
                }
                Some(b";)") => {
                    self.step(2);
                    nesting -= 1;
                    if nesting == 0 {
                        return Ok(());
                    }
                }
                Some(_) => {
                    self.step(1);
                }
                None => {
                    return self.error(UnbalancedBlockComment);
                }
            }
        }
    });

    parse_fun!(:raw skip(self) -> () {
        'outer: loop {
            let up = self.unparsedb();

            match up {
                  [b' ',    ..]
                | [b'\x09', ..]
                | [b'\x0a', ..]
                | [b'\x0d', ..]
                => {
                    self.step(1);
                    continue 'outer;
                },
                [b';', b';', ..] => {
                    self.step(2);
                    loop {
                        match self.unparsedb().get(0) {
                            Some(&b'\x0a') => {
                                self.step(1);
                                continue 'outer;
                            }
                            None => {
                                break 'outer;
                            }
                            _ => {
                                self.step(1);
                            }
                        }
                    }
                }
                [b'(', b';', ..] => {
                    self.skip_block_comment()?;
                }
                _ => break 'outer,
            }
        }

        Ok(())
    });

    /*
    parse_fun!(skip_peek(self) -> &'a str {
        self.skip()?;
        let start = self.cursor;
        'outer: loop {
            let up = self.unparsed().as_bytes();
            match up {
                  [b' ',       ..]
                | [b'\x09',    ..]
                | [b'\x0a',    ..]
                | [b'\x0d',    ..]
                | [b';', b';', ..]
                | [b'(', b';', ..]
                => {
                    break 'outer;
                }
                [] => {
                    self.error(UnexpectedEof)?;
                }
                _ => {
                }
            }
            self.cursor += 1;
        }
        let end = self.cursor;
        Ok(&self.input[start..end])
    });
    */

    /*
    parse_fun!(parse_token(self) -> Token<'a> {
        Err(())
        .or_else(|_| self.parse_keyword())
        .or_else(|_| self.parse_un_sn_fn())
        .or_else(|_| self.parse_string())
        .or_else(|_| self.parse_id())
        .or_else(|_| self.parse_parens())
        .or_else(|_| self.parse_reserved())
    });
    */

    /*
    parse_fun!(parse_keyword(self) -> Keyword {
        Ok(match self.skip_peek()? {
            _ => Err(self.error_(Estr("KeywordNotRecognized")))?,
        })
    });
    */

    /*
    parse_fun!(parse_un_sn_fn(self) -> Token<'a> {
        Ok(match self.skip_peek()? {
            "u8" => Token::U(N::N8),
            "u16" => Token::U(N::N16),
            "u32" => Token::U(N::N32),
            "u64" => Token::U(N::N64),
            "s8" => Token::S(N::N8),
            "s16" => Token::S(N::N16),
            "s32" => Token::S(N::N32),
            "s64" => Token::S(N::N64),
            "f32" => Token::F(N::N32),
            "f64" => Token::F(N::N64),

            _ => Err(self.error_(Estr("NumberNotRecognized")))?,
        })
    });
    */
    parse_fun!(:raw raw_digit(self) -> u8 {
        let r = if let Some(c) = self.unparsedb_one() {
            if c >= b'0' && c <= b'9' {
                c - b'0'
            } else {
                self.error(Estr("NotADecDigit"))?
            }
        } else {
            self.error(Estr("NotADecDigit"))?
        };
        self.step(1);
        Ok(r)
    });
    parse_fun!(:raw raw_hexdigit(self) -> u8 {
        let r = if let Some(c) = self.unparsedb_one() {
            if c >= b'0' && c <= b'9' {
                c - b'0'
            } else if c >= b'a' && c <= b'f' {
                c - b'a' + 10
            } else if c >= b'A' && c <= b'F' {
                c - b'A' + 10
            } else {
                self.error(Estr("NotAHexDigit"))?
            }
        } else {
            self.error(Estr("NotAHexDigit"))?
        };
        self.step(1);
        Ok(r)
    });
    fn raw_digits_loop<F, G>(&mut self, hex: bool, mut g: G, mut f: F) -> Result<(), ParserError>
        where F: FnMut(u8, u8, &mut Parser) -> Result<(), ParserError>,
              G: FnMut(&Parser) -> bool,
    {
        if !hex {
            loop {
                if g(self) {
                    self.error(Estr("NotADecDigit"))?;
                }
                f(self.raw_digit()?, 10, self)?;
                if g(self) {
                    break;
                }
                if let Some('_') = self.unparsed_one() {
                    self.step(1);
                }
            }
        } else {
            loop {
                if g(self) {
                    self.error(Estr("NotAHexDigit"))?;
                }
                f(self.raw_hexdigit()?, 16, self)?;
                if g(self) {
                    break;
                }
                if let Some('_') = self.unparsed_one() {
                    self.step(1);
                }
            }
        }
        Ok(())
    }
    parse_fun!(:raw raw_parse_un(self, bits: u32) -> u64 {
        let hex = self.unparsed().starts_with("0x");
        if hex {
            self.step(2);
        }

        let mut n: u128 = 0;

        let is_token_end = |p: &Parser| p.is_token_end();
        self.raw_digits_loop(hex, is_token_end, |d, radix, self_| {
            n *= radix as u128;
            n += d as u128;
            if n >= (1u128 << bits) {
                self_.error(Estr("IntegerOutOfBounds"))?;
            }
            Ok(())
        })?;

        Ok(n as u64)
    });

    parse_fun!(parse_un(self, bits: u32) -> u64 {
        self.raw_parse_un(bits)
    });

    parse_fun!(parse_sn(self, bits: u32) -> i64 {
        let positive = {
            let s = self.unparsed();
            if s.starts_with('+') {
                self.step(1);
                true
            } else if s.starts_with('-') {
                self.step(1);
                false
            } else {
                true
            }
        };

        let unsigned_n = self.raw_parse_un(64)?;
        let n: i64;

        if positive {
            if (unsigned_n as u128) >= (1u128 << (bits - 1)) {
                self.error(Estr("IntegerOutOfBounds"))?;
            }
            n = unsigned_n as i64;
        } else {
            if (unsigned_n as u128) > (1u128 << (bits - 1)) {
                self.error(Estr("IntegerOutOfBounds"))?;
            }
            n = -(unsigned_n as i64);
        }

        Ok(n)
    });
    parse_fun!(parse_in(self, bits: u32) -> u64 {
        Err(())
        .or_else(|_| self.parse_un(bits))
        .or_else(|_| self.parse_sn(bits).map(|x| x as u64))
    });
    fn raw_parse_f_num<T: ParseFloat>(&mut self) -> Result<T, ParserError> {
        let hex = self.unparsed().starts_with("0x");
        if hex {
            self.step(2);
        }

        let mut n = T::zero();

        let check_exp = |s: &Parser| {
            let r = (!hex && s.unparsed().starts_with(&['E', 'e'][..]))
                  || (hex && s.unparsed().starts_with(&['P', 'p'][..]));
            r
        };

        {
            let is_token_end = |p: &Parser| {
                p.is_token_end() || check_exp(p) || p.unparsed_one() == Some('.')
            };
            self.raw_digits_loop(hex, is_token_end, |d, radix, self_| {
                n = n.mul(T::from(radix));
                n = n.add(T::from(d));
                if n.is_inf() {
                    self_.error(Estr("FloatOutOfBounds"))?;
                }
                Ok(())
            })?;
        }

        if self.unparsed().starts_with(".") {
            self.step(1);

            if !self.is_token_end() && !check_exp(self) {
                let mut frac_mult = T::from(1u8);
                let is_token_end = |p: &Parser| {
                    p.is_token_end() || check_exp(p)
                };
                self.raw_digits_loop(hex, is_token_end, |d, radix, _| {
                    frac_mult = frac_mult.div(T::from(radix));
                    n = n.add(frac_mult.mul(T::from(d)));
                    Ok(())
                })?;
            }
        }

        if check_exp(self) {
            self.step(1);
            let exp_positive = {
                let s = self.unparsed();
                if s.starts_with('+') {
                    self.step(1);
                    true
                } else if s.starts_with('-') {
                    self.step(1);
                    false
                } else {
                    true
                }
            };
            let mut exp_n: u32 = 0;

            let is_token_end = |p: &Parser| p.is_token_end();
            self.raw_digits_loop(false, is_token_end, |d, radix, self_| {
                exp_n *= radix as u32;
                exp_n += d as u32;
                if exp_n >= (1u32 << 16) {
                    self_.error(Estr("FloatExpOutOfBounds"))?;
                }
                Ok(())
            })?;

            let exp_n = T::from(exp_n as u16);
            let exp_n = if exp_positive { exp_n } else { exp_n.neg() };

            let base = if hex {
                T::from(2u8)
            } else {
                T::from(10u8)
            };
            n = n.mul(base.pow(exp_n));
            if n.is_inf() {
                self.error(Estr("FloatOutOfBounds"))?;
            }
        }

        Ok(n)
    }
    fn raw_parse_f<T: ParseFloat>(&mut self) -> Result<T, ParserError> {
        let positive = {
            let s = self.unparsed();
            if s.starts_with('+') {
                self.step(1);
                true
            } else if s.starts_with('-') {
                self.step(1);
                false
            } else {
                true
            }
        };

        let n = if self.unparsed().starts_with("inf") {
            self.step(3);
            T::infinite()
        } else if self.unparsed().starts_with("nan") {
            self.step(3);

            let payload = if self.unparsed().starts_with(":0x") {
                self.step(3);
                let mut payload = 0;
                let is_token_end = |p: &Parser| p.is_token_end();
                self.raw_digits_loop(true, is_token_end, |d, radix, self_| {
                    payload *= radix as u64;
                    payload += d as u64;
                    if payload >= (1u64 << T::signif()) {
                        self_.error(Estr("FloatNanPayloadOutOfBounds"))?;
                    }
                    Ok(())
                })?;

                if !(1 <= payload && payload < (1 << T::signif())) {
                    self.error(Estr("FloatNanPayloadOutOfBounds"))?;
                }

                payload
            } else {
                1u64 << (T::signif() - 1)
            };

            T::nan(payload)
        } else {
            self.raw_parse_f_num::<T>()?
        };

        Ok(if positive {
            n
        } else {
            n.neg()
        })
    }
    parse_fun!(parse_fn(self, bits: u32) -> u64 {
        Ok(if bits == 32 {
            self.raw_parse_f::<f32>()?.to_bits() as u64
        } else if bits == 64 {
            self.raw_parse_f::<f64>()?.to_bits()
        } else {
            self.error(Estr("UnssupportedFloatWidth"))?
        })
    });
    parse_fun!(parse_string(self) -> &'a str {

        self.error(Estr("StringNotRecognized"))
    });
    parse_fun!(parse_id(self) -> &'a str {

        self.error(Estr("IdNotRecognized"))
    });
    parse_fun!(parse_parens(self) -> () {

        self.error(Estr("ParensNotRecognized"))
    });
    parse_fun!(parse_reserved(self) -> &'a str {

        self.error(Estr("Reserved"))
    });
}

use structure::modules::Module;
pub fn from_text_format(_b: &str) -> Result<Module, ParserError> {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parser(s: &str) -> Parser {
        Parser {
            input: s,
            position: 0,
        }
    }

    fn check_case<T, F, P>(s: &str, f: &F, pred: &P, end_offset: usize)
        where T: ::std::fmt::Debug,
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
                let indent = ::std::iter::repeat(' ').take(r.position);
                let c: String = indent.chain(Some('^')).collect();
                panic!("Token stream:\n`{}`\n {}\n\nError: {:?}", s, c, r.error);
            } else {
                panic!("Token stream:\n`{}`\nResult: {:?}", s, r);
            }
        }
    }

    fn check<T, F, P>(s: &str, f: F, pred: P)
        where T: ::std::fmt::Debug,
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

}
