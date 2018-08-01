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
macro_rules! parse_preamble {
    ($self:expr, $t:ty, $b:block) => (
        {
            $self.skip()?;
            let prev_positon = $self.position;
            let r = (|| -> Result<$t, ParserError> {
                $b
            })();
            if r.is_err() {
                $self.position = prev_positon;
            }
            r
        }
    )
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
            parse_preamble!($self, $t, $b)
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

    fn unparsed_one(&self) -> Result<char, ParserError> {
        if let Some(r) = self.input.get(self.position..).and_then(|x| x.chars().next()) {
            Ok(r)
        } else {
            self.error(UnexpectedEof)
        }
    }
    fn unparsedb_one(&self) -> Result<u8, ParserError> {
        if let Some(r) = self.input.get(self.position..).and_then(|x| x.as_bytes().get(0).cloned()) {
             Ok(r)
        } else {
            self.error(UnexpectedEof)
        }
    }
    fn expect(&mut self, c: char) -> Result<char, ParserError> {
        let d = self.unparsed_one()?;
        if d != c {
            self.error(Estr("UnexpectedSymbol"))
        } else {
            self.step(c.len_utf8());
            Ok(c)
        }
    }
    fn expect_any(&mut self) -> Result<char, ParserError> {
        let c = self.unparsed_one()?;
        self.step(c.len_utf8());
        Ok(c)
    }

    fn step(&mut self, n: usize) {
        self.position += n;
    }
    fn unstep(&mut self, n: usize) {
        self.position -= n;
    }
    fn is_token_end_at(&self, i: usize) -> bool {
        match self.unparsedb()[i..] {
            | []
            | [b' ',    ..]
            | [b'\x09', ..]
            | [b'\x0a', ..]
            | [b'\x0d', ..]
            | [b';', b';', ..]
            | [b'(', b';', ..]
            | [b'(']
            | [b')'] => true,
            _ => false,
        }
    }
    fn is_token_end(&self) -> bool {
        self.is_token_end_at(0)
    }
}

const MAX_VEC_LEN: usize = (1 << 32) - 1;

macro_rules! token_enum {
    ($name:ident { $( $e:ident $(( $($et:ty),* ))? : $s:expr ),* $(,)? }) => (
        #[derive(PartialEq, Debug)]
        pub enum $name {
            $(
                $e $(( $($et),* ))?
            ),*
        }
        impl $name {
            /*
            fn to_str(&self) -> &'static str {
                match *self {
                    $(
                        $name::$e => $s,
                    )*
                }
            }
            */
            fn from_str(s: &str) -> Option<Self> {
                match s {
                    $(
                        $s => Some($name::$e),
                    )*
                    _ => None,
                }
            }
        }
    )
}

token_enum!(Token {
    LPAREN : "(",
    RPAREN : ")",
});

token_enum!(Keyword {
    I32    : "i32",
    I64    : "i64",
    F32    : "f32",
    F64    : "f64",
    Func   : "func",
    Param  : "param",
    Result : "result",
});

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

    parse_fun!(:raw raw_digit(self) -> u8 {
        let r = if let Ok(c) = self.unparsedb_one() {
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
        let r = if let Ok(c) = self.unparsedb_one() {
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
                if let Ok('_') = self.unparsed_one() {
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
                if let Ok('_') = self.unparsed_one() {
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
                p.is_token_end() || check_exp(p) || p.unparsed_one().ok() == Some('.')
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
    parse_fun!(parse_string(self) -> Vec<u8> {
        self.expect('\"')?;
        let len = self.unparsed().find('"')
            .ok_or(()).or_else(|_| self.error(Estr("UnterminatedStringLiteral")))?;
        let mut s = Vec::with_capacity(len);
        let push = |s: &mut Vec<u8>, c: char| {
            let mut buf = [0; 4];
            s.extend(c.encode_utf8(&mut buf).bytes());
        };

        while let Ok(c) = self.expect_any() {
            match c {
                '"' => {
                    if s.len() <= MAX_VEC_LEN {
                        return Ok(s.into());
                    } else {
                        return self.error(Estr("StringTooLong"));
                    }
                }
                '\\' => {
                    match self.expect_any()? {
                        't' => { push(&mut s, '\t'); }
                        'n' => { push(&mut s, '\n'); }
                        'r' => { push(&mut s, '\r'); }
                        '\"' => { push(&mut s, '\"'); }
                        '\'' => { push(&mut s, '\''); }
                        '\\' => { push(&mut s, '\\'); }
                        'u' => {
                            self.expect('{')?;

                            let mut n: u32 = 0;

                            let is_token_end = |p: &Parser| {
                                p.is_token_end() || p.unparsed_one().ok() == Some('}')
                            };
                            self.raw_digits_loop(true, is_token_end, |d, radix, self_| {
                                n *= radix as u32;
                                n += d as u32;
                                if n >= (0x110000) {
                                    self_.error(Estr("UnicodeCharOutOfBounds"))?;
                                }
                                Ok(())
                            })?;

                            if !(n < 0xd800 || (0xe000 <= n && n < 0x110000)) {
                                self.error(Estr("UnicodeCharOutOfBounds"))?;
                            }

                            push(&mut s, ::std::char::from_u32(n).unwrap());

                            self.expect('}')?;
                        }
                        _ => {
                            self.unstep(1);
                            let n = self.raw_hexdigit()?;
                            let m = self.raw_hexdigit()?;
                            s.push(16 * n + m);
                        }
                    }
                }
                c if c >= '\u{20}' && c != '\u{7f}' => {
                    push(&mut s, c);
                }
                _ => break,
            }
        }

        self.error(Estr("StringNotRecognized"))
    });
    parse_fun!(parse_name(self) -> String {
        let s = self.parse_string()?;
        if let Ok(s) = String::from_utf8(s) {
            Ok(s)
        } else {
            self.error(Estr("NameIsNotUtf8"))
        }
    });
    parse_fun!(parse_id_char(self) -> char {
        let c = self.expect_any()?;
        match c {
            | '0' ..= '9'
            | 'A' ..= 'Z'
            | 'a' ..= 'z'
            | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/'
            | ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~'
            => {
                Ok(c)
            }
            _ => {
                self.error(Estr("InvalidIdChar"))?
            }
        }
    });
    parse_fun!(parse_id(self) -> String {
        self.expect('$')?;
        let mut s = String::with_capacity(2);
        s.push('$');

        loop {
            s.push(self.parse_id_char()?);
            if self.is_token_end() {
                break;
            }
        }

        Ok(s)
    });
    parse_fun!(parse_keyword(self) -> Keyword {
        let start = self.position;
        let mut end = start;

        loop {
            let c = self.parse_id_char()?;
            println!("look at {}", c);
            end += c.len_utf8();
            if self.is_token_end() {
                break;
            }
        }

        let slice = &self.input[start..end];
        println!("look at {}", slice);

        if let Some(k) = Keyword::from_str(slice) {
            Ok(k)
        } else {
            self.error(Estr("NotAKeyword"))
        }
    });
    parse_fun!(expect_keyword(self, kw: Keyword) -> () {
        if self.parse_keyword()? == kw {
            Ok(())
        } else {
            self.error(Estr("UnexpectedKeyword"))
        }
    });
    parse_fun!(expect_token(self, tk: Token) -> () {
        let c = self.expect_any()?;
        match (c, tk) {
            ('(', Token::LPAREN) => Ok(()),
            (')', Token::RPAREN) => Ok(()),
            _ => self.error(Estr("UnexpectedToken"))?
        }
    });

    fn parse_parenthized<F: FnMut(&mut Self) -> Result<T, ParserError>, T>(&mut self, kw: Keyword, mut f: F) -> Result<T, ParserError> {
        parse_preamble!(self, T, {
            self.expect_token(Token::LPAREN)?;
            self.expect_keyword(kw)?;
            let r = f(self)?;
            self.expect_token(Token::RPAREN)?;
            Ok(r)
        })
    }

    fn parse_optional<F: FnMut(&mut Self) -> Result<T, ParserError>, T>(&mut self, mut f: F) -> Result<Option<T>, ParserError> {
        parse_preamble!(self, Option<T>, {
            if let Ok(r) = f(self) {
                Ok(Some(r))
            } else {
                Ok(None)
            }
        })
    }

    fn parse_vec<F: FnMut(&mut Self) -> Result<T, ParserError>, T>(&mut self, mut f: F) -> Result<Vec<T>, ParserError> {
        parse_preamble!(self, Vec<T>, {
            let mut v = vec![];
            let mut i: usize = 0;
            while let Ok(r) = f(self) {
                assert!(i < MAX_VEC_LEN);
                v.push(r);
                i += 1;
            }
            Ok(v)
        })
    }
}

use structure::types::ValType;
use structure::types::ResultType;
use structure::types::FuncType;
impl<'a> Parser<'a> {
    parse_fun!(parse_valtype(self) -> ValType {
        let r = match self.parse_keyword()? {
            Keyword::I32 => ValType::I32,
            Keyword::I64 => ValType::I64,
            Keyword::F32 => ValType::F32,
            Keyword::F64 => ValType::F64,
            _ => self.error(Estr("UnexpectedValType"))?
        };
        Ok(r)
    });

    parse_fun!(parse_resulttype(self) -> ResultType {
        self.parse_optional(|s| {
            s.parse_result()
        })
    });

    parse_fun!(parse_result(self) -> ValType {
        self.parse_parenthized(Keyword::Result, |s| {
            s.parse_valtype()
        })
    });
    parse_fun!(parse_param(self) -> ValType {
        self.parse_parenthized(Keyword::Param, |s| {
            s.parse_optional(|s| s.parse_id())?;
            s.parse_valtype()
        })
    });
    parse_fun!(parse_functype(self) -> FuncType {
        self.parse_parenthized(Keyword::Func, |s| {
            let args = s.parse_vec(|s| {
                s.parse_param()
            })?;
            let results = s.parse_vec(|s| {
                s.parse_result()
            })?;
            Ok(FuncType {
                args,
                results,
            })
        })
    });
}

#[derive(Debug, PartialEq)]
pub enum Sexp {
    Keyword(Keyword),
    U32(u32),
    U64(u64),
    S32(i32),
    S64(i64),
    F32(u64),
    F64(u64),
    String(Vec<u8>),
    Id(String),
    Parens(Vec<Sexp>),
}

impl<'a> Parser<'a> {
    parse_fun!(parse_sexpr(self) -> Sexp {
        println!("Kw?");
        if let Ok(kw) = self.parse_keyword() {
            return Ok(Sexp::Keyword(kw))
        }
        println!("Un?");
        if let Ok(v) = self.parse_un(32) {
            return Ok(Sexp::U32(v as u32))
        }
        if let Ok(v) = self.parse_un(64) {
            return Ok(Sexp::U64(v))
        }
        println!("Sn?");
        if let Ok(v) = self.parse_sn(32) {
            return Ok(Sexp::S32(v as i32))
        }
        if let Ok(v) = self.parse_sn(64) {
            return Ok(Sexp::S64(v))
        }
        println!("Fn?");
        if let Ok(v) = self.parse_fn(32) {
            return Ok(Sexp::F32(v))
        }
        if let Ok(v) = self.parse_fn(64) {
            return Ok(Sexp::F64(v))
        }
        println!("String?");
        if let Ok(s) = self.parse_string() {
            return Ok(Sexp::String(s))
        }
        println!("Id?");
        if let Ok(s) = self.parse_id() {
            return Ok(Sexp::Id(s))
        }

        println!("(?");
        if let Ok(_) = self.expect_token(Token::LPAREN) {
            println!("(");
            let mut v = vec![];

            println!("sexpr?");
            while let Ok(x) = self.parse_sexpr() {
                v.push(x);
                println!("sexpr");
            }

            println!(")?");
            self.expect_token(Token::RPAREN)?;
            println!(")");

            return Ok(Sexp::Parens(v))
        }

        println!("");

        // TODO: correct reserved handling
        self.error(Estr("Reserved"))
    });
}

use structure::modules::Module;
pub fn from_text_format(_b: &str) -> Result<Module, ParserError> {
    unimplemented!()
}

#[cfg(test)]
#[path="tests_text_format.rs"]
mod tests;
