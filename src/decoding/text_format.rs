#[derive(Debug)]
enum ParserErrorEnum {
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
struct Parser<'a> {
    input: &'a str,
    position: usize,
    commited_position: usize,
}
struct SkipWhitespace;
struct NoScipWhitespace;
macro_rules! parse_fun {
    (:raw $name:ident($self:ident $(,$arg:ident: $at:ty)*) -> $t:ty $b:block) => (
        fn $name(&mut $self $(,$arg: $at)*) -> Result<$t, ParserError> {
            let r = $b;
            $self.reset_position();
            r
        }
    );
    ($name:ident($self:ident $(,$arg:ident: $at:ty)*) -> $t:ty $b:block) => (
        fn $name(&mut $self $(,$arg: $at)*) -> Result<$t, ParserError> {
            $self.skip();
            let r = $b;
            $self.reset_position();
            r
        }
    )
}

impl<'a> Parser<'a> {
    fn error_(&self, err: ParserErrorEnum) -> ParserError {
        ParserError {
            position: self.commited_position,
            error: err,
        }
    }
    fn error(&self, err: ParserErrorEnum) -> Result<(), ParserError> {
        Err(self.error_(err))
    }
    fn unparsed(&self) -> &'a str { &self.input[self.position..] }
    fn unparsedb(&self) -> &'a [u8] { self.input[self.position..].as_bytes() }

    fn unparsed_one(&self) -> Option<char> { self.input[self.position..].chars().next() }
    fn unparsedb_one(&self) -> Option<u8> { self.input[self.position..].as_bytes().get(0).cloned() }

    fn commit(&mut self) {
        self.commited_position = self.position;
    }
    fn reset_position(&mut self) {
        self.position = self.commited_position;
    }
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
enum Keyword {
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
                        self.commit();
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
                    self.skip_block_comment();
                }
                _ => break 'outer,
            }
        }

        self.commit();
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
    parse_fun!(:raw raw_parse_un(self, bits: u32) -> u64 {
        let hex = self.unparsed().starts_with("0x");
        if hex {
            self.step(2);
        }

        let mut n: u128 = 0;

        if !hex {
            loop {
                if self.is_token_end() {
                    self.error(Estr("NotADecDigit"))?;
                }
                let c = self.unparsed_one().unwrap();
                if c >= '0' && c <= '9' {
                    n *= 10;
                    n += (c as u128) - ('0' as u128);
                } else {
                    self.error(Estr("NotADecDigit"))?;
                }
                self.step(1);
                if self.is_token_end() {
                    break;
                }
                if let Some('_') = self.unparsed_one() {
                    self.step(1);
                }
                if n >= (1u128 << bits) {
                    self.error(Estr("IntegerOutOfBounds"))?;
                }
            }
        } else {
            loop {
                if self.is_token_end() {
                    self.error(Estr("NotAHexDigit"))?;
                }
                let c = self.unparsed_one().unwrap();
                if c >= '0' && c <= '9' {
                    n *= 16;
                    n += (c as u128) - ('0' as u128);
                } else if c >= 'a' && c <= 'f' {
                    n *= 16;
                    n += (c as u128) - ('a' as u128) + 10;
                } else if c >= 'A' && c <= 'F' {
                    n *= 16;
                    n += (c as u128) - ('A' as u128) + 10;
                } else {
                    self.error(Estr("NotAHexDigit"))?;
                }
                self.step(1);
                if self.is_token_end() {
                    break;
                }
                if let Some('_') = self.unparsed_one() {
                    self.step(1);
                }
                if n >= (1u128 << bits) {
                    self.error(Estr("IntegerOutOfBounds"))?;
                }
            }
        }

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
    parse_fun!(parse_string(self) -> &'a str {

        Err(self.error_(Estr("StringNotRecognized")))
    });
    parse_fun!(parse_id(self) -> &'a str {

        Err(self.error_(Estr("IdNotRecognized")))
    });
    parse_fun!(parse_parens(self) -> () {

        Err(self.error_(Estr("ParensNotRecognized")))
    });
    parse_fun!(parse_reserved(self) -> &'a str {

        Err(self.error_(Estr("Reserved")))
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
            commited_position: 0,
        }
    }

    fn check_case<T, F, P>(s: &str, f: &F, pred: &P)
        where T: ::std::fmt::Debug,
              F: Fn(&mut Parser) -> Result<T, ParserError>,
              P: Fn(&Result<T, ParserError>) -> bool,
    {
        let mut p = parser(s);
        let r = f(&mut p);
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
        check_case(s, &f, &pred);
        check_case(&format!(" {}", s), &f, &pred);
        check_case(&format!(" {} ", s), &f, &pred);
        check_case(&format!("\n{}", s), &f, &pred);
        check_case(&format!("\n{}\n", s), &f, &pred);
        check_case(&format!("(;;){}", s), &f, &pred);
        check_case(&format!("(;;){}(;;)", s), &f, &pred);
        check_case(&format!("{};;", s), &f, &pred);
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

}
