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
    ($(:$raw:ident)? $name:ident($self:ident $(,$arg:ident: $at:ty)*) -> $t:ty $b:block) => (
        fn $name(&mut $self $(,$arg: $at)*) -> Result<$t, ParserError> {
            $(
                $self.$raw();
            )?
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

    fn commit(&mut self) {
        self.commited_position = self.position;
    }
    fn reset_position(&mut self) {
        self.position = self.commited_position;
    }
    fn step(&mut self, n: usize) {
        self.position += n;
    }
}
enum Keyword {
}

impl<'a> Parser<'a> {
    parse_fun!(skip_block_comment(self) -> () {
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

    parse_fun!(skip(self) -> () {
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
    parse_fun!(:skip raw_parse_un(self, bits: u32) -> u64 {
        let mut s = self.unparsed();

        let hex = s.starts_with("0x");
        if hex {
            self.step(2);
            s = self.unparsed();
        }

        let mut n: u64 = 0;
        let mut allow_underscore = false;

        if !hex {
            while let Some(c) = s.chars().next() {
                if c >= '0' && c <= '9' {
                    n = n.checked_mul(10).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    n = n.checked_add((c as u64) - ('0' as u64)).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    allow_underscore = true;
                } else if allow_underscore && c == '_' {
                    allow_underscore = false;
                } else {
                    self.error(Estr("NotADecDigit"))?;
                }
                self.step(1);
                s = &s[1..];
            }
        } else {
            while let Some(c) = s.chars().next() {
                if c >= '0' && c <= '9' {
                    n = n.checked_mul(16).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    n = n.checked_add((c as u64) - ('0' as u64)).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    allow_underscore = true;
                } else if c >= 'a' && c <= 'f' {
                    n = n.checked_mul(16).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    n = n.checked_add((c as u64) - ('a' as u64) + 10).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    allow_underscore = true;
                } else if c >= 'A' && c <= 'F' {
                    n = n.checked_mul(16).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    n = n.checked_add((c as u64) - ('A' as u64) + 10).ok_or(self.error_(Estr("IntegerOutOfBounds")))?;
                    allow_underscore = true;
                } else if allow_underscore && c == '_' {
                    allow_underscore = false;
                } else {
                    self.error(Estr("NotAHexDigit"))?;
                }
                self.step(1);
                s = &s[1..];
            }
        }

        if (n as u128) >= (1u128 << bits) {
            self.error(Estr("IntegerOutOfBounds"))?;
        }

        Ok(n)
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
                return Err(self.error_(Estr("IntegerNotRecognized")));
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
        .or_else(|_| self.parse_sn(bits).map(|x| x as u64))
        .or_else(|_| self.parse_un(bits))
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

    struct Check {

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
            panic!("Token stream: `{}`, Result: {:?}", s, r);
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
    fn parse_un() {
        let parse_un8 = |x: &mut Parser| Parser::parse_un(x, 8);
        let parse_un16 = |x: &mut Parser| Parser::parse_un(x, 16);
        let parse_un32 = |x: &mut Parser| Parser::parse_un(x, 32);
        let parse_un64 = |x: &mut Parser| Parser::parse_un(x, 64);

        check("0", parse_un8, is_ok_with(0));
        check("0", parse_un16, is_ok_with(0));
        check("0", parse_un32, is_ok_with(0));
        check("0", parse_un64, is_ok_with(0));
    }
}
