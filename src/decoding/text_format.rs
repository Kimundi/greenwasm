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
    cursor: usize,
}
macro_rules! token_parse {
    ($name:ident($self:ident $(,$arg:ident: $at:ty)*) -> $t:ty $b:block) => (
        fn $name(&mut $self $(,$arg: $at)*) -> Result<$t, ParserError> $b
    )
}
impl<'a> Parser<'a> {
    fn error_(&self, err: ParserErrorEnum) -> ParserError {
        ParserError {
            position: self.cursor,
            error: err,
        }
    }
    fn error(&self, err: ParserErrorEnum) -> Result<(), ParserError> {
        Err(self.error_(err))
    }
    fn unparsed(&self) -> &'a str { &self.input[self.cursor..] }
}
enum Keyword {
}
enum N {
    N8,
    N16,
    N32,
    N64
}
enum Token<'a> {
    Keyword(Keyword),
    U(N),
    S(N),
    F(N),
    String(&'a str),
    Id(&'a str),
    POpen,
    PClose,
    Reserved(&'a str),

    Eof,
}
use self::Token::*;

impl<'a> Parser<'a> {
    token_parse!(skip_block_comment(self) -> () {
        self.cursor += 2;
        let mut nesting: usize = 1;

        loop {
            let up = self.unparsed().as_bytes().get(..2);
            match up {
                Some(b"(;") => {
                    self.cursor += 2;
                    nesting += 1;
                }
                Some(b";)") => {
                    self.cursor += 2;
                    nesting -= 1;
                    if nesting == 0 {
                        return Ok(());
                    }
                }
                Some(_) => {
                    self.cursor += 1;
                }
                None => {
                    return self.error(UnbalancedBlockComment);
                }
            }
        }
    });

    token_parse!(skip(self) -> () {
        'outer: loop {
            let up = self.unparsed().as_bytes();

            match up {
                  [b' ',    ..]
                | [b'\x09', ..]
                | [b'\x0a', ..]
                | [b'\x0d', ..]
                => {
                    self.cursor += 1;
                    continue 'outer;
                },
                [b';', b';', ..] => {
                    self.cursor += 2;
                    loop {
                        match self.unparsed().as_bytes().get(0) {
                            Some(&b'\x0a') => {
                                self.cursor += 1;
                                continue 'outer;
                            }
                            None => {
                                break 'outer;
                            }
                            _ => {
                                self.cursor += 1;
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

        Ok(())
    });

    token_parse!(skip_peek(self) -> &'a str {
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

    /*
    token_parse!(parse_token(self) -> Token<'a> {
        Err(())
        .or_else(|_| self.parse_keyword())
        .or_else(|_| self.parse_un_sn_fn())
        .or_else(|_| self.parse_string())
        .or_else(|_| self.parse_id())
        .or_else(|_| self.parse_parens())
        .or_else(|_| self.parse_reserved())
    });
    */

    token_parse!(parse_keyword(self) -> Token<'a> {
        Ok(match self.skip_peek()? {
            _ => Err(self.error_(Estr("KeywordNotRecognized")))?,
        })
    });

    /*
    token_parse!(parse_un_sn_fn(self) -> Token<'a> {
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
    token_parse!(parse_un(self, bits: u32) -> u64 {
        let mut s = self.skip_peek()?;
        let hex = s.starts_with("0x");
        if hex { s = &s[2..]; }

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
                s = &s[1..];
            }
        }

        if (n as u128) >= (1u128 << bits) {
            self.error(Estr("IntegerOutOfBounds"))?;
        }

        Ok(n)
    });

    token_parse!(parse_sn(self, bits: u32) -> i64 {
        let mut s = self.skip_peek()?;
        let positive = if s.starts_with('+') {
            s = &s[1..];
            true
        } else if s.starts_with('-') {
            s = &s[1..];
            false
        } else {
            return Err(self.error_(Estr("IntegerNotRecognized")));
        };

        let unsigned_n = self.parse_un(64)?;
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
    token_parse!(parse_in(self, bits: u32) -> u64 {
        Err(())
        .or_else(|_| self.parse_sn(bits).map(|x| x as u64))
        .or_else(|_| self.parse_un(bits))
    });
    token_parse!(parse_string(self) -> Token<'a> {

        Err(self.error_(Estr("StringNotRecognized")))
    });
    token_parse!(parse_id(self) -> Token<'a> {

        Err(self.error_(Estr("IdNotRecognized")))
    });
    token_parse!(parse_parens(self) -> Token<'a> {

        Err(self.error_(Estr("ParensNotRecognized")))
    });
    token_parse!(parse_reserved(self) -> Token<'a> {

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
            cursor: 0
        }
    }

    fn accept<T>(r: Result<T, ParserError>) {
        r.unwrap();
    }

    fn accept_eq<T: PartialEq>(r: Result<T, ParserError>, expected: T) {
        let is = r.unwrap();
        assert!(is == expected);
    }

    fn reject<T>(r: Result<T, ParserError>) {
        assert!(r.is_err());
    }

    #[test]
    fn parse_un() {
        accept(parser("0").parse_un(8));
        accept(parser("0").parse_un(16));
        accept(parser("0").parse_un(32));
        accept(parser("0").parse_un(64));
    }
}
