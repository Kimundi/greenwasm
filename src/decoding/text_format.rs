enum TokenizerErrorEnum {
    UnbalancedBlockComment,
    UnexpectedEof,
    Unrecognized,
    Estr(&'static str),
}
use self::TokenizerErrorEnum::*;
struct TokenizerError {
    position: usize,
    error: TokenizerErrorEnum,
}
struct Tokenizer<'a> {
    input: &'a str,
    cursor: usize,
}
macro_rules! token_parse {
    ($name:ident($self:ident) -> $t:ty $b:block) => (
        fn $name(&mut $self) -> Result<$t, TokenizerError> $b
    )
}
impl<'a> Tokenizer<'a> {
    fn error_(&self, err: TokenizerErrorEnum) -> TokenizerError {
        TokenizerError {
            position: self.cursor,
            error: err,
        }
    }
    fn error(&self, err: TokenizerErrorEnum) -> Result<(), TokenizerError> {
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

impl<'a> Tokenizer<'a> {
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

            if let Some(c) = up.get(0) {
                match *c {
                    b' ' | b'\x09' | b'\x0a' | b'\x0d' => {
                        self.cursor += 1;
                        continue 'outer;
                    },
                    b';' if up.get(1) == Some(&b';') => {
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
                    b'(' if up.get(1) == Some(&b';') => {
                        self.skip_block_comment();
                    }
                    _ => break 'outer,
                }
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

    token_parse!(parse_token(self) -> Token<'a> {
        Err(())
        .or_else(|_| self.parse_keyword())
        //.or_else(|_| self.parse_un_sn_fn())
        //.or_else(|_| self.parse_string())
        //.or_else(|_| self.parse_id())
        //.or_else(|_| self.parse_parens())
        //.or_else(|_| self.parse_reserved())
    });

    token_parse!(parse_keyword(self) -> Token<'a> {
        Ok(match self.skip_peek()? {
            "???" => unimplemented!(),
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
    token_parse!(parse_string(self) -> Token<'a> {

    });
    token_parse!(parse_id(self) -> Token<'a> {

    });
    */
}

use structure::modules::Module;
pub struct ParserError;
pub fn from_text_format(_b: &str) -> Result<Module, ParserError> {
    unimplemented!()
}
