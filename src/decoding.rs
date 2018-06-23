use super::structure::modules::Module;

pub mod text_format;
pub mod ast_builder;

pub struct BinaryFormatParserError;
pub fn from_binary_format(_b: &[u8]) -> Result<Module, BinaryFormatParserError> {
    unimplemented!()
}
