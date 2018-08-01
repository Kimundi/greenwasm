use structure::modules::Module;

pub struct BinaryFormatParserError;
pub fn from_binary_format(_b: &[u8]) -> Result<Module, BinaryFormatParserError> {
    unimplemented!()
}




#[cfg(test)]
#[path="tests_binary_format.rs"]
mod tests;
