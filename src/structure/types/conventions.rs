use super::*;

impl ValType {
    pub fn bit_width(&self) -> u32 {
        match *self {
            ValType::I32 | ValType::F32 => 32,
            ValType::I64 | ValType::F64 => 64,
        }
    }
}
