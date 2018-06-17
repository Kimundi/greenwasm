use super::*;

impl ValType {
    pub fn bit_width(&self) -> u32 {
        match *self {
            ValType::I32 | ValType::F32 => 32,
            ValType::I64 | ValType::F64 => 64,
        }
    }
}

/*
impl [ExternType] {
    fn funcs(&self) -> Vec<FuncType> {
        panic!()
    }
    fn tables(&self) -> Vec<TableType> {
        panic!()
    }
    fn mems(&self) -> Vec<MemType> {
        panic!()
    }
    fn globals(&self) -> Vec<GlobalType> {
        panic!()
    }
}
*/
