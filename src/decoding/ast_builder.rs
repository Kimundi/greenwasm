pub trait Builder: Sized {
    type StringBuilder: StringBuilder<Self>;
    fn string_builder(&mut self) -> Self::StringBuilder;

    type ValTypeBuilder: ValTypeBuilder<Self>;
    fn valtype_builder(&mut self) -> Self::ValTypeBuilder;
}

pub trait StringBuilder<B> where B: Builder {
    type Result;

    fn push(&mut self, c: char);
    fn pushb(&mut self, c: u8);
    fn build(self) -> Self::Result;
}

pub trait ValTypeBuilder<B> where B: Builder {
    type Result;

    fn build_i32(self) -> Self::Result;
    fn build_i64(self) -> Self::Result;
    fn build_f32(self) -> Self::Result;
    fn build_f64(self) -> Self::Result;
}

// ---------

pub struct HeapTreeBuilder;

impl Builder for HeapTreeBuilder {
    type StringBuilder = HeapTreeString;
    fn string_builder(&mut self) -> Self::StringBuilder {
        HeapTreeString { string: Vec::new() }
    }

    type ValTypeBuilder = EnumValTypeBuilder;
    fn valtype_builder(&mut self) -> Self::ValTypeBuilder {
        EnumValTypeBuilder
    }
}

// ---------

pub struct HeapTreeString {
    string: Vec<u8>
}

impl StringBuilder<HeapTreeBuilder> for HeapTreeString {
    type Result = Vec<u8>;

    fn push(&mut self, c: char) {
        let mut buf = [0; 4];
        self.string.extend(c.encode_utf8(&mut buf).bytes());
    }
    fn pushb(&mut self, c: u8) {
        self.string.push(c);
    }
    fn build(self) -> Self::Result {
        self.string
    }
}

use structure::types::ValType;
pub struct EnumValTypeBuilder;
impl ValTypeBuilder<HeapTreeBuilder> for EnumValTypeBuilder {
    type Result = ValType;
    fn build_i32(self) -> Self::Result { ValType::I32 }
    fn build_i64(self) -> Self::Result { ValType::I64 }
    fn build_f32(self) -> Self::Result { ValType::F32 }
    fn build_f64(self) -> Self::Result { ValType::F64 }
}

