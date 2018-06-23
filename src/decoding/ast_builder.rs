/*

pub trait AstBuilder {
    type String;
    type StringBuilder: StringBuilder<Self::String>;

    fn string_builder(&mut self, size_hint: usize) -> Self::StringBuilder;
}

pub trait StringBuilder<String> {
    pub fn push(&mut self, c: char);
    pub fn pushb(&mut self, c: u8);
    pub fn build(self) -> String;
}

pub struct StringBuilder<'a> {
    ast: &'a mut AstBuilder,
    string: Vec<u8>
}
pub struct StringId(usize);
impl<'a> StringBuilder<'a> {
    pub fn push(&mut self, c: char) {
        let mut buf = [0; 4];
        self.string.extend(c.encode_utf8(&mut buf).bytes());
    }
    pub fn pushb(&mut self, c: u8) {
        self.string.push(c);
    }
    pub fn commit(self) -> StringId {
        self.ast.strings.push(self.string);
        StringId(self.ast.strings.len() - 1)
    }
}
*/

