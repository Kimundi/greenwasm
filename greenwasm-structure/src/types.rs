//! Types according to the _structure_ section of the spec

// 2.1.3 Vectors

/// Vectors may have at most 2^32 - 1 elements, presumably
/// to ensure their size can be expressed as a u32.
pub const WEC_MAX_SIZE: usize = ::std::u32::MAX as usize;

/// A vec(A) according to the spec.
///
/// Constructing this type checks the invariant that it has
/// a size <= WEC_MAX_SIZE.
///
/// Called Wec to prevent confusion with Rusts Vec type.
#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Wec<A> {
    inner: Vec<A>
}
impl<A> From<Vec<A>> for Wec<A> {
    fn from(inner: Vec<A>) -> Self {
        assert!(inner.len() <= WEC_MAX_SIZE);
        Wec { inner }
    }
}
impl<A> Into<Vec<A>> for Wec<A> {
    fn into(self) -> Vec<A> {
        self.inner
    }
}
impl<A> ::std::ops::Deref for Wec<A> {
    type Target = Vec<A>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<'a, A> IntoIterator for &'a Wec<A> {
    type Item = &'a A;
    type IntoIter = <&'a Vec<A> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}
impl<A> IntoIterator for Wec<A> {
    type Item = A;
    type IntoIter = <Vec<A> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}
impl<A> ::std::iter::FromIterator<A> for Wec<A> {
    fn from_iter<T>(iter: T) -> Self
        where T: IntoIterator<Item = A>
    {
        Vec::from_iter(iter).into()
    }
}
impl<A> Default for Wec<A> {
    fn default() -> Self {
        Vec::default().into()
    }
}
impl<A> Wec<A> {
    pub fn safe_append<F: FnMut() -> A>(&mut self, n: usize, mut f: F) {
        let l = self.len();
        self.inner.reserve(l + n);
        for _ in 0..n {
            self.inner.push(f());
        }
    }
}

// 2.2 Values
// 2.2.1 Bytes

pub type Byte = u8;

// 2.2.2 Integers

pub type U8 = u8;
pub type S8 = i8;
pub type I8 = U8;

pub type U16 = u16;
pub type S16 = i16;
pub type I16 = U16;

pub type U32 = u32;
pub type S32 = i32;
pub type I32 = U32;

pub type U64 = u64;
pub type S64 = i64;
pub type I64 = U64;

pub type F32 = f32;
pub type F64 = f64;

/// A Name according to the spec.
///
/// Constructing this type checks the invariant that it has
/// a size <= WEC_MAX_SIZE.
#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Name {
    inner: String
}
impl From<String> for Name {
    fn from(inner: String) -> Self {
        assert!(inner.len() <= WEC_MAX_SIZE);
        Name { inner }
    }
}
impl<'a> From<&'a str> for Name {
    fn from(inner: &'a str) -> Self {
        assert!(inner.len() <= WEC_MAX_SIZE);
        Name { inner: inner.into() }
    }
}
impl Into<String> for Name {
    fn into(self) -> String {
        self.inner
    }
}
impl ::std::ops::Deref for Name {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
pub type Codepoint = char;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

// TODO. What does the [] notation in the grammar spec mean exactly?
// Eg, `resulttype​::=​[valtype?]​`
// ...
// Later answer: Obviously that it is a list with 0 or 1 element,
// as opposed to a missing or not missing element.

/// A ResultType is a list of 0 or 1 elements
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ResultType {
    results: Option<ValType>
}
impl ::std::ops::Deref for ResultType {
    type Target = [ValType];

    fn deref(&self) -> &Self::Target {
        self.results.as_ref().map(::std::slice::from_ref).unwrap_or(&[])
    }
}
impl From<ValType> for ResultType {
    fn from(t: ValType) -> Self {
        ResultType {
            results: Some(t)
        }
    }
}
impl From<Option<ValType>> for ResultType {
    fn from(t: Option<ValType>) -> Self {
        ResultType {
            results: t
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncType {
    pub args: Wec<ValType>,
    pub results: Wec<ValType>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Limits {
    pub min: U32,
    pub max: Option<U32>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemType {
    pub limits: Limits,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TableType {
    pub limits: Limits,
    pub elemtype: ElemType,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ElemType {
    AnyFunc,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct GlobalType {
    pub mutability: Mut,
    pub valtype: ValType,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExternType {
    Func(FuncType),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
