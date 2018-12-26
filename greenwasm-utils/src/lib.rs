use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone)]
pub struct IdAppendContainer<T> {
    counter: u64,
    data: HashMap<u64, T>,
}


impl<T> Default for IdAppendContainer<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IdAppendContainer<T> {
    pub fn new() -> Self {
        Self {
            counter: 0,
            data: Default::default(),
        }
    }
    pub fn append(&mut self, t: T) -> u64 {
        let c = self.counter;
        self.counter += 1;
        self.data.insert(c, t);
        return c;
    }
    pub fn get(&self, id: u64) -> Option<&T> {
        self.data.get(&id)
    }
    pub fn get_mut(&mut self, id: u64) -> Option<&mut T> {
        self.data.get_mut(&id)
    }
}

pub trait NamedLookup<Element> {
    fn lookup(&self, name: &str) -> Option<&Element>;
}
