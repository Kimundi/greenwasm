use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone)]
pub struct IdAppendContainer<T> {
    counter: usize,
    data: HashMap<usize, T>,
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
    pub fn append(&mut self, t: T) -> usize {
        let c = self.counter;
        self.counter += 1;
        self.data.insert(c, t);
        return c;
    }
    pub fn get(&self, id: usize) -> Option<&T> {
        self.data.get(&id)
    }
    pub fn get_mut(&mut self, id: usize) -> Option<&mut T> {
        self.data.get_mut(&id)
    }
    pub fn remove(&mut self, id: usize) -> Option<T> {
        self.data.remove(&id)
    }
}

pub trait NamedLookup<Element> {
    fn lookup(&self, name: &str) -> Option<Element>;
}
