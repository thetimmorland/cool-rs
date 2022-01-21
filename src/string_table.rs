use std::{collections::HashSet, rc::Rc};

pub struct StringTable(HashSet<Rc<String>>);

impl StringTable {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn insert(&mut self, s: &str) -> Rc<String> {
        let rc = Rc::new(s.to_string());

        if self.0.insert(rc.clone()) {
            rc
        } else {
            self.0.get(&rc).unwrap().clone()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn it_works() {
        let mut st = StringTable::new();
        let foo1 = st.insert("foo");
        let foo2 = st.insert("foo");
        let bar1 = st.insert("bar");
        let bar2 = st.insert("bar");

        assert!(Rc::ptr_eq(&foo1, &foo2));
        assert!(Rc::ptr_eq(&bar1, &bar2));

        assert!(!Rc::ptr_eq(&foo1, &bar1));
    }
}
