use crate::codgen::ValuePtrT;
use crate::printable_error::PrintableError;
use std::collections::HashMap;

pub struct Scopes {
    scopes: HashMap<String, ValuePtrT>,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            scopes: HashMap::default(),
        }
    }
    pub fn insert(&mut self, name: String, value: ValuePtrT) -> Result<(), PrintableError> {
        if let Some(_) = self.scopes.get(&name) {
            return Err(PrintableError::new(format!(
                "Name {} is already in used, cannot shadow it",
                name
            )));
        }
        self.scopes.insert(name, value);
        Ok(())
    }

    pub fn get(&self, name: &str) -> &ValuePtrT {
        self.scopes.get(name).unwrap()
    }
}
