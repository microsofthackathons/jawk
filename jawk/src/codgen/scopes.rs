use crate::codgen::ValuePtrT;
use crate::printable_error::PrintableError;
use std::collections::HashMap;
use gnu_libjit::Value;


pub struct Scopes {
    scalars: HashMap<String, ValuePtrT>,
    arrays: HashMap<String, Value>,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            scalars: HashMap::default(),
            arrays: HashMap::default(),
        }
    }
    pub fn insert_scalar(&mut self, name: String, value: ValuePtrT) -> Result<(), PrintableError> {
        if self.arrays.contains_key(&name) {
            return Err(PrintableError::new(format!("fatal: attempt to use array `{}` in a scalar context", name)))
        }
        if let Some(_) = self.scalars.get(&name) {
            return Err(PrintableError::new(format!(
                "Name {} is already in used, cannot shadow it",
                name
            )));
        }
        self.scalars.insert(name, value);
        Ok(())
    }

    pub fn get_scalar(&self, name: &str) -> Result<&ValuePtrT, PrintableError> {
        if self.arrays.contains_key(name) {
            return Err(PrintableError::new(format!("fatal: attempt to use array `{}` in a scalar context", name)))
        }
        if let Some(scalar) = self.scalars.get(name) {
            Ok(scalar)
        } else {
            panic!("Scalar {} does not exist", name);
        }
    }
}
