use std::fmt::{Display, Formatter};

// TODO: Should be able to print a code sample with line numbers
#[derive(Debug)]
pub struct PrintableError {
    pub msg: String,
}

impl Display for PrintableError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

impl PrintableError {
    pub fn new<S: Into<String>>(msg: S) -> Self {
        PrintableError { msg: msg.into() }
    }
}
