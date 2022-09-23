use std::rc::Rc;
use mawk_regex::Regex;

fn main() {
    let regex = Regex::new("c+");
    println!("{}", regex.matches("dddd"));
    println!("{}", regex.matches("cccc"));

    let rc_str = Rc::new(String::from("c+"));
    let regex2 = Regex::new(&*rc_str);
    println!("{}", regex2.matches("dddd"));
    println!("{}", regex2.matches("cccc"));
}