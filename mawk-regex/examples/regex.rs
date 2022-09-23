use mawk_regex::Regex;

fn main() {
    let regex = Regex::new("c+");
    println!("{}", regex.matches("dddd"));
    println!("{}", regex.matches("cccc"));
}