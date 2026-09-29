#[macro_use]
mod solution;

fn main() {
    let v = repeat_vec!(String::from("x"); 4);
    println!("{}", v.join(""));
    println!("{}", v.len());
}
