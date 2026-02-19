#[macro_use]
mod solution;

fn main() {
    println!("{:?}", csv_line!());
    let v: Vec<i32> = repeat_vec!(3; 0);
    println!("{}", v.len());
}
