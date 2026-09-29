#[allow(dead_code)]
#[macro_use]
mod solution;

const A: i64 = prefix_calc!((value 7));
const B: i64 = prefix_calc!((add (value 1) (mul (value 2) (value 3))));

fn main() {
    println!("{A}");
    println!("{B}");
}
