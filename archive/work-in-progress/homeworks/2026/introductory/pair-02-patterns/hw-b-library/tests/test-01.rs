mod solution;

fn main() {
    println!("{:?}", solution::parse_op("add 10"));
    println!("{:?}", solution::parse_op("mul x"));
}
