mod solution;

fn main() {
    println!("{:?}", solution::make_chunks(10, 3));
    println!("{:?}", solution::make_chunks(3, 8));
    println!("{:?}", solution::make_chunks(0, 0));
}
