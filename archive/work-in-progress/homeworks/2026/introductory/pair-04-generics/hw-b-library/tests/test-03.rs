mod solution;

fn main() {
    let a = vec![1, 2, 3];
    let b = vec![10, 20];
    println!("{:?}", solution::interleave(&a, &b));
}
