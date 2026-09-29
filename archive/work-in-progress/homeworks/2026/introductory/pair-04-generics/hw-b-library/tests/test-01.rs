mod solution;

fn main() {
    let xs = vec![3, 1, 9, 2, 9];
    println!("{:?}", solution::top_n(&xs, 3));
}
