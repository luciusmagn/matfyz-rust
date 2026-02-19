mod solution;

fn main() {
    let xs = vec![5, 4, 3];
    println!("{:?}", solution::top_n(&xs, 0));
    println!("{:?}", solution::frequencies::<i64>(&[]));
}
