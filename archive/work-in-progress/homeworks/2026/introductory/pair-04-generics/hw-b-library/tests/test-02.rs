mod solution;

fn main() {
    let xs = vec!["b", "a", "b", "c", "a", "a"];
    println!("{:?}", solution::frequencies(&xs));
}
