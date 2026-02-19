mod solution;

fn main() {
    let data = [10, 20, 30];
    let view = solution::CheckedSlice::new(&data);
    println!("{}", view.len());
    println!("{:?}", view.get(1));
    println!("{:?}", view.get(4));
}
