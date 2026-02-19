mod solution;

fn main() {
    let data: [i64; 0] = [];
    let view = solution::CheckedSlice::new(&data);
    println!("{}", view.len());
    println!("{:?}", view.window_sum(0, 0));
    println!("{}", solution::checksum(&data));
}
