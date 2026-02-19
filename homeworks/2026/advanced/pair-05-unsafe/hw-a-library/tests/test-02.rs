mod solution;

fn main() {
    let data = [4, -1, 7, 2];
    let view = solution::CheckedSlice::new(&data);
    println!("{:?}", view.window_sum(0, 4));
    println!("{:?}", view.window_sum(1, 3));
    println!("{:?}", view.window_sum(3, 1));
}
