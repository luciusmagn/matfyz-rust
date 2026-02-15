mod solution;

fn main() {
    let values = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    println!("{:?}", solution::parallel_chunk_sums(values.clone(), 3));
    println!("{:?}", solution::parallel_chunk_sums(values, 1));
}
