mod solution;

fn main() {
    let values = vec![5, -2, 7, 0, 4, -1, 3];
    println!("{:?}", solution::parallel_chunk_sums(values.clone(), 4));
    println!("{}", solution::parallel_weighted_checksum(values, 4));
}
