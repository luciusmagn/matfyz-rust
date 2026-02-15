mod solution;

fn main() {
    let empty: Vec<i64> = vec![];
    println!("{:?}", solution::parallel_chunk_sums(empty.clone(), 0));
    println!("{}", solution::parallel_weighted_checksum(empty, 0));

    let one = vec![42];
    println!("{:?}", solution::make_chunks(one.len(), 16));
    println!("{:?}", solution::parallel_chunk_sums(one.clone(), 16));
    println!("{}", solution::parallel_weighted_checksum(one, 16));
}
