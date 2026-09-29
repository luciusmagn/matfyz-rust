mod solution;

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_time()
        .build()
        .unwrap();
    let v = rt.block_on(solution::async_sum(vec![5, -1, 4]));
    println!("{v}");
}
