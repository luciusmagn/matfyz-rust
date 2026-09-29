mod solution;

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_time()
        .build()
        .unwrap();
    let v = rt.block_on(solution::delayed_double(-7));
    println!("{v}");
}
