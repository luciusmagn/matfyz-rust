mod solution;

fn main() {
    let out = solution::dispatch_jobs(vec![1, 2, 3, 4], 2);
    println!("{:?}", out);
}
