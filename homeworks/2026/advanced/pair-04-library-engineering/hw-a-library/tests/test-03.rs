mod solution;

fn main() {
    let cfg = "a=10\nbadline\n";
    println!("{:?}", solution::parse_config(cfg));
}
