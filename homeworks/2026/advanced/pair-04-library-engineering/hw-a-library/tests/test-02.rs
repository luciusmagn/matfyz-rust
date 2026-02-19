mod solution;

fn main() {
    let cfg = "x=1\nx=2\n";
    println!("{:?}", solution::parse_config(cfg));
}
