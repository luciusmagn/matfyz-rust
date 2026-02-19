mod solution;

fn main() {
    let cfg = "alpha=10\nbeta=20\n";
    let map = solution::parse_config(cfg).unwrap();
    println!("{}", solution::render_config(&map));
    println!("{}", solution::sum_values(&map));
}
