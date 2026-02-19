mod solution;

fn main() {
    let cfg = "\n  aa = -4\n bb = 8 \n";
    let map = solution::parse_config(cfg).unwrap();
    println!("{}", solution::render_config(&map));
    println!("{}", solution::sum_values(&map));
}
