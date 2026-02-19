mod solution;

fn main() {
    println!("{}", solution::normalize_path("../../a"));
    println!("{}", solution::normalize_path("/../../a"));
}
