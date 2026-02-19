mod solution;

fn main() {
    println!("{}", solution::normalize_path("a//b/./c"));
    println!("{}", solution::normalize_path("/tmp///x/.."));
}
