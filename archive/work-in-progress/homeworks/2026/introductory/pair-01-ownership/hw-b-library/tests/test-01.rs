mod solution;

fn main() {
    let ws = solution::normalize_words("Hello RUST hello");
    println!("{}", solution::join_with_comma(&ws));
}
