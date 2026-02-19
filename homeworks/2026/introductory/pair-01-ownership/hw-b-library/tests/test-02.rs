mod solution;

fn main() {
    let ws = solution::normalize_words("bb aa bb cc");
    let uniq = solution::unique_sorted(&ws);
    println!("{}", solution::join_with_comma(&uniq));
}
