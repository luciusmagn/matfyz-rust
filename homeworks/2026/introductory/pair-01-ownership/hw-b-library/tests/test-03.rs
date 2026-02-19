mod solution;

fn main() {
    let ws = solution::normalize_words("   ");
    println!("{}", ws.len());
    println!("{:?}", solution::join_with_comma(&ws));
}
