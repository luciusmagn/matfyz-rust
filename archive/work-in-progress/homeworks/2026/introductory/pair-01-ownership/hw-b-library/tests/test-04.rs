mod solution;

fn main() {
    let ws = vec!["žluťoučký".to_string(), "kůň".to_string(), "kůň".to_string()];
    let uniq = solution::unique_sorted(&ws);
    println!("{}", solution::join_with_comma(&uniq));
}
