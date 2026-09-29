use std::collections::BTreeSet;

pub fn normalize_words(input: &str) -> Vec<String> {
    input
        .split_whitespace()
        .map(|w| w.to_lowercase())
        .collect()
}

pub fn unique_sorted(words: &[String]) -> Vec<String> {
    let mut set = BTreeSet::new();
    for w in words {
        set.insert(w.clone());
    }
    set.into_iter().collect()
}

pub fn join_with_comma(words: &[String]) -> String {
    words.join(",")
}
