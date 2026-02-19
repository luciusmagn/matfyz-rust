# I1B Owned Word Utilities

Course track: Introductory Rust (2026)  
Homework pair: I1  
Type: library  
Submission filename: `solution.rs`

## Task

Implement the required API in `solution.rs`:

```rust
pub fn normalize_words(input: &str) -> Vec<String>;
pub fn unique_sorted(words: &[String]) -> Vec<String>;
pub fn join_with_comma(words: &[String]) -> String;
```

## Behavior contract

1. `normalize_words`:
- split by whitespace
- lowercase each token
- return owned `Vec<String>`
2. `unique_sorted`:
- remove duplicates
- return lexicographically sorted words
3. `join_with_comma`:
- join words with `,` and no trailing separator

## Requirements

1. Keep signatures exactly as specified.
2. Keep output deterministic.
3. Do not print from library functions.
