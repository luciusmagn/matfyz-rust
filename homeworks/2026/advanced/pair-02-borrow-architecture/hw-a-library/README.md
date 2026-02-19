# A2A Borrowed Field Views

Course track: Advanced Rust (2026)  
Homework pair: A2  
Type: library  
Submission filename: `solution.rs`

## Task

Implement the required API in `solution.rs`:

```rust
pub fn split_fields<'a>(line: &'a str, sep: char) -> Vec<&'a str>;
pub fn longest_field<'a>(line: &'a str, sep: char) -> Option<&'a str>;
pub fn pick_columns<'a>(line: &'a str, sep: char, idxs: &[usize]) -> Vec<&'a str>;
```

## Behavior contract

1. `split_fields` returns borrowed views of fields from `line` split by `sep`.
2. For empty input line, `split_fields` returns an empty vector.
3. `longest_field` returns the longest field; if multiple fields share max length, return the first.
4. `pick_columns` returns fields by positional indexes in `idxs` in the same order as `idxs`.
5. Invalid indexes in `pick_columns` are skipped.

## Requirements

1. Keep all returned string slices borrowed from the original `line`.
2. Keep the function signatures exactly as specified.
3. Do not print from library functions.
