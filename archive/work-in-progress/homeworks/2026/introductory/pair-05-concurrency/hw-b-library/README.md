# I5B Channel-Based Parallel Helpers

Course track: Introductory Rust (2026)  
Homework pair: I5  
Type: library  
Submission filename: `solution.rs`

## Task

Implement parallel helper functions:

```rust
pub fn chunk_ranges(len: usize, workers: usize) -> Vec<(usize, usize)>;
pub fn dispatch_jobs(values: Vec<i64>, workers: usize) -> Vec<(usize, i64)>;
pub fn parallel_square_sum(values: Vec<i64>, workers: usize) -> i64;
```

## Behavior contract

1. `chunk_ranges` uses the same rule as I5A.
2. `dispatch_jobs` computes one sum per chunk and returns `(chunk_index, sum)` sorted by index.
3. `parallel_square_sum` computes `sum(v*v)` over all values.
