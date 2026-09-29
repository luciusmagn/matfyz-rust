# I6B Async Helper Library

Course track: Introductory Rust (2026)  
Homework pair: I6  
Type: library  
Submission filename: `solution.rs`

## Task

Implement async helpers in `solution.rs`:

```rust
pub async fn delayed_double(value: i64) -> i64;
pub async fn async_sum(values: Vec<i64>) -> i64;
pub fn run_async_sum(values: Vec<i64>) -> i64;
```

## Behavior contract

1. `delayed_double` returns `2 * value`.
2. `async_sum` doubles each value asynchronously and returns sum.
3. `run_async_sum` runs `async_sum` on a Tokio runtime and returns its result.
