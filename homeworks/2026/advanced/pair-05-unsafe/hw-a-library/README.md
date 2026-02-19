# A5A CheckedSlice Wrapper

Course track: Advanced Rust (2026)  
Homework pair: A5  
Type: library  
Submission filename: `solution.rs`

## Task

Implement a safe wrapper around raw pointer reads.

Required API:

```rust
pub struct CheckedSlice<'a>;

impl<'a> CheckedSlice<'a> {
    pub fn new(slice: &'a [i64]) -> Self;
    pub fn len(&self) -> usize;
    pub fn get(&self, idx: usize) -> Option<i64>;
    pub fn window_sum(&self, start: usize, end: usize) -> Option<i64>;
}

pub fn checksum(slice: &[i64]) -> i64;
```

## Behavior contract

1. `get(idx)` returns element copy if `idx < len`, otherwise `None`.
2. `window_sum(start, end)` sums `[start, end)` and returns `None` for invalid ranges.
3. `checksum(slice)` computes `sum((i + 1) * slice[i])`.
4. Empty ranges are valid and sum to `0`.

## Requirements

1. Keep API signatures exactly as specified.
2. Keep public API safe.
3. Keep behavior deterministic and do not print from library code.
