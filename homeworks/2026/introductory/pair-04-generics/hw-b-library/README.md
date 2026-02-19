# I4B Generic Utility Library

Course track: Introductory Rust (2026)  
Homework pair: I4  
Type: library  
Submission filename: `solution.rs`

## Task

Implement generic utility functions:

```rust
pub fn top_n<T: Ord + Clone>(values: &[T], n: usize) -> Vec<T>;
pub fn frequencies<T: Ord + Clone>(values: &[T]) -> Vec<(T, usize)>;
pub fn interleave<T: Clone>(a: &[T], b: &[T]) -> Vec<T>;
```

## Behavior contract

1. `top_n`: return at most `n` largest elements in descending order.
2. `frequencies`: return `(value, count)` sorted by value ascending.
3. `interleave`: alternate from `a`, `b`, then append leftovers.
