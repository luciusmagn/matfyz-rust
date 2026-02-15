# A1A Parallel Chunk Processing Library

Course track: Advanced Rust (2026)  
Homework pair: A1  
Type: library  
Submission filename: `solution.rs`

## Task

Implement the required API in `solution.rs`:

```rust
pub fn make_chunks(len: usize, workers: usize) -> Vec<(usize, usize)>;
pub fn parallel_chunk_sums(values: Vec<i64>, workers: usize) -> Vec<i64>;
pub fn parallel_weighted_checksum(values: Vec<i64>, workers: usize) -> i64;
```

## Behavior contract

1. `make_chunks`:
- `effective_workers = max(1, min(workers, max(len, 1)))`
- For `i` in `0..effective_workers`:
  - `start = i * len / effective_workers`
  - `end = (i + 1) * len / effective_workers`
- Return chunk boundaries in worker order.
- For `len == 0`, return exactly `[(0, 0)]`.

2. `parallel_chunk_sums`:
- Split with `make_chunks(values.len(), workers)`.
- Compute one sum per chunk, preserving chunk order in output.
- For empty input, return `[0]`.

3. `parallel_weighted_checksum`:
- Compute `sum((index + 1) * value)` over full input.
- Result must match sequential semantics exactly.
- For empty input, return `0`.

## Requirements

1. Keep the API signatures exactly as specified.
2. Keep output behavior deterministic.
3. Do not print from library functions.
