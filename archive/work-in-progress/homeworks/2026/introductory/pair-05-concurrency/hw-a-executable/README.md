# I5A Deterministic Parallel Chunk Sum

Course track: Introductory Rust (2026)  
Homework pair: I5  
Type: executable  
Submission filename: `main.rs`

## Task

Split numeric input into worker chunks, process in parallel, and print deterministic output.

## Input format

1. Line 1: `workers` (positive integer)
2. Line 2: `n` (number of values)
3. Line 3: exactly `n` signed integers separated by spaces

## Output format

For each chunk in chunk index order:

```text
chunk <idx>=<chunk_sum>
```

Then:

```text
total=<sum_of_all_values>
```

Chunk ranges use:

- `effective_workers = max(1, min(workers, max(n, 1)))`
- `start = i * n / effective_workers`
- `end = (i + 1) * n / effective_workers`

For `n = 0`, produce one chunk `0..0`.
