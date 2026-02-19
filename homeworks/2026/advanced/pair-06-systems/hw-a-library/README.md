# A6A Path Utility Library

Course track: Advanced Rust (2026)  
Homework pair: A6  
Type: library  
Submission filename: `solution.rs`

## Task

Implement path-processing helpers in `solution.rs`.

Required API:

```rust
pub fn normalize_path(path: &str) -> String;
pub fn join_path(base: &str, child: &str) -> String;
pub fn split_extension(path: &str) -> (String, Option<String>);
```

## Behavior contract

1. `normalize_path`:
- removes repeated `/`
- removes `.` components
- resolves `..` where possible
- preserves leading `/` for absolute paths
- returns `.` for empty relative result
2. `join_path`:
- if `child` is absolute, normalize and return `child`
- else normalize `base + "/" + child`
3. `split_extension`:
- splits extension from final path segment
- returns `(stem, Some(ext))` when extension exists
- returns `(original_path, None)` otherwise

## Requirements

1. Deterministic behavior, no filesystem access required.
2. Keep signatures exactly as specified.
3. Do not print from library functions.
