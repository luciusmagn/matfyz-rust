# A4A Config Library Contract

Course track: Advanced Rust (2026)  
Homework pair: A4  
Type: library  
Submission filename: `solution.rs`

## Task

Implement a small configuration library in `solution.rs`.

Required API:

```rust
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigError {
    MissingEquals(usize),
    EmptyKey(usize),
    InvalidValue(usize),
    DuplicateKey(String),
}

pub fn parse_config(input: &str) -> Result<BTreeMap<String, i64>, ConfigError>;
pub fn render_config(map: &BTreeMap<String, i64>) -> String;
pub fn sum_values(map: &BTreeMap<String, i64>) -> i64;
```

## Parsing rules

1. Empty lines are ignored.
2. Non-empty line must have exactly one `=` split point (`key=value`).
3. Key is trimmed and must be non-empty.
4. Value is trimmed and must parse as `i64`.
5. Duplicate keys are rejected as `DuplicateKey`.
6. Error line numbers are 1-based in original input.

## Rendering rules

1. Emit lines as `key=value` in sorted key order (BTreeMap iteration order).
2. Join lines with `\n` and no trailing newline.

## Requirements

1. Keep behavior deterministic.
2. Keep signatures exactly as specified.
3. Do not print from library functions.
