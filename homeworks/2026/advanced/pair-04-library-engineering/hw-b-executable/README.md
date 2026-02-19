# A4B Config Canonicalizer

Course track: Advanced Rust (2026)  
Homework pair: A4  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that canonicalizes key-value pairs.

## Input format

1. Line 1: `n` (number of records).
2. Next `n` lines: `key=value` where value is `i64`.

## Validation

A record is invalid if:

1. It does not contain `=`.
2. Key is empty after trimming.
3. Value is not a valid `i64`.

If any record is invalid, print exactly:

```text
error
```

and terminate.

## Output format (valid input)

1. Print canonical records sorted by key: `key=value`.
2. If duplicate key appears, keep the latest value.
3. Print final line: `total=<sum of values>`.

## Requirements

1. Output must be deterministic.
2. Do not print debug output.
