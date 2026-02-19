# A2B Key Lookup Engine

Course track: Advanced Rust (2026)  
Homework pair: A2  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that builds a key-value table and answers lookup queries.

## Input format

1. Line 1: `n` (number of entries).
2. Next `n` lines: `key:value`.
3. Next line: `q` (number of queries).
4. Next `q` lines: one key per line.

Keys and values are non-empty strings without `:`.

## Output format

For each query key:

- if key exists: `key=value`
- otherwise: `key=<missing>`

After all queries print:

```text
found=<count_found> missing=<count_missing>
```

If an entry line is invalid (missing `:`), print exactly `invalid input` and terminate.

## Requirements

1. For duplicate keys in input entries, keep the latest value.
2. Keep output order exactly as query order.
3. Do not print debug output.
