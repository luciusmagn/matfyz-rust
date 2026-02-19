# I4A Numeric Summary Pipeline

Course track: Introductory Rust (2026)  
Homework pair: I4  
Type: executable  
Submission filename: `main.rs`

## Task

Read integers and print deterministic summary statistics.

## Input format

1. Line 1: `n` (number of integers)
2. Line 2: exactly `n` integers separated by spaces

## Output format

For valid input print:

```text
unique=<comma-separated unique sorted values>
sum=<sum>
min=<min_or_none>
max=<max_or_none>
```

For `n = 0`, unique list is empty and min/max are `none`.

For invalid input (wrong count or parse error), print `invalid input`.
