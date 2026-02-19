# I6A Async Task Doubler

Course track: Introductory Rust (2026)  
Homework pair: I6  
Type: executable  
Submission filename: `main.rs`

## Task

Write a Tokio-based async program that doubles numbers concurrently.

## Input format

1. Line 1: `n` (number of values)
2. Line 2: exactly `n` signed integers separated by spaces

## Output format

For each input value at index `i`, print:

```text
task <i>=<2*value>
```

Then print:

```text
sum=<sum_of_all_doubled_values>
```

Output order must match input index order.

For invalid input print `invalid input`.
