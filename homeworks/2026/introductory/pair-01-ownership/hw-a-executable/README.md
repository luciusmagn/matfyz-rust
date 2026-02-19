# I1A Word Ownership Pipeline

Course track: Introductory Rust (2026)  
Homework pair: I1  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that processes words and prints a deterministic report.

## Input format

1. Line 1: `n` (number of words).
2. Next `n` lines: one word per line.

## Output format

For each input word at index `i` (starting from 0), print:

```text
i:<len>:<reversed_word>
```

After all lines print:

```text
total_chars=<sum_of_lengths>
```

If input is incomplete or invalid, print `invalid input` and terminate.

## Requirements

1. Keep output order exactly as input order.
2. Handle empty word lines correctly.
3. Do not print debug output.
