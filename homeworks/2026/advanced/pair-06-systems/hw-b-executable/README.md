# A6B Log Severity Summary

Course track: Advanced Rust (2026)  
Homework pair: A6  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that summarizes log severities.

## Input format

1. Line 1: `n` (number of log lines)
2. Next `n` lines: `LEVEL message...`

`LEVEL` may be `INFO`, `WARN`, `ERROR`, or anything else.

## Output format

Print exactly:

```text
INFO=<count>
WARN=<count>
ERROR=<count>
OTHER=<count>
status=<ok|degraded>
```

`status=degraded` if `ERROR > 0`, otherwise `status=ok`.

## Requirements

1. Count only first token as level.
2. Empty lines count as `OTHER`.
3. Keep formatting exact and deterministic.
