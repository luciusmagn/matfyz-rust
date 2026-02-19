# A3B Arithmetic Command Evaluator

Course track: Advanced Rust (2026)  
Homework pair: A3  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that evaluates simple arithmetic commands.

## Input format

1. Line 1: `n` (number of commands).
2. Next `n` lines: `op a b` where:
- `op` is one of `add`, `sub`, `mul`, `div`
- `a`, `b` are signed integers (`i64`)

## Output format

For each command:

- valid command: print numeric result
- invalid command (unknown op, parse failure, division by zero): print `err`

After all commands print:

```text
ok=<count_ok> err=<count_err>
```

## Requirements

1. Keep output line count exactly as specified.
2. Use deterministic formatting.
3. Do not print debug output.
