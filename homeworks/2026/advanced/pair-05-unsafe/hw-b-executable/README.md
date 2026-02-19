# A5B Bounded Stack Processor

Course track: Advanced Rust (2026)  
Homework pair: A5  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that executes stack commands with fixed capacity.

## Input format

1. Line 1: `capacity` (non-negative integer).
2. Line 2: `n` (number of commands).
3. Next `n` lines, one command each:
- `push X`
- `pop`
- `peek`

`X` is signed `i64`.

## Output rules

For each command print:

1. `push X`
- success: `ok`
- when stack full: `full`
2. `pop`
- when non-empty: popped value
- when empty: `empty`
3. `peek`
- when non-empty: top value
- when empty: `empty`

After all commands print:

```text
size=<current_size>
```

## Requirements

1. Parse and execute commands in order.
2. Keep output exactly deterministic.
3. Do not print debug output.
