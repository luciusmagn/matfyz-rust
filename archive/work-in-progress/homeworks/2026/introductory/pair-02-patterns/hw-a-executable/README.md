# I2A Stateful Command Processor

Course track: Introductory Rust (2026)  
Homework pair: I2  
Type: executable  
Submission filename: `main.rs`

## Task

Implement a command interpreter over one integer state.

## Input format

1. Line 1: `n` (number of commands)
2. Next `n` lines, command forms:
- `add X`
- `sub X`
- `mul X`
- `reset`
- `print`

`X` is signed `i64`.

## Output format

1. For each `print`, output current state value on a separate line.
2. For malformed/unknown command, output `err`.
3. After all commands output `final=<state>`.

State starts at `0`.
