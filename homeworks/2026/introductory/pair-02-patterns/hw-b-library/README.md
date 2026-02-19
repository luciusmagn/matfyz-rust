# I2B Enum Command Library

Course track: Introductory Rust (2026)  
Homework pair: I2  
Type: library  
Submission filename: `solution.rs`

## Task

Implement enum-driven command parsing and evaluation.

Required API:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Add(i64),
    Sub(i64),
    Mul(i64),
    Reset,
    Print,
}

pub fn parse_op(line: &str) -> Option<Op>;
pub fn apply_op(state: &mut i64, op: &Op) -> Option<i64>;
pub fn run_program(lines: &[&str]) -> Vec<i64>;
```

## Behavior contract

1. `parse_op` returns `None` for malformed/unknown commands.
2. `apply_op` mutates state and returns `Some(state)` only for `Print`.
3. `run_program` starts at state `0`, ignores invalid commands, and returns all printed values.
