# A3A Macro Toolkit

Course track: Advanced Rust (2026)  
Homework pair: A3  
Type: library  
Submission filename: `solution.rs`

## Task

Implement the following in `solution.rs`:

1. Macro `csv_line!` that converts arguments into one comma-separated `String`.
2. Macro `repeat_vec!` that builds a vector with `count` clones of `value`.
3. Macro `join_nonempty!` that joins non-empty rendered values with `|`.

## Behavior contract

1. `csv_line!()` with no arguments returns an empty string.
2. `csv_line!(a, b, c)` returns `"a,b,c"` using `Display` formatting.
3. `repeat_vec!(value; count)` returns a `Vec` with exactly `count` elements.
4. `join_nonempty!()` with no arguments returns an empty string.
5. `join_nonempty!(a, b, c)` formats each argument with `Display`, skips empty rendered values, and joins the rest with `|`.

## Requirements

1. Export macros so tests can use them from `mod solution;`.
2. Keep output deterministic.
3. Do not print from library code.
