# A3B Recursive Declarative Macros

Course track: Advanced Rust (2026)  
Homework pair: A3  
Type: library  
Submission filename: `solution.rs`

## Task

Implement the following in `solution.rs`:

1. Trait:

```rust
pub trait RenderLabel {
    fn render_label(&self) -> String;
}
```

2. Macro `count_exprs!` that returns the number of expressions passed to it.
3. Macro `prefix_calc!` that evaluates a nested prefix arithmetic expression.
4. Macro `impl_render_label!` that implements `RenderLabel` for a list of types.

## Required syntax

```rust
count_tts!()
count_tts!(foo bar (baz quux) [x y] {z})

prefix_calc!((value 7))
prefix_calc!((neg (value 7)))
prefix_calc!((add (value 1) (mul (value 2) (value 3))))
prefix_calc!((sub (value 10) (neg (value 4))))

impl_render_label!(i32, usize, u8)
```

## Behavior contract

1. `count_tts!()` returns `0usize`.
2. `count_tts!(...)` returns the number of top-level token trees in its input.
3. `prefix_calc!((value expr))` expands to the value of `expr`.
4. `prefix_calc!((neg x))`, `prefix_calc!((add a b))`, `prefix_calc!((sub a b))`, `prefix_calc!((mul a b))` and `prefix_calc!((div a b))` evaluate recursively.
5. `prefix_calc!` must be usable in `const` context.
6. `impl_render_label!` generates `RenderLabel` impls where `render_label()` returns exactly `"<type>=<value>"`.

## Requirements

1. Export macros so tests can use them from `mod solution;`.
2. Use recursive declarative macros for `count_tts!` and `prefix_calc!`.
3. Use `$crate` inside macro expansions that refer back into the same crate.
4. Do not solve `prefix_calc!` by converting tokens to a string and parsing them at runtime.
5. Keep output deterministic.
6. Do not print from library code.
