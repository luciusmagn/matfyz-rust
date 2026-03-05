#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 3],
  config-info(
    title: [Advanced Rust (2026): Macros],
    subtitle: [Lecture 3],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Expansion Model

== What a Declarative Macro Is
- `macro_rules!` maps token-tree patterns to token-tree expansions.
- Expansion runs before type checking.
- Macro correctness and type correctness are separate phases.

== Why Macros Are Powerful
- Remove repetitive syntax, not just repetitive computation.
- Encode DSL-like call sites with compile-time checking.
- Generate impl/test/module boilerplate consistently.

== Cost of Macro Power
- Debugging errors can be harder than function errors.
- Public macro syntax is public API.
- Overly broad patterns produce poor diagnostics.

== Expansion Pipeline
- Parse source into token trees.
- Match macro arm top-down.
- Substitute metavariables into expansion.
- Parse expanded output as normal Rust.

== Minimal Example
```rust
macro_rules! twice {
    ($x:expr) => { 2 * ($x) };
}

let y = twice!(21);
println!("{y}");
```

= Pattern Language

== Fragment Specifiers
- `expr`, `ident`, `ty`, `path`, `pat`, `item`, `stmt`, `tt`.
- Choose the narrowest fragment that expresses intent.
- Narrow fragments improve both matching and diagnostics.

== `tt` Fragment
- `tt` is the most flexible unit.
- Useful for recursive parsing patterns.
- Easy to misuse if grammar is underspecified.

== Repetition Operators
- `*` zero or more.
- `+` one or more.
- `?` optional single occurrence.
- Separator is placed inside repetition form.

```rust
macro_rules! list {
    ($($x:expr),* $(,)?) => {
        vec![$($x),*]
    };
}
```

== Optional Trailing Comma
- Common ergonomics expectation.
- Use `$(,)?` in pattern where appropriate.
- Keep behavior consistent across macro variants.

== Nested Repetition
- Needed for matrix/table-like syntax.
- Requires careful metavariable alignment.
- Mismatch errors can become non-obvious.

= Hygiene and Name Resolution

== Hygiene Principle
- Macro-generated local identifiers do not capture caller locals by accident.
- Captured metavariables preserve caller bindings.
- This avoids a class of textual-substitution bugs.

== Hygiene Example
```rust
macro_rules! make_tmp {
    ($e:expr) => {{
        let tmp = 10;
        tmp + ($e)
    }};
}

let tmp = 1;
println!("{}", make_tmp!(tmp)); // caller tmp is still used inside ($e)
```

== Path Hygiene in 2026-Style Code
- Prefer fully-qualified std/core paths inside macro expansions.
- Avoid requiring caller imports for expansion internals.

```rust
macro_rules! boxed {
    ($x:expr) => {
        ::std::boxed::Box::new($x)
    };
}
```

== `$crate` for Internal References
- `$crate` makes intra-crate macro references robust.
- Important when macro is exported and used from other crates.

```rust
macro_rules! call_helper {
    () => {
        $crate::helper()
    };
}
```

= Error Design

== Fallback Arm Strategy
- Final arm with `compile_error!` gives predictable diagnostics.
- Put strict valid forms first.
- Keep error text domain-specific.

```rust
macro_rules! op {
    (add $a:expr, $b:expr) => { $a + $b };
    (sub $a:expr, $b:expr) => { $a - $b };
    ($($rest:tt)*) => {
        compile_error!("op!: expected `add <expr>, <expr>` or `sub <expr>, <expr>`");
    };
}
```

== Ambiguity Control
- Avoid overlapping arms with similar prefixes.
- If overlap is required, order arms from specific to general.
- Ambiguity often appears in DSL-like macros.

== User-Facing Macro UX
- Error points should indicate bad token region.
- Macro syntax should be minimal and regular.
- Avoid hidden side effects in expansion.

= Advanced Patterns

== TT Muncher
- Recursive macro that consumes token stream progressively.
- Typical for custom mini-parser behavior.
- Must include explicit base case to terminate.

```rust
macro_rules! count_idents {
    () => {0usize};
    ($head:ident $(, $tail:ident)*) => {
        1usize + count_idents!($($tail),*)
    };
}
```

== Accumulator Pattern
- Carry partial state across recursive calls.
- Useful for token transforms and normalization.
- Keep accumulator representation simple.

== Macro-Generated Impl Blocks
- Great for repetitive trait impls.
- Keep generated code inspectable.
- Avoid generating surprising blanket impl combinations.

```rust
macro_rules! impl_display_num {
    ($($t:ty),* $(,)?) => {
        $(
            impl MyDisplay for $t {
                fn render(&self) -> String { self.to_string() }
            }
        )*
    };
}
```

== Macro-Generated Tests
- Reduce copy-paste in parameterized tests.
- Deterministic test naming matters for CI readability.

```rust
macro_rules! case {
    ($name:ident, $input:expr, $expected:expr) => {
        #[test]
        fn $name() {
            assert_eq!(solve($input), $expected);
        }
    };
}
```

= Interactions with Type System

== Macros and Type Inference
- Macro expands before type checking.
- Inference failures often look like ordinary type errors.
- Add type ascription at call site when diagnostics are unclear.

== Trait Bounds in Expansion
- Generated code can require trait bounds caller did not expect.
- Document these requirements explicitly in macro docs.
- Hidden bounds are common source of user confusion.

== `expr` vs `tt` API Choice
- `expr` gives stronger syntactic guarantees.
- `tt` gives flexibility but weaker guarantees.
- Prefer `expr` unless grammar genuinely needs `tt`.

= Public API and Versioning

== Macro Syntax as SemVer Surface
- Removing accepted form is a breaking change.
- Tightening pattern fragments can break callers.
- Reordering ambiguous arms can change behavior.

== Edition and Context Sensitivity
- Macro behavior can differ with path resolution context.
- Test from external crate, not only internal tests.
- Exported macros deserve integration tests.

== Documentation Pattern
- Provide at least one minimal and one advanced example.
- Document accepted grammar in bullet form.
- Document failure forms with expected diagnostics.

= Practical Engineering

== Debugging Expansion
- Use `cargo expand` when reasoning about generated code.
- Read expanded code as normal Rust first.
- Keep expansions small enough for human review.

== Review Checklist
- Is macro needed vs function/trait?
- Is grammar small and explicit?
- Are diagnostics precise?
- Is expanded code unsurprising?
- Is semver impact understood?

== Final Rules
- Macros should compress boilerplate, not hide design.
- If expansion logic is hard to explain, simplify grammar.
- Prefer two simple macros over one clever parser macro.
- Keep macro call sites readable for non-authors.
