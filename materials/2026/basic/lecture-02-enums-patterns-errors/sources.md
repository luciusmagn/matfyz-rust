# Technical references

Examples target Rust 1.88.0, edition 2021, matching the existing course decks. Standard-library and Reference links below use the 1.88.0 documentation. The Rust Book links follow its current edition.

## Enums and representation

- [Rust Book: defining enums](https://doc.rust-lang.org/book/ch06-01-defining-an-enum.html)
- [Rust Book: recursive types and `Box`](https://doc.rust-lang.org/book/ch15-01-box.html)
- [Rust Reference: type layout](https://doc.rust-lang.org/1.88.0/reference/type-layout.html)
- [`Option` representation guarantees](https://doc.rust-lang.org/1.88.0/std/option/index.html#representation)

The equal-size example uses `Option<&u8>`, a documented guarantee for references to sized types. General enum layouts and arbitrary `Option<T>` sizes are not inferred from that example. Product/sum counts describe possible values, not bytes of storage.

## Patterns and ownership

- [Rust Book: pattern syntax](https://doc.rust-lang.org/book/ch19-03-pattern-syntax.html)
- [Rust Reference: patterns](https://doc.rust-lang.org/1.88.0/reference/patterns.html)
- [Rust Reference: match expressions and guards](https://doc.rust-lang.org/1.88.0/reference/expressions/match-expr.html)
- [Rust Reference: let statements and `let else`](https://doc.rust-lang.org/1.88.0/reference/statements.html#let-statements)
- [`matches!`](https://doc.rust-lang.org/1.88.0/std/macro.matches.html)

The borrowed patterns use the binding modes inferred when matching `&Option<T>` and `&mut Option<T>`. The explicit `ref` example starts from an owned option. No edition-2024-only pattern combinations or let chains are needed.

## Optional values and errors

- [`Option`: adapters, extraction, combinators and in-place operations](https://doc.rust-lang.org/1.88.0/std/option/enum.Option.html)
- [`Result`: mapping, conversion and propagation](https://doc.rust-lang.org/1.88.0/std/result/enum.Result.html)
- [Rust Book: recoverable errors and `?`](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [`NonZeroU16`](https://doc.rust-lang.org/1.88.0/std/num/type.NonZeroU16.html)
- [`std::error::Error`](https://doc.rust-lang.org/1.88.0/std/error/trait.Error.html)
- [`Termination` for `Result`](https://doc.rust-lang.org/1.88.0/std/process/trait.Termination.html#impl-Termination-for-Result%3CT,+E%3E)

For `Result`, the explicit expansion of `?` includes error conversion through `From`. Detailed trait implementations are deferred to the traits lecture. `PortError` deliberately retains the original parsing error; formatting and implementing the standard error interface are separate topics.

## Panics and cleanup

- [Rust Book: unrecoverable errors](https://doc.rust-lang.org/book/ch09-01-unrecoverable-errors-with-panic.html)
- [Rust Reference: destructors](https://doc.rust-lang.org/1.88.0/reference/destructors.html)
- [`catch_unwind`](https://doc.rust-lang.org/1.88.0/std/panic/fn.catch_unwind.html)

Cleanup during a panic depends on unwinding. Aborting panics do not unwind; catching an unwinding panic does not roll back prior side effects. Returned `Result` errors follow ordinary return and drop behavior.
