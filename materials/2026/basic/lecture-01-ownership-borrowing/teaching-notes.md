# Ownership: teaching notes

## 90-minute allocation

| Minutes | Slides | Material |
| --- | --- | --- |
| 0–12 | 1–7 | Values, resource lifetime, String storage, scope |
| 12–27 | 8–14 | Moves, unavailable bindings, reinitialization, arguments and returns |
| 27–40 | 15–19 | Copy as a type property, derivation, String cloning |
| 40–50 | 20–22 | Partial moves and availability across control-flow paths |
| 50–62 | 23–27 | Destruction, observable order, early release, replacement |
| 62–77 | 28–36 | Shared and exclusive access, mutability, last use, reborrowing |
| 77–87 | 37–43 | String and sequence slices, vector growth, function signatures |
| 87–90 | 44–46 | Ownership choices, summary, references |

The examples are independent. Each executable block contains a complete program. The timing includes compilation, small edits, and questions. Use the failing/successful pair on slides 32–33 to separate borrow duration from the enclosing lexical scope.

## Technical details

- **Move semantics:** transfer of an owned value. The compiler may eliminate physical copies. The diagrams describe ownership and reachable storage, rather than an ABI or a mandatory instruction sequence.
- **Copy:** an explicit type property, independent of value size. A struct with only integer fields is still non-Copy until it implements Copy. Copy requires Clone, and every field must be Copy. A type with a destructor cannot implement Copy.
- **Clone:** type-defined duplication. The String example has independent byte buffers; reference-counted pointer clones can share a resource. Detailed shared-ownership types belong to a later treatment.
- **Partial moves:** the remaining initialized fields are still dropped. Moving fields out of a value that implements Drop is restricted, because its destructor expects the complete value.
- **Conditional moves:** availability is checked across control-flow paths. The compiler does not prove the result of the string-length comparison used on slide 22.
- **Destruction:** ordinary owned locals drop at scope exit, rather than at their last use. Local bindings drop in reverse declaration order; struct fields have a different, declaration-order rule. The Trace examples produce `2 end 1 ` and `1 body 2 `, respectively.
- **Resource lifetime:** unwinding performs cleanup along the unwound path. Abort, process exit, and deliberate leaks can bypass destructors. Memory safety permits leaks.
- **Underscore bindings:** `_outer` and `_inner` are real bindings. Replacing either with the wildcard `_` changes the lifetime of the temporary in the Trace example.
- **Borrowing:** the examples use ordinary shared access. Interior-mutability types provide additional protocols and are a later topic. `&mut T` is non-Copy; a shorter reborrow temporarily provides access through an existing mutable reference.
- **Last use:** ending a borrow does not drop its owner. Reference validity and destruction of an owned local are separate questions.
- **Slices:** String indices are byte offsets at UTF-8 boundaries. The slice examples use ASCII so the boundary is unambiguous. `String::len` counts bytes.
- **Borrowed results:** the single-input lifetime-elision rule is enough for `trimmed`. Explicit lifetime parameters and APIs with multiple input lifetimes can follow in a later lecture.

## Checks

There are 31 standalone examples: 22 successful programs with checked output and nine intentional compiler failures. `verify-examples.lisp` checks them on Rust 1.88.0, edition 2021. The signature overview is a declaration fragment.

## Sources

- [The Rust Book, chapter 4](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)
- [Rust Reference: place expressions and value expressions](https://doc.rust-lang.org/reference/expressions.html#place-expressions-and-value-expressions)
- [Rust Reference: destructors and drop scopes](https://doc.rust-lang.org/reference/destructors.html)
- [Copy](https://doc.rust-lang.org/std/marker/trait.Copy.html), [Clone](https://doc.rust-lang.org/std/clone/trait.Clone.html), [drop](https://doc.rust-lang.org/std/mem/fn.drop.html)
- [String representation](https://doc.rust-lang.org/std/string/struct.String.html#representation), [Vec guarantees](https://doc.rust-lang.org/std/vec/struct.Vec.html#guarantees)
