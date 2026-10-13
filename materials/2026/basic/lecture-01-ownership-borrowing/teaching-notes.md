# Ownership teaching notes

## 90-minute allocation

| Minutes | Slides | Material |
| --- | --- | --- |
| 0–15 | 1–10 | Pointers, stack and heap, resource lifetime, String storage, structs, scope |
| 15–29 | 11–18 | Moves, Box, reinitialization, owned arguments and returns |
| 29–40 | 19–26 | Copy, Clone, partial moves and control-flow paths |
| 40–49 | 27–31 | Destruction, observable order, early release and replacement |
| 49–64 | 32–41 | References, method receivers, mutability and last use |
| 64–72 | 42–46 | Reborrowing, Stacked Borrows and reference validity |
| 72–86 | 47–59 | Slices, borrowed results, lifetime parameters and elision |
| 86–90 | 60–63 | Ownership choices, summary and further reading |

The examples are independent. Each executable block contains a complete program. Run selected examples during the lecture rather than compiling all 41. Prioritize the conflicting-access pair on slides 39–40 and the borrowed method on slide 57. If the memory introduction needs more discussion, leave replacement and the two-input lifetime examples for individual reading.

## Memory and owned values

- Introduce an address as a numbered location in memory. A pointer refers to a location, and dereferencing accesses the value there. The diagram's hexadecimal address is illustrative. Real Rust pointers also have provenance and may have metadata. An arbitrary integer address is not sufficient permission for a valid access.
- Stack frames model the storage associated with active calls. The compiler can keep locals in registers, inline calls, or eliminate storage. Heap allocation requests a separate block whose lifetime need not match the allocating call. These are storage concepts, distinct from Rust's ownership rules.
- Struct fields are part of the enclosing value. A field such as `String` can itself own a separate allocation. Named, tuple and unit structs all obey the same ownership rules.
- Moves transfer ownership. The compiler may eliminate physical copies. The diagrams describe ownership and reachable storage rather than an ABI or a mandatory instruction sequence.
- `Box::new(7)` allocates storage for one integer. Moving the `Box<i32>` does not move that integer to a new allocation, and the box is non-Copy even though `i32` is Copy. Dropping it destroys the value and frees its allocation. A box of a zero-sized type needs no allocation.
- Copy is an explicit type property, independent of value size. It requires Clone, every field must be Copy, and a type with a destructor cannot implement Copy. The String clone example allocates an independent buffer. Reference-counted pointer clones can share a resource, a later topic.
- After a partial move, initialized fields are still dropped. Moving fields out of a value that implements Drop is restricted because its destructor expects the complete value. Availability is checked across control-flow paths without proving the string-length comparison on slide 26.

## Destruction

- Ordinary owned locals drop at scope exit, not at their last use. Local bindings drop in reverse declaration order. Struct fields drop in declaration order. The Trace examples produce `2 end 1 ` and `1 body 2 `.
- Unwinding performs cleanup along the unwound path. Abort, process exit and deliberate leaks can bypass destructors. Memory safety permits leaks.
- `_outer` and `_inner` are real bindings. Replacing either with the wildcard `_` changes the lifetime of the temporary in the Trace example.

## References and methods

- Shared references borrow ordinary data here. Interior mutability introduces additional protocols and belongs to a later lecture. `&mut T` is non-Copy. A shorter reborrow temporarily provides access through an existing mutable reference.
- In an inherent `impl`, `Self` names the implementing type. A method receives its instance through `self`, `&self` or `&mut self`. Method-call syntax inserts a borrow when required. The consuming Label example moves the String field out of the receiver.
- Ending a borrow does not drop its owner. The reborrow example deliberately uses no nested scope. Access through `outer` resumes after the last use of `inner`, and writing through it before another use of `inner` fails borrow checking.
- Stacked Borrows is an experimental aliasing model, not the compiler's borrow-checking algorithm or a normative Rust specification. Miri supports it alongside Tree Borrows. The diagram shows only the relevant exclusive permissions, not the full stack or the rules for shared references, raw pointers and protected tags. A write through `outer` invalidates the newer `inner` permission in this example. A lexical scope boundary is not a stack-pop operation in the model. Compiler rejection and a dynamic model violation are different mechanisms.

## Slices and lifetime elision

- String indices are byte offsets at UTF-8 boundaries. The examples use ASCII to make the boundary unambiguous. `String::len` counts bytes. A slice contains a data pointer and a length.
- Lifetime parameters express relationships between borrows. They do not change how long an owned value exists. In `choose`, both input borrows must be usable for the chosen `'a`, but the owners need not have identical scopes.
- Elision uses the signature, not an inspection of the function body. Distinct elided input lifetimes get distinct parameters. A single input lifetime is assigned to elided output lifetimes.
- With `&self` or `&mut self`, the receiver lifetime is assigned to elided output lifetimes even when other arguments have borrowed types. The `before` method borrows its result from `self.value`, not from `separator`. Returning a slice of the separator instead would require an explicit relationship to its lifetime.

## Checks

There are 41 standalone examples with 30 checked outputs and 11 intentional compiler failures. Run `verify-examples.lisp` on Rust 1.88.0, edition 2021. The receiver-elision comparison and ownership-signature overview are declaration fragments.

## Sources

- [The Rust Book, chapter 4](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)
- [Structs and methods](https://doc.rust-lang.org/book/ch05-00-structs.html)
- [Place expressions and value expressions](https://doc.rust-lang.org/reference/expressions.html#place-expressions-and-value-expressions)
- [Destructors and drop scopes](https://doc.rust-lang.org/reference/destructors.html)
- [Copy](https://doc.rust-lang.org/std/marker/trait.Copy.html), [Clone](https://doc.rust-lang.org/std/clone/trait.Clone.html), [drop](https://doc.rust-lang.org/std/mem/fn.drop.html)
- [String representation](https://doc.rust-lang.org/std/string/struct.String.html#representation), [Vec guarantees](https://doc.rust-lang.org/std/vec/struct.Vec.html#guarantees), [Box](https://doc.rust-lang.org/std/boxed/index.html)
- [Lifetime elision](https://doc.rust-lang.org/reference/lifetime-elision.html)
- [Stacked Borrows model](https://github.com/rust-lang/unsafe-code-guidelines/blob/master/wip/stacked-borrows.md)
- [Miri](https://github.com/rust-lang/miri)
