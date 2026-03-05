#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 5],
  config-info(
    title: [Advanced Rust (2026): Unsafe Rust, Soundness Boundaries],
    subtitle: [Lecture 5],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Unsafe Contract

== What `unsafe` Actually Means
- `unsafe` does not disable borrow checker globally.
- It permits specific operations that require manual proof.
- All UB-prevention obligations move to you.

== Five Unsafe Capabilities
- Dereference raw pointer.
- Call unsafe function.
- Access/modify mutable static.
- Implement unsafe trait.
- Access union field.

== Soundness Boundary Pattern
- Keep unsafe blocks tiny.
- State invariants in comments/docs near unsafe block.
- Expose safe API that enforces invariants.

== UB Is Global Contract Violation
- UB is not a normal runtime failure.
- Compiler may assume UB never happens and optimize accordingly.
- A single UB site can invalidate unrelated code assumptions.

= Raw Memory Primitives

== Raw Pointers
- `*const T` and `*mut T` are not references.
- No aliasing/lifetime guarantees by type alone.
- Dereference requires `unsafe`.

```rust
let x = 10_i64;
let p: *const i64 = &x;
unsafe {
    println!("{}", *p);
}
```

== `NonNull<T>`
- Encodes non-null guarantee.
- Still does not encode aliasing or lifetime correctness.
- Useful in collection internals.

```rust
use std::ptr::NonNull;

let mut x = 5_i32;
let p = NonNull::from(&mut x);
```

== `MaybeUninit<T>`
- Correct tool for uninitialized storage.
- Prevents accidental reads of uninitialized memory.
- Use `assume_init` only after full initialization proof.

```rust
use std::mem::MaybeUninit;

let mut slot = MaybeUninit::<i64>::uninit();
slot.write(42);
let v = unsafe { slot.assume_init() };
println!("{v}");
```

== `ManuallyDrop<T>`
- Disables automatic drop.
- Useful in custom ownership transitions.
- Easy source of double-drop if misused.

= Aliasing and Provenance

== Reference Rules Still Matter
- Creating `&mut T` asserts unique mutable access.
- Violating that assertion is UB even if code "seems to work".
- Unsafe code must respect high-level reference semantics.

== `UnsafeCell<T>`
- Only legal way to opt out of shared-reference immutability.
- Foundation for `Cell`, `RefCell`, atomics, mutex internals.
- Does not solve synchronization by itself.

```rust
use std::cell::UnsafeCell;

struct MyCell<T> {
    inner: UnsafeCell<T>,
}
```

== Pointer Provenance Intuition
- Pointer is not just integer address.
- It carries origin/allocation relation constraints.
- Integer round-tripping can violate provenance assumptions.

== Stacked-Borrows-Like Reasoning
- Think in terms of active borrow stack permissions.
- Creating new mutable reference may invalidate old aliases.
- Miri can help catch many violations experimentally.

= Drop and Layout Edges

== Drop Check (dropck)
- Drop order influences lifetime validity.
- Generic types with destructors need careful lifetime constraints.
- Hidden borrow in Drop can cause surprising compile failures.

== `repr(C)` and ABI
- `repr(C)` for C ABI/layout compatibility.
- `repr(Rust)` field order/layout is not stable ABI.
- `repr(transparent)` for wrapper ABI compatibility.

== Unions
- Union field access is unsafe because active variant is unchecked.
- You must track which field is valid by external invariant.

```rust
union U {
    i: u32,
    f: f32,
}
```

== Padding and Uninitialized Bytes
- Reading padding bytes is UB-sensitive territory in some contexts.
- Avoid bytewise assumptions about arbitrary `T` layout.
- Prefer explicit serialization logic.

= Unsafe Traits

== Why `Send`/`Sync` Can Be Unsafe to Implement
- Incorrect impl can break thread-safety guarantees globally.
- Must reason about all reachable interior state.
- Often safer to compose existing thread-safe primitives.

== Typical Unsafe Trait Checklist
- Aliasing model documented?
- Interior mutation synchronized?
- Drop path race-free?
- FFI callbacks thread constraints enforced?

= FFI Boundaries

== `extern "C"` Contracts
- ABI and calling convention must match exactly.
- Ownership transfer rules must be explicit on both sides.
- Panics must not unwind across non-Rust boundaries.

```rust
extern "C" {
    fn strlen(s: *const core::ffi::c_char) -> usize;
}
```

== Nullability and Ownership in FFI
- Use `*mut T` / `*const T` for nullable pointers.
- Convert to references only after checks.
- Clearly define who allocates and who frees.

== FFI Error Mapping
- C APIs often use sentinel values / errno.
- Wrap low-level protocol into Rust `Result` surface.
- Keep unsafe at boundary adapter layer.

= Case Studies

== Case: Safe Slice from Pointer
- Preconditions: non-null for non-empty, valid allocation, initialized elements.
- Lifetime must not outlive allocation.
- API should force caller to acknowledge preconditions.

```rust
unsafe fn view<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
    std::slice::from_raw_parts(ptr, len)
}
```

== Case: Bounded Buffer Wrapper
- Internal storage may use unsafe indexing for speed.
- Public methods must enforce bounds/invariants.
- Unit tests must target boundary transitions aggressively.

== Case: Lock-Free Node Reclamation
- ABA and reclamation become central correctness issue.
- Ownership alone does not solve reclamation races.
- Epoch/hazard strategies are required in practice.

= Verification Workflow

== Unsafe Code Review Template
- Invariant list.
- Why invariant holds at this point.
- What could invalidate invariant.
- Which tests/benchmarks cover boundary.

== Dynamic Tools
- Miri for aliasing and UB classes.
- Sanitizers for memory/thread issues in compatible builds.
- Fuzzers for boundary API misuse paths.

== Documentation Requirements
- `# Safety` section for every unsafe fn.
- State caller obligations in precise terms.
- Include minimal valid and invalid usage examples.

= Final Rules

== Engineering Rules
- Unsafe core should be minimal and local.
- Safe wrapper must encode invariants by API shape.
- Do not rely on "works in tests" as proof.
- If invariant proof is unclear, redesign before shipping.
