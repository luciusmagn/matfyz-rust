#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 2],
  config-info(
    title: [Advanced Rust (2026): Borrow Checker, Lifetimes, Variance],
    subtitle: [Lecture 2],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Ownership and References

== Ownership: Core Invariant
- Every value has exactly one owner.
- Value is dropped when owner goes out of scope.
- Ownership can be moved.
- Borrowing is temporary non-owning access.

== Reference Kinds
- Shared reference: `&T`
  - multiple aliases allowed
  - no mutation through this reference
- Mutable reference: `&mut T`
  - unique access requirement
  - mutation allowed

== Two Fundamental Rules
- A reference cannot outlive its referent.
- A mutable reference cannot be aliased.
- These constraints prevent use-after-free and data races.

== Returning Reference to Temporary
```rust
fn as_str(data: &u32) -> &str {
    let s = format!("{}", data);
    // invalid: s is dropped at function end
    &s
}
```
- Borrow checker rejects this at compile time.

== Aliasing + Reallocation Hazard
```rust
let mut data = vec![1, 2, 3];
let x = &data[0];

data.push(4); // may reallocate
println!("{}", x);
```
- Borrow checker prevents potential dangling reference.

= Borrow Checker Analysis

== What Borrow Checker Does
- Tracks ownership and borrow states across control flow.
- Validates lifetime and aliasing constraints.
- Rejects programs with potential memory-unsafe executions.
- Cost is compile-time only.

== Simplified Analysis Pipeline
- Build control-flow graph.
- Propagate borrow state facts per program point.
- Check aliasing and lifetime constraints.
- Emit diagnostics at violating points.

== NLL (Non-Lexical Lifetimes)
- Lifetime ends at last use, not always block end.
- Reduces unnecessary borrow conflicts.
- Does not relax soundness constraints.

```rust
let mut v = vec![1, 2, 3];
let first = &v[0];
println!("{first}");

v.push(4); // allowed after last use of first
```

== Reborrowing Model
- Borrow from borrow creates derived permission.
- Parent borrow is restricted while derived borrow is active.

```rust
let mut x = 10;
let a = &mut x;
let b = &mut *a;

*b += 1;
*a += 2;
```

== Two-Phase Borrow Example
- First phase: reservation.
- Second phase: activation.
- Enables patterns like method call with shared-then-mutable access.

```rust
let mut v = vec![1, 2, 3];
v.push(v.len());
```

= Lifetimes

== Lifetime Meaning
- Lifetime is a region where reference is valid.
- Lifetimes are compile-time reasoning artifacts.
- They express relationship constraints in signatures.

== Explicit Lifetime Coupling
```rust
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```
- Output lifetime is constrained by both inputs.

== Lifetime Elision (Desugaring Idea)
```rust
fn first_word(s: &str) -> &str {
    s.split_whitespace().next().unwrap_or("")
}
```
==
```rust
fn first_word<'a>(s: &'a str) -> &'a str {
    s.split_whitespace().next().unwrap_or("")
}
```

== Elision Rules (Practical)
- Each input reference gets its own lifetime.
- If exactly one input lifetime exists, output may elide to it.
- For methods, `&self` lifetime can be used for output.
- Otherwise explicit output lifetime needed.

== Unbounded Lifetime in Unsafe
```rust
fn get_str<'a>(ptr: *const String) -> &'a str {
    unsafe { &*ptr }
}
```
- This can manufacture overly-permissive lifetimes.
- Unsafe boundaries must rebind lifetime constraints immediately.

== `'static` Distinction
- `&'static T`: referent valid for program duration.
- `T: 'static`: type has no non-static reference dependency.
- These are related but not identical concepts.

= Subtyping and Variance

== Lifetime Subtyping
- If `'long` fully contains `'short`, then `'long <: 'short`.
- Longer-lived reference can be used where shorter is expected.

```rust
let hello: &'static str = "hello";
{
    let local = String::from("x");
    let short = &local;
    dbg!(hello, short);
}
```

== Variance Definitions
- Covariant: subtyping preserved.
- Contravariant: subtyping reversed.
- Invariant: no subtyping relationship propagated.

== Common Variance Cases
- `&'a T`: covariant in `'a`, covariant in `T`.
- `&'a mut T`: covariant in `'a`, invariant in `T`.
- `Vec<T>`: covariant in `T`.
- `Cell<T>`: invariant in `T`.
- `fn(T) -> U`: contravariant in `T`, covariant in `U`.

== Why `&mut T` is Invariant
```rust
fn assign<T>(input: &mut T, val: T) {
    *input = val;
}

let mut hello: &'static str = "hello";
```
==
```rust
{
    let world = String::from("world");
    // if &mut were covariant in T, this would be unsound
    // assign(&mut hello, &world);
}
println!("{hello}");
```
- Invariance prevents storing short-lived refs into long-lived slots.

== Contravariance in Function Arguments
```rust
fn store(input: &'static str) {
    let _ = input;
}

fn demo<'a>(input: &'a str, f: fn(&'a str)) {
    f(input);
}
```
==
```rust
fn main() {
    let local = String::from("local");
    // demo(&local, store); // rejected
}
```
- Function accepting only `'static` cannot stand in for one accepting any `'a`.

= Borrow Splitting and Iteration

== Field-Level Borrow Splitting
```rust
struct Point { x: i32, y: i32 }

let mut p = Point { x: 0, y: 0 };
let x = &mut p.x;
let y = &mut p.y;
*x += 10;
*y += 20;
```
- Borrow checker understands disjoint struct fields.

== Container Splitting Limits
```rust
let mut arr = [1, 2, 3];
let a = &mut arr[0];
let b = &mut arr[1]; // rejected
```
- Index expressions do not automatically prove disjointness.

== Explicit Disjointness via API
```rust
let mut arr = [1, 2, 3, 4, 5];
let (left, right) = arr.split_at_mut(2);
left[0] += 10;
right[0] += 20;
```

== Mutable Iterator Subtlety
- `Iterator::next(&mut self)` can yield `&mut` items safely.
- Safety relies on one-shot consumption guarantee.
- Iterator implementation must ensure no aliasing of yielded refs.

= Stacked Borrows Perspective

== Why This Model Matters
- Gives formal aliasing interpretation for unsafe code reasoning.
- Clarifies when pointer/reference accesses are permitted.
- Helps explain Miri diagnostics.

== Permission Stack Intuition
- Memory location tracks permission stack.
- New mutable borrow pushes exclusive permission.
- Certain accesses invalidate older permissions.

== Example of Invalidation
```rust
let mut x = 10;
let r1 = &mut x;
*r1 = 20;

let r2 = &*r1;
println!("{}", *r2);
```
==
```rust
*r1 = 30;
// Using r2 after this may violate stacked-borrows rules
```

== Miri as Practical Tool
- Executes MIR with UB checks including aliasing classes.
- Not complete formal verifier, but very useful detector.
- Especially valuable for unsafe abstraction testing.

= API Architecture with Borrow Checker

== Owner/View Split
- Owner struct keeps allocation and mutability rights.
- View APIs expose borrowed projection types.
- Reduces accidental lifetime coupling in higher layers.

```rust
struct Buffer {
    data: String,
}

impl Buffer {
    fn as_str(&self) -> &str { &self.data }
}
```

== Handle-Based Update Pattern
- Return IDs/indices instead of long-lived references.
- Re-borrow only at mutation site.
- Avoids global borrow entanglement.

== Higher-Rank Callbacks in APIs
```rust
fn call_with_ref<F>(f: F)
where
    F: for<'a> Fn(&'a i32) -> &'a i32,
{
    let x = 10;
    let y = f(&x);
    println!("{y}");
}
```
- Useful when callback must be lifetime-polymorphic.

= Diagnostics Workflow

== Reading Borrow Errors Effectively
- First error is usually root cause.
- Identify conflicting borrows and their live ranges.
- Minimize live range before structural escalation.

== Resolution Order
- Introduce narrower scopes.
- Split data into disjoint borrows.
- Use handle/ID indirection.
- Use interior mutability only when semantic fit is explicit.

== Quality Target
- Lifetime relationships should be evident from API signatures.
- Caller should not need hidden ownership assumptions.
- Borrow checker should confirm design, not fight accidental coupling.
