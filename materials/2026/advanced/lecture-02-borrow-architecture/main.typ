#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 2],
  config-info(
    title: [Advanced Rust (2026): Borrow Checker, API Architecture],
    subtitle: [Lecture 2],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Core Model

== Ownership as Capability
- Think of ownership as a unique capability to mutate and eventually drop.
- Shared borrows temporarily split read capability from owner.
- Mutable borrows temporarily transfer exclusive capability.
- Compiler checks capability discipline statically.

== Aliasing XOR Mutability
- You can have aliasing (`&T`) or mutation (`&mut T`).
- You cannot have both at the same time for the same location.
- This rule is the source of many API-shape decisions.

== Region Inference
- Lifetimes are inferred regions, not runtime counters.
- Borrow checker reasons over control-flow graph.
- Each reference must remain valid over its whole use region.

== NLL (Non-Lexical Lifetimes)
- Lifetimes end at last use, not always at block end.
- NLL reduces false conflicts.
- It still does not permit alias + mutation violations.

```rust
let mut v = vec![1, 2, 3];
let first = &v[0];
println!("{first}"); // last use of first
v.push(4);           // allowed with NLL
```

== Reborrowing
- Borrowing from a borrow creates a derived borrow.
- Parent borrow is restricted while child borrow is active.
- This is common with method chaining and iterators.

```rust
let mut x = 10;
let a: &mut i32 = &mut x;
let b: &mut i32 = &mut *a;
*b += 1;
*a += 2;
```

== Two-Phase Borrows
- Method receiver `&mut self` may use two-phase borrowing.
- Reservation happens before argument evaluation.
- Activation happens at call site.
- Explains why some seemingly conflicting calls compile.

```rust
let mut v = vec![1, 2, 3];
v.push(v.len()); // accepted via two-phase borrow rules
```

== Borrow Splitting in Structs
- Distinct fields can be borrowed independently.
- Field-level disjointness is tracked.

```rust
struct Pair { left: i64, right: i64 }

let mut p = Pair { left: 1, right: 2 };
let a = &mut p.left;
let b = &mut p.right;
*a += 10;
*b += 20;
```

== Borrow Splitting in Slices
- Compiler cannot infer disjointness from two indexes alone.
- Use API that encodes disjointness explicitly.

```rust
let mut xs = [1, 2, 3, 4];
let (a, b) = xs.split_at_mut(2);
a[0] += 10;
b[0] += 20;
```

== Temporary Values and Dangling
- Returning reference to temporary is rejected.
- Borrow must outlive returned reference.

```rust
// fn bad() -> &str {
//     let s = String::from("x");
//     &s
// }
```

= Lifetimes in APIs

== Elision Rules
- One input reference -> output may elide to it.
- Methods with `&self` often elide output to `self`.
- Multiple candidates require explicit lifetimes.

```rust
fn head(s: &str) -> &str {
    s.split_whitespace().next().unwrap_or("")
}
```

== Explicit Lifetime Coupling
- Explicit lifetimes declare coupling between inputs and outputs.
- They do not extend object lifetime.

```rust
fn pick<'a>(a: &'a str, b: &'a str, choose_a: bool) -> &'a str {
    if choose_a { a } else { b }
}
```

== HRTB (Higher-Ranked Trait Bounds)
- Needed when closure/function must work for any lifetime.
- Common in callback-heavy APIs and adapters.

```rust
fn apply_for_all<F>(f: F)
where
    F: for<'a> Fn(&'a str) -> usize,
{
    let _ = f("abc");
    let s = String::from("longer");
    let _ = f(&s);
}
```

== Variance Overview
- `&'a T` is covariant in `'a` and `T`.
- `&'a mut T` is covariant in `'a`, invariant in `T`.
- Invariance often blocks intuitive subtype substitutions.

== Why Invariance Matters
- If `&mut T` were covariant in `T`, type safety breaks.
- Invariance prevents writing wrong subtype through mutable ref.
- This appears in generic container API design.

== `impl Iterator + '_`
- Expresses borrowed iterator tied to receiver lifetime.
- Avoids explicit named lifetime in many cases.

```rust
struct Bag { xs: Vec<i64> }

impl Bag {
    fn evens(&self) -> impl Iterator<Item = i64> + '_ {
        self.xs.iter().copied().filter(|x| x % 2 == 0)
    }
}
```

= Design Patterns

== Owner/View Split
- Store owns memory (`Vec`, `String`, `Box`).
- View structs borrow from owner.
- Keeps allocation policy separate from traversal/query APIs.

```rust
struct Table { buf: String }

struct LineView<'a> {
    raw: &'a str,
}
```

== Stable Handle Pattern
- Return indices/keys instead of references when mutation follows.
- Avoid long borrow chains.

```rust
fn find_id(xs: &[String], needle: &str) -> Option<usize> {
    xs.iter().position(|x| x == needle)
}
```

== Borrow-Then-Compute Pitfall
- Holding borrow across heavy computation blocks other operations.
- Prefer compute-then-borrow or short inner scopes.

```rust
let idx = map.get(key).copied();
if let Some(i) = idx {
    values[i] += 1;
}
```

== Interior Mutability Boundary
- `Cell/RefCell` shift checks to runtime.
- Useful for local invariants, caches, graph wiring.
- Not a replacement for architectural clarity.

```rust
use std::cell::RefCell;

let cache = RefCell::new(Vec::<i64>::new());
cache.borrow_mut().push(10);
```

== `Rc<RefCell<T>>` Tradeoff
- Enables shared ownership + mutation in single-threaded graphs.
- Loses compile-time aliasing guarantees.
- Runtime panics possible on borrow rule violation.

== `Arc<Mutex<T>>` Parallel Analogue
- Thread-safe shared mutation boundary.
- Sync cost and deadlock risks become explicit concerns.
- API should hide lock mechanics where possible.

= Drop and Soundness Edges

== Drop Check (dropck)
- Destructor execution affects lifetime constraints.
- Type with `Drop` may require borrowed fields to outlive drop.
- Subtle when building self-referential or pinned structures.

== Self-Referential Struct Trap
- Moving value can invalidate internal self pointers.
- Plain Rust structs cannot safely contain references to themselves.
- Use indirection + pinning patterns instead.

== Pinning Motivation
- `Pin` prevents moves after pinning guarantee.
- Essential for self-referential async state machines.
- Pinning is about location stability, not mutability itself.

```rust
use std::pin::Pin;

fn consume_pinned(_x: Pin<&mut Vec<u8>>) {}
```

= API Case Studies

== Case Study: Parser API
- Bad API returns references to ephemeral parse buffer.
- Better API stores owned input and returns borrowed views.
- Best choice depends on expected pipeline lifetime.

== Case Study: Builder vs Borrowed Config
- Borrowed config avoids cloning during assembly.
- Final built object should usually own required data.
- Keep borrow complexity in construction phase, not runtime phase.

== Case Study: Batch Processing
- Input as `&[T]`, output owned summary.
- Avoid exposing internal references unless required.
- Improves call-site ergonomics and test stability.

```rust
fn summarize(xs: &[i64]) -> (i64, i64) {
    let sum = xs.iter().sum::<i64>();
    let max = xs.iter().copied().max().unwrap_or(0);
    (sum, max)
}
```

= Diagnostics Workflow

== Reading Borrow Errors
- Start from first error, not last.
- Identify conflicting borrows and their live ranges.
- Reduce live range first, clone last.

== Typical Fix Sequence
- Introduce local scope blocks.
- Replace reference return with owned return if contract allows.
- Split data structure into independently borrowed parts.
- Use handles/ids when mutation and lookup interleave.

== What Not To Do
- Do not blindly add `clone()` everywhere.
- Do not default to `Rc<RefCell<T>>` for every conflict.
- Do not hide lifetime bugs behind unnecessary heap allocation.

== Final Rules
- API lifetime story must be intentional, not accidental.
- Minimize borrow duration at call boundaries.
- Prefer explicit ownership transitions over implicit coupling.
- If it is hard to explain borrow semantics, redesign API shape.
