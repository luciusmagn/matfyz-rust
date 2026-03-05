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

== Ownership
- Every value has exactly one owner #pause
- Value is dropped when owner goes out of scope #pause
- Ownership can be moved #pause
- Borrowing is temporary non-owning access

== Reference Kinds
- Shared/Immutable reference: `&T` #pause
  - multiple aliases allowed #pause
  - no mutation through this reference #pause
- Exclusive/Mutable reference: `&mut T` #pause
  - unique access requirement #pause
  - mutation allowed

== Rules
1. A reference cannot outlive its referent
2. A mutable reference cannot be aliased

These constraints prevent use-after-free and data races.

== Example

```rust
fn as_str(data: &u32) -> &str {
    let s = format!("{}", data);
    // invalid: s is dropped at function end
    &s
}
```

Borrow checker rejects this at compile time.

== Aliasing
```rust
let mut data = vec![1, 2, 3];
let x = &data[0];

data.push(4); // may reallocate
println!("{}", x);
```

Borrow checker prevents potential dangling reference.

= Borrow Checker

==
- Tracks ownership and borrow states across control flow #pause
- Validates lifetime and aliasing constraints #pause
- Rejects programs with potential memory-unsafe executions #pause
- Cost is compile-time only #pause
- Paper: Stacked Borrows: An Aliasing Model for Rust

== Simplified Pipeline
- Build control-flow graph #pause
- Propagate borrow state facts per program point #pause
- Check aliasing and lifetime constraints #pause
- Emit diagnostics at violating points

== NLL (Non-Lexical Lifetimes)
- Lifetime ends at last use, not always block end #pause
- Reduces unnecessary borrow conflicts #pause
- Does not relax soundness constraints #pause
- Integrated relatively recently #pause
- Does not apply to Drop types

```rust
let mut v = vec![1, 2, 3];
let first = &v[0];
println!("{first}");

v.push(4); // allowed after last use of first
```

== Reborrowing Model
- Borrow from borrow creates derived permission #pause
- Parent borrow is restricted while derived borrow is active

```rust
let mut x = 10;
let a = &mut x;
let b = &mut *a;

*b += 1;
*a += 2;
```

= Lifetimes

==
- Lifetime is a region where reference is valid #pause
- Lifetimes are compile-time reasoning artifacts

== Explicit Lifetime Coupling
```rust
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```
- Output lifetime is constrained by both inputs

== Lifetime Elision
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

== Elision Rules
- Each input reference gets its own lifetime #pause
- If exactly one input lifetime exists, output may elide to it #pause
- For methods, `&self` lifetime can be used for output #pause
- Otherwise explicit output lifetime needed

== Unbounded Lifetime in Unsafe
```rust
fn get_str<'a>(ptr: *const String) -> &'a str {
    unsafe { &*ptr }
}
```
- This can manufacture overly-permissive lifetimes #pause
- Unsafe boundaries must rebind lifetime constraints immediately

== `'static`
- `&'static T`: referent valid for program duration #pause
- `T: 'static`: type has no non-static reference dependency

= Subtyping and Variance

== Lifetime Subtyping
- If `'long` fully contains `'short`, then `'long <: 'short` #pause
- Longer-lived reference can be used where shorter is expected

== Lifetime Subtyping
```rust
fn debug<'a>(a: &'a str, b: &'a str) {
    println!("a = {a:?} b = {b:?}");
}

fn main() {
    let hello: &'static str = "hello";
    {
        let world = String::from("world");
        let world = &world; // 'world has a shorter lifetime than 'static
        debug(hello, world);
    }
}
```

==
- We're assigning `&world` to `hello` (via `input`) #pause
- `&world` has a shorter lifetime than `&'static str` #pause
- If allowed, it would create a dangling reference #pause
- `&mut T` is invariant over `T` to prevent this exact problem

== Variance Definitions
- Covariant: subtyping preserved #pause
- Contravariant: subtyping reversed #pause
- Invariant: no subtyping relationship propagated

== Common Variance Cases
- `&'a T`: covariant in `'a`, covariant in `T` #pause
- `&'a mut T`: covariant in `'a`, invariant in `T` #pause
- `Vec<T>`: covariant in `T` #pause
- `Cell<T>`: invariant in `T` #pause
- `fn(T) -> U`: contravariant in `T`, covariant in `U`

== Function Pointers and Contravariance
Function arguments are contravariant:

```rust
fn store(input: &'static str) {
    // Stores in a collection requiring 'static values
}

fn demo<'a>(input: &'a str, f: fn(&'a str)) {
    f(input);
}

fn main() {
    let local = String::from("local");
    // Error! Can't pass store (requires 'static) where
    // a function accepting any lifetime is expected
    demo(&local, store);
}
```

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

Borrow checker understands disjoint struct fields.

== Container Splitting Limits
```rust
let mut arr = [1, 2, 3];
let a = &mut arr[0];
let b = &mut arr[1]; // rejected
```
Index expressions are not automatically considered disjoint

== Explicit Disjointness via API
```rust
let mut arr = [1, 2, 3, 4, 5];
let (left, right) = arr.split_at_mut(2);
left[0] += 10;
right[0] += 20;
```

== Mutable Iterators
- `Iterator::next(&mut self)` can yield `&mut` items safely #pause
- Safety relies on one-shot consumption guarantee #pause
- Iterator implementation must ensure no aliasing of yielded refs

= Stacked Borrows

==
- Gives formal aliasing interpretation for unsafe code reasoning #pause
- Clarifies when pointer/reference accesses are permitted #pause
- Helps explain Miri diagnostics

== The Permission Stack
For each memory location:
- Permissions are tracked in a stack #pause
- Each new reference operation pushes to the stack #pause
- Creating a `&mut` invalidates all other access #pause
- When references go out of scope, they're popped #pause
- Accessing memory requires appropriate permission

== Stacked Borrows Example
```rust
let mut x = 10;
let r1 = &mut x;  // Stack: [r1]
*r1 = 20;         // OK, r1 has permission
let r2 = &*r1;    // Stack: [r1, r2]
println!("{}", *r2); // OK, r2 has permission 
*r1 = 30;         // Stack: [r1], r2 invalidated
// Using r2 here would be UB under stacked borrows
```

== Higher-Rank Trait Bound

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
- Useful when callback must be lifetime-polymorphic

= Questions?
