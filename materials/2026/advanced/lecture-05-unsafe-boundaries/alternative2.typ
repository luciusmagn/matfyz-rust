#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 5],
  config-info(
    title: [Advanced Rust (2026): Unsafe Rust],
    subtitle: [Lecture 5],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Unsafe Rust

== Safe Rust Guarantees

Safe Rust makes the following set of promises, it forbids/prevents: #pause

- dereferencing dangling or unaligned pointers #pause
- observing uninitialized memory through safe APIs #pause
- mutable aliasing violations #pause
- data races #pause
- producing values that violate type invariants

#pause

Rust does *not* prevent memory leaks, deadlocks, logic bugs, `Rc` cycles, and infinite loops, though.

== `unsafe` Contract

`unsafe`  more of a contract and not "turn off the rules." #pause

We tell the compiler: "I have verified properties you cannot check. Hold me to everything else." #pause

Type system, the borrow checker, lifetime rules are all still active inside `unsafe`. #pause

We gain access to five specific operations the compiler cannot verify on its own, however

== Five Unsafe Operations

+ Dereferencing raw pointers
+ Calling `unsafe fn`
+ Accessing or mutating `static mut`
+ Implementing `unsafe trait`
+ Reading union fields

== `unsafe` keyword places

- `unsafe fn`: the *caller* must satisfy extra preconditions #pause
- `unsafe { ... }`: the author asserts specific operations are sound here #pause
- `unsafe trait`: implementors promise semantic guarantees the compiler cannot check #pause
- `unsafe impl`: you claim those guarantees hold for your type #pause

`unsafe fn` pushes proof obligations outward; an `unsafe` block absorbs them inward.

== Attitude

Gjengset: most Rust programmers either treat unsafe with fear or dismiss it as "just C." #pause

Both notions are incorrect. #pause

- Unsafe Rust can be *harder* than C #pause
- The C compiler makes few promises, so it assumes little #pause
- In Rust, the optimizer relies on strong guarantees #pause
  - `unsafe` code must uphold all of them, even though the compiler cannot check

= Unsafe usecases

== Hardware

```rust
const MMIO_REG: *mut u32 = 0x1000_0000 as *mut u32;

unsafe fn poke(value: u32) {
    std::ptr::write_volatile(MMIO_REG, value);
}
```
#pause

- The compiler is not psychic, it has no model of your memory map. #pause
- It cannot know whether `0x1000_0000` is a valid device register. #pause

HAL crates hide this from application code. But somebody has to write the HAL.

== Example numero dos: VGA Buffer

```rust
const VGA_PTR: *mut u8 = 0x8080_8080 as *mut u8;

let vga = unsafe {
    std::slice::from_raw_parts_mut(VGA_PTR, 800 * 600)
};
```
#pause

- Safety depends entirely on facts outside the program #pause
    - is this address mapped?
    - Is there an OS managing it?
    - Are we the only accessor? #pause

E.g. On baremetal before boot, this is fine. In a userspace process, it's nonsense

== FFI

```rust
extern "C" {
    fn strcpy(
        dest: *mut libc::c_char,
        src: *const libc::c_char,
    ) -> *mut libc::c_char;
}
```
#pause

The Rust compiler is still not psychic and cannot inspect the implementation of `strcpy`.
It doesnt know that `strcpy` requires a null terminator, that it writes past `dest` if the buffer is too small,
or that the source and destination must not overlap. #pause

Rustc sees is a function signature and a promise that you will call it correctly

== Safe Wrappers

```rust
fn safe_strcpy(dest: &mut [u8], src: &str) -> Result<(), &'static str> {
    if dest.len() <= src.len() {
        return Err("destination buffer too small");
    }
    let c_src = std::ffi::CString::new(src)
        .expect("interior null byte");
    unsafe {
        strcpy(
            dest.as_mut_ptr() as *mut libc::c_char,
            c_src.as_ptr(),
        );
    }
    Ok(())
}
```

We minimize the unsafe part. The safe wrapper encodes the preconditions as types and checks

== Concurrency Primitives

`Mutex<T>` hands out `&mut T` through `&self`. #pause

That looks like it violates the aliasing rule: you have `&Mutex<T>` (shared) and produce `&mut T` (exclusive).#pause

We use `UnsafeCell<T>`, the *only* legitimate way to get `&mut` from `&` in Rust. All interior-mutability types
(`Cell`, `RefCell`, `Mutex`, `RwLock`, `AtomicU64`) use it. #pause

- The lock protocol ensures exclusivity at runtime.
- The `unsafe` block is where you assert that the protocol works.

== Borrow Checker not being smart enough

```rust
fn next(buffer: &mut String) -> &str {
    loop {
        let event = parse(buffer);
        if true { return event; }
    }
}
fn parse(buffer: &mut String) -> &str { unimplemented!() }
```
#pause

This is correct, but the borrow checker rejects it because it cannot see that the early return ends the mutable borrow. #pause

== Performance

```rust
fn sum_safe(v: &[i32]) -> i32 {
    v.iter().copied().sum()
}

fn sum_raw(v: &[i32]) -> i32 {
    let (mut sum, ptr, len) = (0, v.as_ptr(), v.len());
    for i in 0..len {
        sum += unsafe { *ptr.add(i) };
    }
    sum
}
```
#pause

In this case, LLVM already eliminates the bounds check in `sum_safe`. The unsafe version buys nothing and costs an ongoing proof obligation.  #pause

= Undefined Behavior

== Types as Invariants

A type is a set of valid values: #pause

- `bool`: only `0` and `1`, not `2` or eg `0xFF` #pause
  - For instance, `Option<bool>` depends on this to encode into one byte #pause
- `char`: only valid Unicode scalar values #pause
- `!`: *no* values at all #pause
- `&T`: non-null, aligned, points to a valid `T`, no coexisting `&mut T` #pause

Producing a value outside these sets is UB the moment it exists

== Compiler Assumptions

The optimizer does not treat `unsafe` blocks specially.
It assumes *all* code respects Rust's rules: #pause

- References are non-null, aligned, dereferenceable, and point to valid data
- `&mut` is exclusive, `&` is shared, and the two never coexist for the same location
- Values match their type (a `bool` is 0 or 1, a `char` is a valid Unicode scalar, `!` has no values)
- No data races occur #pause

If any of these are violated, even inside `unsafe`, the optimizer is no longer constrained.

== Invalid Values

```rust
let raw: u8 = 2;
let flag: bool = unsafe { std::mem::transmute(raw) };
```
#pause

This is UB already at the `transmute`.
The `bool` type has exactly two inhabitants: `0` and `1`.
Producing a `bool` with value `2` is already a lie to the compiler. #pause

== Never Type

```rust
fn paradox() -> ! {
    unsafe { std::mem::transmute(()) }
}
```
#pause

`!` has zero inhabitants. Producing one is instant UB. The compiler may assume this function never returns.
And if it does, anything goes. #pause
== Aliasing

Consider this function:

```rust
fn compute(i: &u32, o: &mut u32) {
    if *i > 10 { *o = 1; }
    if *i > 5  { *o *= 2; }
}
```

== Aliasing

The compiler can transform it to:

```rust
fn compute(i: &u32, o: &mut u32) {
    let cached = *i;
    if cached > 10 { *o = 2; }
    else if cached > 5 { *o *= 2; }
}
```
#pause

- This is legal because `&u32` and `&mut u32` cannot alias in Rust, so they cannot point to the same thing. #pause
- The compiler is allowed to cache `*i` across the write to `*o`. #pause

In C, this optimization is *not* legal. Because `i` and `o` could point to the same `int`. Stricter default semantics can lead to better codegen. #pause

If your unsafe code violate this, the optimizer may produce wrong code.

== Reference Invariants

```rust
let ptr = 0x1usize as *const u8;
let r: &u8 = unsafe { &*ptr };
```
#pause

Forming the reference, not dereferencing it, is already the dangerous part. A `&u8` promises:
- Non-null
- Properly aligned
- Points to initialized, valid `u8`
- The referent is live for the reference's lifetime
- No `&mut u8` exists to this location simultaneously

== Packed Structs

```rust
#[repr(C, packed)]
struct Header { tag: u8, len: u16 }

let h = Header { tag: 1, len: 0x1234 };
// let r = &h.len; // UB: creates unaligned reference
let len = unsafe {
    std::ptr::addr_of!(h.len).read_unaligned()
};
```
#pause

`packed` removes padding, field accesses can become alignment issues.

= Raw Memory

== Manual Ownership

```rust
struct BufferHolder { buf: *mut i32, len: usize }

impl BufferHolder {
    fn new(n: usize) -> Self {
        let mut v = vec![0i32; n];
        let ptr = v.as_mut_ptr();
        std::mem::forget(v);      // ownership transfers to us
        Self { buf: ptr, len: n }
    }
}
```
#pause

`mem::forget` prevents `Vec`'s destructor from running. The allocation is now our problem: we must free it exactly once.

==

```rust
impl Drop for BufferHolder {
    fn drop(&mut self) {
        unsafe {
            // reconstruct the Vec so its Drop frees the allocation.
            // len == capacity because we allocated with vec![0; n].
            drop(Vec::from_raw_parts(self.buf, self.len, self.len));
        }
    }
}
```
#pause

If we get `len` or capacity wrong, or call this twice, or forget to implement `Drop` at all: double-free, use-after-free, or leak.

== `MaybeUninit`

Uninitialized memory is not "a `T` with unknown contents." It is *not yet a `T`*. #pause

```rust
use std::mem::MaybeUninit;

let mut slot = MaybeUninit::<u32>::uninit();
slot.write(42);
let val = unsafe { slot.assume_init() }; // now it's a u32
```
#pause

`assume_init()` is the exact point where you assert the value is valid. Before that line, reading it as `u32` would be UB.

== Panic Safety

```rust
impl<T> Vec<T> {
    fn extend_map<U, F: FnMut(&U) -> T>(
        &mut self, src: &[U], mut f: F,
    ) {
        self.reserve(src.len());
        unsafe { self.set_len(self.len() + src.len()); } // <-- !
        let mut dst = unsafe { self.as_mut_ptr().add(self.len() - src.len()) };
        for u in src {
            unsafe { std::ptr::write(dst, f(u)); }
            dst = unsafe { dst.add(1) };
        }
    }
}
```

== Panic Safety

- The `set_len` call tells `Vec` that all new slots are initialized *before* `f` has run. #pause
- If `f` panics on the third element, unwinding calls `drop()` on the `Vec`
- Destructor drops all `self.len()` elements (including slots that were never written to :() #pause
- Dropping uninitialized memory is obviously UB
- The bug doesn't show on the happy path and only surfaces when (even unrelated) code panics #pause

The fix: increment the length *after* each successful write, not before the loop.
(Or use a drop guard that resets the length on panic.)

= Safe Abstractions

== Caller Obligations

If your function signature has no `unsafe`, every caller is
entitled to assume it cannot cause UB, and they can #pause

- can call it with any value the type permits
- can panic in callbacks
- can clone, drop, or leak the result
- can do all of this from multiple threads if `Send`/`Sync` allow it #pause

== Module-Wide Proof Surface

We cannot be auditing only the `unsafe { }` blocks. #pause

Safe code in the *same module* can invalidate the invariants that unsafe code depends on.
Constructors, setters, `Clone`, `Drop`, and anything that touches the private fields are all part of the proof. #pause

```rust
mod my_vec {
    pub struct MyVec<T> { ptr: *mut T, len: usize, cap: usize }

    // this safe fn can break the unsafe code in `push` and `drop`
    // if it sets len > cap or len > initialized count.
    pub fn corrupt(&mut self) { self.len = 999; }
}
```

== Unsafe Traits

```rust
unsafe trait GlobalInvariant {
    // Implementors promise some property the compiler cannot check.
}

unsafe impl GlobalInvariant for MyType {
    // "I assert that MyType upholds the invariant."
}
```
#pause
`Send` and `Sync` are the canonical examples. The promise is not local, which means the consequences are not local either.

== Minimizing Unsafe Surface

- Small `unsafe` blocks are easier to audit #pause
- Extract raw operations into a safe local helper, then use the helper #pause

= Tooling

== Miri

Miri interprets Rust at the MIR level and checks safety properties at runtime: use-after-free, out-of-bounds access, uninitialized reads, aliasing violations (Stacked Borrows / Tree Borrows). #pause

#pause

Miri is not a proof of correctness, but it catches classes of bugs the compiler usually doesn't.

== Miri

```rust
struct Oops(Vec<u8>);
impl Oops {
    fn new() -> Self {
        let v = Vec::with_capacity(10);
        let (ptr, _, cap) = v.into_raw_parts();
        // Claims 10 elements exist, but nothing was written.
        let v = unsafe { Vec::from_raw_parts(ptr, 10, cap) };
        Self(v)
    }
}
fn main() {
    println!("{}", Oops::new().0[0]); // reads uninit memory
}
```
#pause
Miri will flag this

== Sanitizers

- *AddressSanitizer*: catches out-of-bounds heap/stack access, use-after-free, double-free (works with `RUSTFLAGS="-Zsanitizer=address"`) #pause
- *ThreadSanitizer*: catches data races across threads #pause
- *cargo-careful*: runs your code with extra debug assertions from the standard library enabled #pause

None of these *prove* correctness, these belong in CI along with other checks.
