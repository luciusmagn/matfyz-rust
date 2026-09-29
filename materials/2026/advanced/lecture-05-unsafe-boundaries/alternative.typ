#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 5],
  config-info(
    title: [Advanced Rust (2026): Unsafe Rust],
    subtitle: [Lecture 5 · Alternative],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Unsafe Rust

== Unsafe Rust
- Rust has safe Rust and unsafe Rust #pause
- `unsafe` does not disable the type system #pause
- It does let you perform operations the compiler cannot justify on its own #pause
- The price is that proof obligations move from compiler to programmer

== Safe Rust
- Safe code cannot dereference dangling references #pause
- Safe code cannot observe uninitialized values through safe APIs #pause
- Safe code cannot violate reference aliasing rules #pause
- Safe code cannot cause unsynchronized write races #pause
- Safe code can still leak memory, deadlock, or compute nonsense

== Forms of `unsafe`
- `unsafe fn`: caller must satisfy extra preconditions #pause
- `unsafe trait`: implementer promises extra semantic guarantees #pause
- `unsafe impl`: you assert that those guarantees really hold #pause
- `unsafe { ... }`: local escape hatch for specific operations

== What `unsafe` Permits
- Dereferencing raw pointers #pause
- Calling unsafe functions #pause
- Accessing union fields #pause
- Accessing or mutating mutable statics #pause
- Implementing unsafe traits

== Why We Have Unsafe
- Some correct programs are beyond what the compiler can prove #pause
- Hardware and FFI live outside the compiler's model #pause
- Concurrency primitives and low-level collections need raw capabilities #pause
- Performance sometimes matters, though this is the weakest reason unless measured

= Why Unsafe Exists

== Hardware
```rust
const DEVICE_REGISTER_ADDRESS: *mut u32 = 0x1000_0000 as *mut u32;

unsafe fn write_to_device_register(value: u32) {
    std::ptr::write_volatile(DEVICE_REGISTER_ADDRESS, value);
}

fn main() {
    unsafe {
        write_to_device_register(0xABCD_EF01);
    }
}
```

== Hardware Notes
- The compiler cannot know whether that address is mapped or valid #pause
- It cannot know whether concurrent access is allowed #pause
- On a hosted OS this is usually nonsense #pause
- On bare metal this may be exactly the intended code

== Another Hardware Example
```rust
const VGA_PTR: *mut u8 = 0x8080_8080 as *mut u8;

let vga = unsafe {
    std::slice::from_raw_parts_mut(VGA_PTR, 800 * 600)
};
```
- Safety depends on external facts about the machine #pause
- HALs hide this from most users #pause
- The HAL itself still has to be written

== Foreign Functions
```rust
extern "C" {
    fn strcpy(
        dest: *mut libc::c_char,
        src: *const libc::c_char,
    ) -> *mut libc::c_char;
}
```
- Rust cannot inspect what the foreign function actually does

== Safe Wrapper Around FFI
```rust
fn safe_strcpy(dest: &mut [u8], src: &str) -> Result<(), &'static str> {
    let src_len = src.len();
    if dest.len() <= src_len {
        return Err("destination buffer is too small");
    }
```
==
```rust
    let c_src = std::ffi::CString::new(src).expect("CString::new failed");
    let src_ptr = c_src.as_ptr();
    let dest_ptr = dest.as_mut_ptr() as *mut libc::c_char;

    unsafe { strcpy(dest_ptr, src_ptr); }
    Ok(())
}
```

== Concurrency Primitives
- `Mutex<T>` can manufacture `&mut T` from `&self` #pause
- The compiler cannot see the lock protocol in the machine state #pause
- Unsafe code bridges from raw synchronization to safe API #pause
- This is also why `UnsafeCell<T>` exists

== Borrow Checker Limitations
```rust
fn next(buffer: &mut String) -> &str {
    loop {
        let event = parse(buffer);
        if true {
            return event;
        }
    }
}

fn parse(buffer: &mut String) -> &str { unimplemented!() }
```
- There are correct programs the borrow checker does not yet accept #pause
- Reaching for `unsafe` here should be the exception #pause
- Usually a redesign is better than overriding the borrow checker

== Borrow Checker Notes
- This is one place where people are tempted to say "I know better" #pause
- Often they do not #pause
- Sometimes they do, but the burden of proof becomes entirely manual #pause
- Future work such as Polonius may accept more patterns without unsafe

== Performance Example
```rust
fn sum_vector_safe(v: &[i32]) -> i32 {
    let mut sum = 0;
    for &item in v.iter() {
        sum += item;
    }
    sum
}
```
==
```rust
fn sum_vector_unsafe(v: &[i32]) -> i32 {
    let mut sum = 0;
    let len = v.len();
    unsafe {
        let ptr = v.as_ptr();
        for i in 0..len {
            sum += *ptr.add(i);
        }
    }
    sum
}
```

== Performance Notes
- Unsafe for performance is the most controversial category #pause
- The benefit is often marginal #pause
- The maintenance cost is not marginal #pause
- Measure first, then decide

= Values and UB

== Compiler Assumptions
- The compiler assumes all code, including unsafe code, respects Rust's rules #pause
- It assumes no dangling dereference, no broken aliasing, no invalid values, and no unsynchronized write races #pause
- Optimizations are built on these assumptions #pause
- If the assumptions are false, the optimizer is no longer constrained by source-level intuition

== Valid Values
- A type is not just a bag of bits #pause
- `bool` only admits `0` and `1` #pause
- `!` admits no values at all #pause
- References must be non-null, aligned, and point to a valid referent

== Invalid Value Example
```rust
let raw = 2_u8;
let flag: bool = unsafe { std::mem::transmute(raw) };
println!("{flag}");
```
- The problem is not the `println!` #pause
- The problem is that the program produced an invalid `bool` at all

== Never Type Example
```rust
fn paradox() -> ! {
    unsafe { std::mem::transmute(()) }
}
```
- The never type has no values #pause
- Producing one is undefined behavior #pause
- UB is not an exception mechanism, it is loss of semantic footing

== References Carry Promises
```rust
let ptr = 0x1usize as *const u8;
let r: &u8 = unsafe { &*ptr };
println!("{}", *r);
```
- Forming a reference already claims more than "I have an address" #pause
- It claims alignment, liveness, and validity of the referent

== Aliasing Example
```rust
fn compute(i: &u32, o: &mut u32) {
    if *i > 10 {
        *o = 1;
    }

    if *i > 5 {
        *o *= 2;
    }
}
```
==
```rust
fn compute(i: &u32, o: &mut u32) {
    let cached_i = *i;
    if cached_i > 10 {
        *o = 2;
    } else if cached_i > 5 {
        *o *= 2;
    }
}
```

== Why This Rewrite Is Legal
- Rust references carry non-aliasing assumptions the optimizer may use #pause
- In C this sort of rewrite is much more constrained #pause
- In Rust unsafe code must not manufacture references that invalidate these assumptions

== Packed Layout and Unaligned Access
```rust
#[repr(C, packed)]
struct Header {
    tag: u8,
    len: u16,
}

let header = Header { tag: 1, len: 0x1234 };
```
==
```rust
// let r = &header.len; // would form an unaligned reference
let len = unsafe { std::ptr::addr_of!(header.len).read_unaligned() };
println!("{len}");
```
- Packed layout often turns innocent field access into an unsafe-layout problem #pause
- This is one reason field-offset tricks are subtle

= Raw Memory

== Transmuting References
```rust
use std::mem;

struct BufferHolder {
    buffer: *mut i32,
    len: usize,
}
```
==
```rust
impl BufferHolder {
    fn new(size: usize) -> Self {
        let mut vec = vec![0; size];
        let buffer_ptr = vec.as_mut_ptr();
        mem::forget(vec);
        BufferHolder { buffer: buffer_ptr, len: size }
    }
```
==
```rust
    unsafe fn get_buffer_mut<'a>(&'a self) -> &'a mut [i32] {
        assert!(!self.buffer.is_null(), "buffer is null");
        std::mem::transmute::<(*mut i32, usize), &'a mut [i32]>((self.buffer, self.len))
    }
}
```

== Manual Drop Repair
```rust
impl Drop for BufferHolder {
    fn drop(&mut self) {
        unsafe {
            let _vec = Vec::from_raw_parts(self.buffer, self.len, self.len);
        }
    }
}
```
- Once we step outside ordinary ownership, destruction becomes our problem #pause
- This is where double-free and leak bugs appear easily

== Memory Leak Is Not UB
```rust
use std::mem;

pub fn strify(s: String) -> &'static str {
    let s_ref = s.as_str();
    let s_static_ref: &'static str = unsafe { mem::transmute(s_ref) };
    mem::forget(s);
    s_static_ref
}
```
- This leaks memory #pause
- It does not immediately cause UB #pause
- Safe Rust can leak too, for example through `Rc` cycles

== `MaybeUninit`
- Uninitialized memory is not a `T` that merely holds unknown contents #pause
- It is memory that has not yet become a `T` #pause
- `MaybeUninit<T>` is the way to represent that honestly #pause
- Without it, code tends to lie to the compiler too early

== `MaybeUninit` Example
```rust
use std::mem::MaybeUninit;

let mut slot = MaybeUninit::<u32>::uninit();
slot.write(42);
let value = unsafe { slot.assume_init() };
println!("{value}");
```
- `assume_init` is exactly the point where you assert the invariant #pause
- Before that, the value is not a `u32` yet

== Panic Safety in Unsafe Code
```rust
impl<T> Vec<T> {
    fn extend_map<U, F>(&mut self, us: &[U], mut f: F)
    where
        F: FnMut(&U) -> T,
    {
        self.reserve(us.len());
        let cur_len = self.len();
        unsafe { self.set_len(cur_len + us.len()); }
```
==
```rust
        let mut into = unsafe { self.as_mut_ptr().add(cur_len) };
        for u in us {
            unsafe {
                std::ptr::write(into, f(u));
                into = into.add(1);
            }
        }
    }
}
```

== What Goes Wrong Here
- If `f(u)` panics halfway through, `Vec` already believes all new elements exist #pause
- Unwinding may then drop uninitialized elements #pause
- The bug only appears on exceptional control flow #pause
- This is why unsafe code must reason about panic paths too

== Deferred Drop Is Also Tricky
- Delaying destruction can be attractive for performance #pause
- It can also break lifetime assumptions in subtle ways #pause
- If `T` contains references, "drop later" is already a semantic promise #pause
- Safe callers can feed your unsafe structure values you did not anticipate

= Safe Abstractions

== Safe Wrappers Must Be Fully Safe
- A safe wrapper around unsafe internals must be correct for all safe callers #pause
- It is not enough that it works for intended usage #pause
- The caller is allowed to be weird, generic, panicking, and inconvenient #pause
- If the API is safe, the burden is yours

== The Whole Module Matters
- Auditing only the `unsafe {}` lines is not enough #pause
- Safe code in the same module can violate the invariants the unsafe code relies on #pause
- Constructors, mutators, `Drop`, `Clone`, and panic paths are part of the proof #pause
- Private fields do not save you from bad safe APIs

== `SAFETY:` Comments
```rust
unsafe {
    // SAFETY: `ptr` points inside `self.buf`, `index < self.len`,
    // and this slot has not been initialized yet
    std::ptr::write(ptr.add(index), value);
}
```
- The comment should name the actual invariants #pause
- "SAFETY: this is safe" is useless #pause
- This is for future readers, including future you

== Unsafe Traits
- `unsafe trait` means the trait has semantic obligations the compiler cannot check #pause
- `Send` and `Sync` are the usual examples #pause
- An `unsafe impl` is not a local trick, it is a global promise #pause
- If the promise is wrong, unrelated safe code can become unsound

== Keeping Unsafe Small
- Small unsafe blocks are easier to audit #pause
- Narrow abstraction boundaries reduce proof surface #pause
- Converting raw operations into a safe local helper is usually worth it #pause
- "Maximum cleverness" is the wrong optimization target here

= Tools

== Miri
- Miri interprets Rust's mid-level IR #pause
- It catches many classes of UB-related mistakes during execution #pause
- It is not a proof system #pause
- It is still one of the best tools we have for unsafe exploration

== Running Miri
```text
rustup +nightly component add miri
cargo +nightly miri test
```
- Miri should be part of the workflow for code that contains unsafe

== Miri Example
```rust
#![feature(vec_into_raw_parts)]

struct Hehe(Vec<u8>);

impl Hehe {
    fn new() -> Self {
        let v = Vec::with_capacity(10);
        let (parts_ptr, _, parts_cap) = v.into_raw_parts();
```
==
```rust
        let v = unsafe { Vec::from_raw_parts(parts_ptr, 10, parts_cap) };
        Self(v)
    }
}

fn main() {
    let data = Hehe::new();
    println!("Uninitialized data: {}", data.0[0]);
}
```

== Miri Notes
- This reads uninitialized memory #pause
- The code can look plausible in a quick review #pause
- Miri is very good at turning this sort of mistake into a concrete failure

== Sanitizers
- Miri is not enough by itself #pause
- AddressSanitizer and ThreadSanitizer also belong in the toolbox #pause
- Unsafe-heavy projects should run extra checks in CI #pause
- Tooling does not prove soundness, but it catches real mistakes early

== Further Reading
- The Rustonomicon is still the canonical long-form unsafe guide #pause
- The reference matters for layout, validity, and FFI details #pause
- For nontrivial unsafe code, reading primary documentation is not optional

== Closing
- Unsafe exists because some correct low-level code cannot be expressed otherwise #pause
- The hard part is not writing the `unsafe` keyword, it is maintaining the proof around it #pause
- The proof includes values, aliasing, lifetimes, panic paths, and module-wide invariants #pause
- Keep the surface small and the documentation explicit

= Questions?
