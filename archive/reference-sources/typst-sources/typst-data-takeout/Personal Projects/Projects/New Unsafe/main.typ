#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Unsafe Rust],
   author: [Lukáš Hozda],
   date: datetime.today(),
   logo: emoji.key,
   institution: [Braiins Systems s.r.o],
 ),
)

#title-slide()

= Unsafe Rust

== Unsafe Rust

- There are two languages in Rust #pause
  - Safe Rust #pause
  - Unsafe Rust #pause
- Split with a boundary in the form of `unsafe {}` blocks #pause
- Rust programmers have a difficult relationship with unsafe #pause
  - Mostly fear and hatred #pause
  - Or on the other end of the spectrum - "It's just C" #pause
- Neither of these perspectives is completely true #pause
- There are very good reasons for having unsafe in the language #pause
  and we can write unsafe correctly

== First, what safe Rust does for us?

- No undefined behavior: #pause
  - Integer overflow #pause
  - Out of bounds access #pause
  - Unitialized value access #pause
  - Dangling pointer derefence #pause
  - Data race in threaded programs #pause
  - Mutable pointer aliasing issues #pause
  - Infinite loop without side effects #pause
    - ↑ try this one in C/C++

== Why do we have unsafe?

- Lets us write code whose safety cannot be verified by the
  compiler #pause

- Use-cases: #pause
  - Working with hardware directly #pause
  - Interop (interacting with external code, libraries etc.) #pause
  - Writing concurrency primitives #pause
    - E.g. if you wanted to write your own `Arc`, `Mutex`, `RwLock` #pause
  - Overcoming borrow checker limitations #pause
    - There will always be programs that the borrow-checker does
      not know are correct #pause
  - Performance optimizations

== Hardware

```rs
// I know there is a memory-mapped register at this address
const DEVICE_REGISTER_ADDRESS: *mut u32 = 0x1000_0000 as *mut u32;

unsafe fn write_to_device_register(value: u32) {
    // Directly write to a memory address, which is considered unsafe
    // because Rust can't guarantee that this memory address is valid or that
    // no data races will occur.
    std::ptr::write_volatile(DEVICE_REGISTER_ADDRESS, value);
}

fn main() {
    unsafe {
        write_to_device_register(0xABCD_EF01);
    }
}
```

== Hardware

```rs
const VGA_PTR: u32 = 0x80808080 as *mut u8;

let mut vga = unsafe {
  std::slice::from_raw_parts_mut(VGA_PTR, 800 * 600)
};

```

The compiler has no way of knowing about these. #pause
- This snippet is also only safe under particular conditions #pause
(when running on bare metal, no OS) #pause

-  HALs libraries do these for most users, but who writes the HAL? #pause
(HAL = Hardware Abstraction Layer)

== Interfacing with foreign code
```rs
extern "C" {
    // Declares that there's an external C function called strcpy
    // It takes two pointers to C-style strings (null-terminated)
    // and returns a pointer to the destination string.
    fn strcpy(
      dest: *mut libc::c_char,
      src: *const libc::c_char
    ) -> *mut libc::c_char;
}
```

This is unsafe because the compiler has no idea what the function is doing()

== Required glue

Typically, we write safe wrappers around these:

```rs
fn safe_strcpy(dest: &mut [u8], src: &str) -> Result<(), &'static str> {
    let src_len = src.len();
    if dest.len() <= src_len {
        return Err("destination buffer is too small");
    }

    let c_src = std::ffi::CString::new(src).expect("CString::new failed");
    let src_ptr = c_src.as_ptr();

    let dest_ptr = dest.as_mut_ptr() as *mut libc::c_char;

    unsafe { strcpy(dest_ptr, src_ptr); }

    Ok(())
}

```

== Concurrency primitives

- Consider Mutex - it creates &mut T through &self #pause
- The compiler cannot know that we are not doing mutable pointer aliasing #pause
- Unsafe is required to change the type of references

== Transmuting references

```rs
use std::mem;

struct BufferHolder {
    buffer: *mut i32,
    len: usize,  // Store length to safely access the buffer
}
```

== 

```rs
impl BufferHolder {
    fn new(size: usize) -> Self {
        let mut vec = vec![0; size];
        let buffer_ptr = vec.as_mut_ptr();
        mem::forget(vec); // Prevent Vec from being dropped
        BufferHolder {
            buffer: buffer_ptr,
            len: size,
        }
    }
    use std::mem::transmute;
    unsafe fn get_buffer_mut<'a>(&'a self) -> &'a mut [i32] {
        assert!(!self.buffer.is_null(), "Buffer is null");
        transmute::<(*mut i32, usize), &'a mut [i32]>((self.buffer, self.len))
    }
}
```
== 
```rs
impl Drop for BufferHolder {
    fn drop(&mut self) {
        unsafe {
            // Properly drop the buffer by converting back to Vec
            let _vec = Vec::from_raw_parts(self.buffer, self.len, self.len);
        }
    }
}
```

== Memory leaks are not considered unsafe

This is safe:

```rs
use std::mem;

pub fn strify(s: String) -> &'static str {
	let s_ref = s.as_str();
	let s_static_ref: &'static str = unsafe { mem::transmute(s_ref) };
	mem::forget(s);
	s_static_ref
}
```

`mem::forget` is used to leak the String. It takes by ownership, but prevents
`Drop` from being run

== Overcoming borrow-checker limitations

Consider we want to return reference early:

```rs
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

This is correct, but the compiler does not know it yet

== Performance optimization

- Contentious - you may often do premature optimization #pause
- Check first if you actually need to optimize anything

```rs
fn sum_vector_safe(v: &[i32]) -> i32 {
    let mut sum = 0;
    for &item in v.iter() {
        sum += item;
    }
    sum
}
```

== 

```rs
fn sum_vector_unsafe(v: &[i32]) -> i32 {
    let mut sum = 0;
    let len = v.len();
    unsafe {
        let ptr = v.as_ptr();
        for i in 0..len {
            // Perform an unchecked access to the vector element
            sum += *ptr.add(i);
        }
    }
    sum
}
```

== Should you use unsafe?

- Only if you need to #pause
- With unsafe, we tell the compiler that the code is safe, because we checked it manually #pause
- We can be wrong

== Compiler assumptions

The compiler assumes all code follows safety rules. #pause

Assumes no code ever: #pause
- Dereferences dangling/unaligned pointers #pause
- Violetes reference aliasing rules #pause
- Causes an unsychronized data race with a write #pause
- Produces an invalid value #pause
- etc. #pause

That includes unsafe code.

==

- You can create undefined behavior if you use unsafe incorrectly:

```rs
fn paradox() -> ! {
	unsafe { std::mem::transmute(()) }
}
```

It should be impossible to create an instance of the Never type.

Undefined behavior == russian roulette

== Problems

```rs
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
vs.

```rs
fn compute(i: &u32, o: &mut u32) {
    let cached_i = *i;
    if cached_i > 10 {
        *o = 2;
    } else if cached_i > 5 {
        *o *= 2;
    }
}
```

Rust is expects to be able to do this optimization, and you have
to ensure that it can do it

== Subtle example from Jon Gjengset

```rs
impl<T> Vec<T> {
    fn extend_map<U, F>(&mut self, us: &[U], mut f: F)
    where
        F: FnMut(&U) -> T {
        self.reserve(us.len());
        let cur_len = self.len();
        unsafe { self.set_len(cur_len + us.len()); };

        let into = unsafe { self.as_mut_ptr().add(cur_len) };
        for u in us {
            unsafe {
                std::ptr::write(into, f(u));
                into += 1;
            }
        }
    }
}
```

What happens if *f()* panics (and panic isn't set to abort)?

Hint: consider if *T* implements *Drop*

== 

Safe wrappers for unsafe code must behave correctly no matter
what the safe code does

== Miri

- Experimental component of the Rust toolchain #pause
- Mid-level Intermediate Representation Interpreter #pause
- Essentially a Rust interpreter that detects possible memory issues

```
rustup +nightly component add miri
```

==

```rs
#![feature(vec_into_raw_parts)]

struct Hehe(Vec<u8>);

impl Hehe {
    fn new() -> Self {
        let v = Vec::with_capacity(10);
        let (parts_ptr, _, parts_cap) = v.into_raw_parts();
        let v = unsafe { Vec::from_raw_parts(parts_ptr, 10, parts_cap) };
        Self(v)
    }
}

fn main() {
    let data = Hehe::new();
    println!("Uninitialized data: {}", data.0[0]);
}
```

Miri will detect this:

#image("screenshot.png")