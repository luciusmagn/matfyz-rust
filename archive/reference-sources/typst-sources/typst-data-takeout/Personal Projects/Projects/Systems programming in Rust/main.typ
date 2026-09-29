#import "@preview/touying:0.3.2": *

// Themes: default, simple, metropolis, dewdrop, university, aqua
#let s = themes.metropolis.register(aspect-ratio: "16-9")
#let s = (s.methods.info)(
  self: s,
  title: [Some tools for Systems programming in Rust],
  subtitle: [],
  author: [Lukáš Hozda],
  date: datetime.today(),
  institution: [discord: *\@magnusi*],
)
#let (init, slides, touying-outline, alert) = utils.methods(s)
#show: init

#show strong: alert

#let (slide, title-slide, focus-slide) = utils.slides(s)
#show: slides


== PhantomData

- `PhantomData` is a zero-sized type used to mark unused type parameters
- It can be used to tell the compiler that a type is being used in a specific way, even if it doesn't appear in the struct's fields
- Useful for creating custom structs with raw pointers that do not have a well-defined type
- Can also be used to keep track of a lifetime

```rust
use std::marker::PhantomData;

struct MyStruct<T> {
    data: *mut T,
    _marker: PhantomData<T>,
}
```

== Homework: Implementing `Array<T, D>`

Specification:

- Create a struct `Array<T, D>` that takes a raw "void" pointer and a destructor
- The type should perform a default initialization of the provided memory
- Provide access to the elements of the array
- The struct should behave like a custom allocator, but in an explicit manner
- When array is dropped, the destructor is called with the raw pointer

= Binding and Using libc

== Linking to libc

- Add `libc` as a dependency in your `Cargo.toml` file

```toml
[dependencies]
libc = "0.2"
```

- Import the libc crate in your Rust code


== Using libc Functions
- Call libc functions directly from Rust code

```rs
use libc::{c_int, c_void};

unsafe {
    libc::printf(b"Hello, world!\n\0".as_ptr() as *const c_char);
    libc::malloc(1024);
}
```

Remember to use correct types! Rust's char is not the same as C's char!

= Using the nix Library (UNIXy systems' API)

== Adding nix as a Dependency

```toml
[dependencies]
nix = "0.28"
```
Then:
```rs
use nix::sys::stat::{fstat, FileStat};
use nix::unistd::{close, pipe, read, write};

let (read_fd, write_fd) = pipe().unwrap();
let message = "Hello, world!";
write(write_fd, message.as_bytes()).unwrap();
close(write_fd).unwrap();

let mut buffer = [0; 1024];
let bytes_read = read(read_fd, &mut buffer).unwrap();
close(read_fd).unwrap();
```

= Compiling C Code and Calling it from Rust

== Creating a C Library

- Write your C code in a separate file, e.g., `my_lib.c`

```c
#include <stdio.h>

void hello_from_c() {
    printf("Hello from C!\n");
}
```

```toml
[build-dependencies]
cc = "1.0"

[dependencies]
libc = "0.2"
```

- Create a build.rs file to link the library during compilation

```rs
fn main() {
    cc::Build::new()
        .file("my_lib.c")
        .compile("my_lib");
}
```

Declare in Rust:

```rust
extern "C" {
    fn hello_from_c();
}
```

Call in Rust:

```rs
unsafe {
    hello_from_c();
}
```

= Foreign Function Interface (FFI)

== Declaring External Functions

- Use *extern* keyword to declare functions from other languages

```rs
extern "C" {
    fn my_c_function(arg1: i32, arg2: *const c_char) -> i32;
}

let result = unsafe {
    my_c_function(42, "Hello".as_ptr() as *const _)
};
```

= Interacting with the Operating System (Unix Sockets)

== Unix Socket

- Use the `nix` crate to create a Unix socket

```rust
use nix::sys::socket::{socket, AddressFamily, SockType, SockFlag};
use nix::sys::socket::{bind, connect, SockAddr};
use std::path::Path;

let socket_fd = socket(
    AddressFamily::Unix,
    SockType::Stream,
    SockFlag::empty(),
    None,
).unwrap();
let socket_path = Path::new("/tmp/my_socket");
let socket_addr = SockAddr::new_unix(socket_path).unwrap();
bind(socket_fd, &socket_addr).unwrap();
// connect(socket_fd, &socket_addr).unwrap();
```

= SIMD

```rs
#![feature(portable_simd)]
use std::simd::i32x4;

fn main() {
    let a = i32x4::from([1, 2, 3, 4]);
    let b = i32x4::from([5, 6, 7, 8]);

    let sum = a + b;

    println!("SIMD sum: {:?}", sum);
}
```

= Cross-compilation

- Use cross or setup the C toolchain by yourself

```toml
[target.aarch64-unknown-linux-gnu]
linker = "aarch64-linux-gnu-gcc"
```

Remember the get the STD for a particular target:

```
rustup target add <name>
```

= Raw Assembly in Rust

== Inline Assembly

- Use the `asm!` macro to write inline assembly

```rust
use std::arch::asm;

// Multiply x by 6 using shifts and adds
let mut x: u64 = 4;
unsafe {
    asm!(
        "mov {tmp}, {x}",
        "shl {tmp}, 1",
        "shl {x}, 2",
        "add {x}, {tmp}",
        x = inout(reg) x,
        tmp = out(reg) _,
    );
}
assert_eq!(x, 4 * 6);
```

= ManuallyDrop

== Avoiding Destructor Calls

```rs
use std::mem::ManuallyDrop;

let x = ManuallyDrop::new(String::from("Hello, world!"));

unsafe {
    ManuallyDrop::drop(&mut x);
}
```

= Self-referential structs

```rs
use std::mem::ManuallyDrop; use std::ptr;

struct SelfReferential {
    data: String,
    reference: *const String,
}
impl SelfReferential {
    fn new(data: String) -> Self {
        let res = SelfReferential {
            data: data,
            reference: ptr::null(),
        };
        let mut res = ManuallyDrop::new(res);
        res.reference = &res.data;
        unsafe { ManuallyDrop::into_inner(res) }
    }
}
```

= Pivo

Pivo?