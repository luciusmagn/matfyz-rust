#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 6],
  config-info(
    title: [Advanced Rust (2026): Systems Interfaces],
    subtitle: [Lecture 6],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Boundary Programming

== Systems Interfaces
- Rust code has type, lifetime, layout, and aliasing rules #pause
- C libraries, kernels, CPUs, and wire formats have different rules #pause
- Systems Rust is mostly about translating between these rule sets #pause
- `unsafe` is where we state the part Rust cannot verify

== Boundary Examples
- C ABI: functions, structs, strings, callbacks #pause
- Operating system: file descriptors, signals, sockets, memory mappings #pause
- CPU: alignment, atomics, SIMD, inline assembly #pause
- Build system: linkers, target triples, native dependencies

== One Boundary, Two Contracts
- Rust side: values must be valid for their Rust types #pause
- Foreign side: bytes and handles must satisfy external preconditions #pause
- Safe wrapper: checks or encodes the foreign preconditions #pause
- Unsafe block: the smallest place where the two models meet

= Representation and Layout

== `repr(Rust)`
- Default Rust layout is not a stable interface #pause
- Field order, padding, and enum layout are compiler choices #pause
- The compiler may exploit invalid values for niche optimization #pause
- Do not expose `repr(Rust)` types across FFI or on-disk formats

== `repr(C)`
- `repr(C)` asks Rust to use the platform C ABI layout #pause
- It fixes field order and uses C-like padding and alignment #pause
- It does not make the fields safe to read as raw bytes #pause
- It does not define endianness for external protocols

== Layout Inspection
```rust
use std::mem::{align_of, size_of};

#[repr(C)]
struct Header {
    tag: u8,
    len: u32,
}

println!("size  = {}", size_of::<Header>());
println!("align = {}", align_of::<Header>());
```
#pause

The likely size is 8: one byte of data, three bytes of padding, four bytes of data

== Padding Is Not Data
```rust
#[repr(C)]
#[derive(Clone, Copy)]
struct Header {
    tag: u8,
    len: u32,
}

fn bytes(h: Header) -> [u8; 8] {
    unsafe { std::mem::transmute(h) }
}
```
#pause

- This can read uninitialized padding bytes #pause
- Miri can report this even when ordinary tests appear fine #pause
- Serialize fields explicitly instead

== Field-by-Field Encoding
```rust
fn encode_header(tag: u8, len: u32) -> [u8; 5] {
    let mut out = [0_u8; 5];
    out[0] = tag;
    out[1..5].copy_from_slice(&len.to_le_bytes());
    out
}
```
#pause

- The wire format is five bytes here, not `size_of::<Header>()` #pause
- Endianness is chosen by the protocol #pause
- Padding never enters the representation

== Rust Values Have Validity Rules
- `bool` may only contain `0` or `1` #pause
- `char` must be a Unicode scalar value #pause
- References must be non-null, aligned, valid, and obey aliasing rules #pause
- Creating an invalid value is already undefined behavior

== C Booleans and Rust Booleans
```rust
#[repr(C)]
struct CRecord {
    tag: libc::c_int,
    enabled: libc::c_int,
}

fn enabled(record: CRecord) -> bool {
    record.enabled != 0
}
```
#pause

A C API that says "boolean" often means integer convention, not Rust `bool`

== C Enums
```c
enum Mode {
    MODE_READ = 1,
    MODE_WRITE = 2,
};
```
#pause

```rust
#[repr(i32)]
enum Mode {
    Read = 1,
    Write = 2,
}
```
#pause

- A C caller can still pass `17` #pause
- Rust enum values must match one of the variants #pause
- Parse into `Result<Mode, i32>` at the boundary

== Packed Field Reference
```rust
#[repr(C, packed)]
struct PacketHeader {
    tag: u8,
    len: u16,
}

fn len_ref(h: &PacketHeader) -> &u16 {
    &h.len
}
```
#pause

Taking a normal reference to `len` would require alignment that the packed field may not have

== Packed Field Access
```rust
fn len_ok(h: &PacketHeader) -> u16 {
    let ptr = std::ptr::addr_of!(h.len);
    unsafe { u16::from_le(std::ptr::read_unaligned(ptr)) }
}
```
#pause

- `addr_of!` avoids creating an intermediate reference #pause
- `read_unaligned` states the actual load contract #pause
- Endianness conversion is still a separate operation

= Typed Raw Memory

== `PhantomData`
- Raw pointers do not express ownership or borrowing by themselves #pause
- `PhantomData` adds compile-time type and lifetime relationships #pause
- It has no runtime storage #pause
- It changes how the compiler reasons about the wrapper

== Borrowed Slice Wrapper
```rust
use std::marker::PhantomData;

struct RawSlice<'a, T> {
    ptr: *const T,
    len: usize,
    _marker: PhantomData<&'a T>,
}
```
#pause

The marker says: this value behaves as if it borrowed `T` values for `'a`

== Constructing the Wrapper
```rust
impl<'a, T> RawSlice<'a, T> {
    unsafe fn from_raw_parts(ptr: *const T, len: usize) -> Self {
        Self {
            ptr,
            len,
            _marker: PhantomData,
        }
    }
}
```
#pause

The unsafe constructor must require the same preconditions as `slice::from_raw_parts`

== Exposing the Slice
```rust
impl<'a, T> RawSlice<'a, T> {
    fn as_slice(&self) -> &'a [T] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }
}
```
#pause

- The safe method is only sound if the constructor contract was true #pause
- This is a common shape: unsafe construction, safe observation

== Marker Shapes
- `PhantomData<T>`: wrapper logically owns values of type `T` #pause
- `PhantomData<&'a T>`: wrapper logically shares values for `'a` #pause
- `PhantomData<&'a mut T>`: wrapper logically has exclusive access for `'a` #pause
- `PhantomData<fn() -> T>`: mentions `T` without ownership

== Variance and Drop Check
- A lifetime marker can prevent a raw pointer wrapper from outliving borrowed data #pause
- An ownership marker affects drop checking #pause
- Marker choice can affect `Send`, `Sync`, and variance #pause
- If the marker is wrong, the public type tells a false story

== Untyped Allocation
```rust
struct ForeignArray<T> {
    ptr: std::ptr::NonNull<T>,
    len: usize,
    drop_fn: unsafe extern "C" fn(*mut T, usize),
    _marker: PhantomData<T>,
}
```
#pause

- The pointer came from somewhere else #pause
- Rust still needs to know whether `T` is logically owned #pause
- `Drop` must call the matching foreign destructor exactly once

== Destruction Boundary
```rust
impl<T> Drop for ForeignArray<T> {
    fn drop(&mut self) {
        unsafe {
            (self.drop_fn)(self.ptr.as_ptr(), self.len);
        }
    }
}
```
#pause

This is only sound if `drop_fn` expects exactly this pointer, length, allocator, and element type

= C Strings and FFI

== Declaring C Functions
```rust
use libc::{c_char, size_t};

unsafe extern "C" {
    fn strlen(s: *const c_char) -> size_t;
}
```
#pause

- The declaration is a promise about ABI and signature #pause
- Rust cannot inspect the C implementation #pause
- Calling the function is unsafe unless wrapped by checked Rust code

== Rust String to C String
```rust
use std::ffi::CString;

fn strlen_checked(s: &str) -> Result<usize, std::ffi::NulError> {
    let c = CString::new(s)?;
    let n = unsafe { strlen(c.as_ptr()) };
    Ok(n as usize)
}
```
#pause

- `CString::new` rejects interior NUL bytes #pause
- `as_ptr` is valid only while the `CString` is alive #pause
- The C function must not store the pointer for later use

== C String to Rust String
```rust
use std::ffi::CStr;

unsafe fn borrowed_name(ptr: *const libc::c_char) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    Some(CStr::from_ptr(ptr).to_string_lossy().into_owned())
}
```
#pause

`CStr::from_ptr` requires a valid NUL-terminated byte sequence reachable from `ptr`

== Foreign Ownership
```rust
unsafe extern "C" {
    fn strdup(s: *const libc::c_char) -> *mut libc::c_char;
    fn free(ptr: *mut libc::c_void);
}
```
#pause

- Some returned pointers are borrowed #pause
- Some returned pointers must be released by a specific function #pause
- The Rust type must encode which case applies

== Owned C String Wrapper I
```rust
use std::{ffi::{CStr, CString}, io};

fn duplicate(s: &str) -> io::Result<String> {
    let input = CString::new(s)
        .map_err(|_| io::Error::from(io::ErrorKind::InvalidInput))?;

    let ptr = unsafe { strdup(input.as_ptr()) };
    if ptr.is_null() {
        return Err(io::Error::last_os_error());
    }
```
==
```rust
    let out = unsafe {
        CStr::from_ptr(ptr).to_string_lossy().into_owned()
    };
    unsafe {
        free(ptr.cast());
    }
    Ok(out)
}
```
#pause

The same allocator family that creates the pointer must destroy it

== Callback Boundaries
```rust
type Callback = unsafe extern "C" fn(ctx: *mut libc::c_void, value: i32);

unsafe extern "C" {
    fn register_callback(cb: Callback, ctx: *mut libc::c_void);
}
```
#pause

- The callback may be called later, on another thread, or during registration #pause
- The context pointer must remain valid for the API's actual lifetime #pause
- Panics must not unwind into C unless the ABI explicitly permits it

== Callback Context
```rust
struct State {
    sum: i64,
}

unsafe extern "C" fn on_value(ctx: *mut libc::c_void, value: i32) {
    let state = unsafe { &mut *(ctx.cast::<State>()) };
    state.sum += value as i64;
}
```
#pause

This assumes exclusive access to `State` whenever the C library invokes the callback

= Building and Linking

== Build Scripts
```toml
[build-dependencies]
cc = "1"

[dependencies]
libc = "0.2"
```
#pause

`build.rs` is compiled for the build host and runs before the Rust crate is compiled

== Compiling a C Shim
```rust
fn main() {
    println!("cargo:rerun-if-changed=src/shim.c");
    println!("cargo:rerun-if-changed=src/shim.h");

    cc::Build::new()
        .file("src/shim.c")
        .warnings(true)
        .compile("shim");
}
```
#pause

A missing `rerun-if-changed` can leave stale native code in incremental builds

== Linker Directives
```rust
fn main() {
    println!("cargo:rustc-link-search=native=/opt/vendor/lib");
    println!("cargo:rustc-link-lib=dylib=vendor");
}
```
#pause

- `static=foo` links `libfoo.a` #pause
- `dylib=foo` links a dynamic library #pause
- Runtime library search is a separate deployment problem

== `pkg-config`
```rust
fn main() {
    pkg_config::Config::new()
        .atleast_version("1.2")
        .probe("libzstd")
        .unwrap();
}
```
#pause

`pkg-config` asks the host system how to compile and link a native dependency

== Generated Bindings
- `bindgen`: C or C++ headers to Rust declarations #pause
- `cbindgen`: Rust declarations to C headers #pause
- Generated bindings are not automatically safe APIs #pause
- The public wrapper still needs a Rust-level contract

== Cross Compilation
- `HOST`: machine running the compiler and build script #pause
- `TARGET`: machine the final binary is built for #pause
- Rust target support is not enough if native libraries are involved #pause
- You also need the target linker, headers, and libraries

== Target Configuration
```toml
[target.aarch64-unknown-linux-gnu]
linker = "aarch64-linux-gnu-gcc"

[build]
target = "aarch64-unknown-linux-gnu"
```
#pause

The hard part is usually not Rust code generation, but native dependency resolution

= File Descriptors and OS Handles

== File Descriptors
- Unix file descriptors are small integers indexing kernel state #pause
- Ownership means responsibility to call `close` exactly once #pause
- Borrowing means temporary use without closing #pause
- Duplicating means creating a second owned descriptor for the same open file description

== Raw FD Double Close
```rust
use std::fs::File;
use std::os::fd::{AsRawFd, FromRawFd};

let file = File::open("data.txt")?;
let fd = file.as_raw_fd();

let also_owner = unsafe { File::from_raw_fd(fd) };
```
#pause

Now both `file` and `also_owner` believe they own the same descriptor

== Owned Descriptors
```rust
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::io;

fn pipe_owned() -> io::Result<(OwnedFd, OwnedFd)> {
    let mut fds = [-1; 2];
    let rc = unsafe { libc::pipe(fds.as_mut_ptr()) };
    if rc == -1 {
        return Err(io::Error::last_os_error());
    }
```
==
```rust
    let read = unsafe { OwnedFd::from_raw_fd(fds[0]) };
    let write = unsafe { OwnedFd::from_raw_fd(fds[1]) };
    Ok((read, write))
}
```
#pause

The two `from_raw_fd` calls are ownership transfers from the kernel API into Rust

== Borrowed Descriptors
```rust
use std::os::fd::{AsRawFd, BorrowedFd};
use std::io;

fn set_nonblocking(fd: BorrowedFd<'_>) -> io::Result<()> {
    let raw = fd.as_raw_fd();
    let flags = unsafe { libc::fcntl(raw, libc::F_GETFL) };
    if flags == -1 {
        return Err(io::Error::last_os_error());
    }
```
==
```rust
    let rc = unsafe {
        libc::fcntl(raw, libc::F_SETFL, flags | libc::O_NONBLOCK)
    };
    if rc == -1 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}
```
#pause

`BorrowedFd<'_>` prevents this function from accidentally taking ownership

== Syscall Error Shape
- Many C syscalls return `-1` on error and store details in `errno` #pause
- `EINTR` often means retry #pause
- `EAGAIN` or `EWOULDBLOCK` means nonblocking operation would block #pause
- Short reads and short writes are ordinary results, not necessarily errors

== Read Loop Skeleton
```rust
fn read_some(fd: BorrowedFd<'_>, buf: &mut [u8]) -> io::Result<usize> {
    loop {
        let n = unsafe {
            libc::read(fd.as_raw_fd(), buf.as_mut_ptr().cast(), buf.len())
        };
        if n >= 0 {
            return Ok(n as usize);
        }
```
==
```rust
        let err = io::Error::last_os_error();
        if err.raw_os_error() == Some(libc::EINTR) {
            continue;
        }
        return Err(err);
    }
}
```
#pause

This is the shape hidden inside many safer I/O abstractions

== Unix Domain Socket Path
- `bind` creates a filesystem entry for pathname sockets #pause
- A stale path from a crashed process can make the next `bind` fail #pause
- Filesystem permissions still matter #pause
- Cleanup has to match the socket lifetime, not merely the file descriptor lifetime

= Memory Mappings

== `mmap`
- `mmap` creates a virtual memory mapping in the process #pause
- The result is not Rust allocation and must not be passed to `dealloc` #pause
- Failure is `MAP_FAILED`, not a null pointer #pause
- `munmap` needs the same base pointer and length

== Mapping Wrapper I
```rust
use std::{io, ptr::NonNull};

struct Mapping {
    ptr: NonNull<u8>,
    len: usize,
}

impl Mapping {
    fn anonymous(len: usize) -> io::Result<Self> {
        let raw = unsafe {
            libc::mmap(
```
==
```rust
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            )
        };
```
==
```rust
        if raw == libc::MAP_FAILED {
            return Err(io::Error::last_os_error());
        }
        let Some(ptr) = NonNull::new(raw.cast()) else {
            unsafe { libc::munmap(raw, len); }
            return Err(io::Error::other("mmap returned null"));
        };
        Ok(Self { ptr, len })
    }
}
```

== Mapping Wrapper II
```rust
impl Mapping {
    fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len)
        }
    }
}

impl Drop for Mapping {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.ptr.as_ptr().cast(), self.len);
        }
    }
}
```

== Mapping Invariants
- `ptr` is the exact base address returned by `mmap` #pause
- `len` is the exact length used for the mapping #pause
- `len > 0` for this wrapper #pause
- No Rust reference outlives the mapping #pause
- Mutable slices are not handed out concurrently with aliases

== Memory-Mapped Files
- File length and mapping length are separate facts #pause
- Truncating a mapped file can make later access fault #pause
- Writes through mappings still obey OS synchronization rules #pause
- Rust slices cannot express every external invalidation event

= Address-Sensitive Values

== `ManuallyDrop`
- `ManuallyDrop<T>` suppresses automatic destructor execution #pause
- It does not make invalid values valid #pause
- It does not erase the field from the type #pause
- After manual drop, the program must track that the value is gone

== Manual Drop Example
```rust
use std::mem::ManuallyDrop;

struct Slot {
    value: ManuallyDrop<String>,
    initialized: bool,
}

impl Slot {
    unsafe fn clear(&mut self) {
        ManuallyDrop::drop(&mut self.value);
        self.initialized = false;
    }
}
```
#pause

The boolean is now part of the safety invariant

== Self-Reference Is Not Stable
```rust
use std::ptr;

struct SelfRef {
    data: String,
    ptr: *const String,
}

impl SelfRef {
    fn new(data: String) -> Self {
        let mut out = Self { data, ptr: ptr::null() };
        out.ptr = &out.data;
        out
    }
}
```
#pause

Returning `out` may move it, invalidating the stored address

== Pinning the Allocation I
```rust
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::ptr;

struct StableSelfRef {
    data: String,
    ptr: *const String,
    _pin: PhantomPinned,
}
```
==
```rust
impl StableSelfRef {
    fn new(data: String) -> Pin<Box<Self>> {
        let mut boxed = Box::pin(Self {
            data,
            ptr: ptr::null(),
            _pin: PhantomPinned,
        });
```
==
```rust
        let ptr = &boxed.data as *const String;
        unsafe {
            Pin::as_mut(&mut boxed).get_unchecked_mut().ptr = ptr;
        }
        boxed
    }
}
```
#pause

The unsafe assignment relies on the boxed allocation not moving afterwards

== Pin Contract
- `Pin<P>` is a promise about the pointee, not about the pointer object #pause
- `PhantomPinned` prevents automatic `Unpin` #pause
- Moving `Pin<Box<T>>` is fine; moving the boxed `T` is not #pause
- Field projection from pinned structs is where many bugs enter

= Target-Specific Code

== Conditional Compilation
```rust
#[cfg(target_family = "unix")]
fn platform_name() -> &'static str {
    "unix"
}

#[cfg(target_family = "windows")]
fn platform_name() -> &'static str {
    "windows"
}
```
#pause

`cfg` removes code before type checking for the other platform

== CPU Features
```rust
#[cfg(target_arch = "x86_64")]
fn has_avx2() -> bool {
    std::is_x86_feature_detected!("avx2")
}
```
#pause

- Compiling for an architecture is not the same as enabling every CPU feature #pause
- Dispatch can be runtime or compile-time #pause
- Calling feature-specific code without the feature is undefined behavior

== Target Features
```rust
#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn avx2_impl(input: &[u8]) -> usize {
    input.len()
}
```
#pause

A safe caller must check the feature before calling this function

== Inline Assembly
```rust
#[cfg(target_arch = "x86_64")]
fn read_tsc() -> u64 {
    let hi: u32;
    let lo: u32;
    unsafe {
        std::arch::asm!(
            "rdtsc",
```
==
```rust
            out("edx") hi,
            out("eax") lo,
            options(nomem, nostack, preserves_flags),
        );
    }
    ((hi as u64) << 32) | lo as u64
}
```
#pause

The operand list and options are part of the correctness argument

== Assembly Contracts
- Registers must match the instruction and ABI #pause
- `nomem` is only legal if the assembly does not read or write memory #pause
- `preserves_flags` is only legal if flags are preserved #pause
- Target-specific code must be hidden behind `cfg` or feature checks

= Questions?
