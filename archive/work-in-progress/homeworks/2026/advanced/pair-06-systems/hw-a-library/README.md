# A6A C String Boundary Helpers

Course track: Advanced Rust (2026)  
Homework pair: A6  
Type: library  
Submission filename: `solution.rs`

## Task

Implement helpers for crossing a C string boundary. The public API is safe; the implementation may use `CString`, `CStr`, and `libc::strlen` internally.

Required API:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CStringBoundaryError {
    InteriorNul(usize),
    MissingTerminator,
}

pub fn nul_terminated_bytes(input: &str) -> Result<Vec<u8>, CStringBoundaryError>;
pub fn strlen_checked(bytes: &[u8]) -> Result<usize, CStringBoundaryError>;
pub fn c_strings_from_block(bytes: &[u8]) -> Result<Vec<String>, CStringBoundaryError>;
pub fn join_c_strings(values: &[&str]) -> Result<Vec<u8>, CStringBoundaryError>;
```

## Behavior contract

1. `nul_terminated_bytes` returns UTF-8 bytes followed by one `0` byte.
2. `nul_terminated_bytes` rejects interior NUL bytes and reports their byte position.
3. `strlen_checked` requires at least one NUL terminator in the slice.
4. `strlen_checked` returns the length before the first NUL byte.
5. `strlen_checked` must not call C with an unterminated pointer.
6. `c_strings_from_block` parses a block of consecutive NUL-terminated UTF-8 strings.
7. `c_strings_from_block` rejects a non-empty block that does not end in NUL.
8. `join_c_strings` concatenates multiple NUL-terminated strings into one byte block.
9. Empty strings are valid C strings.

## Requirements

1. Keep the public API exactly as specified.
2. Do not expose raw pointers in the public API.
3. Do not print from library functions.
4. Do not use unchecked UTF-8 conversion.
