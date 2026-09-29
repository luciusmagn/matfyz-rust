# A6B Unix File Descriptor Helpers

Course track: Advanced Rust (2026)  
Homework pair: A6  
Type: library  
Submission filename: `solution.rs`

## Task

Implement small safe wrappers around Unix file descriptor operations. The public API uses `OwnedFd` and `BorrowedFd`; raw descriptor ownership must not leak into callers.

Required API:

```rust
use std::io;
use std::os::fd::{BorrowedFd, OwnedFd};

pub fn make_pipe() -> io::Result<(OwnedFd, OwnedFd)>;
pub fn write_all_fd(fd: BorrowedFd<'_>, bytes: &[u8]) -> io::Result<()>;
pub fn read_exact_fd(fd: BorrowedFd<'_>, len: usize) -> io::Result<Vec<u8>>;
pub fn read_to_end_fd(fd: BorrowedFd<'_>, max_bytes: usize) -> io::Result<Vec<u8>>;
pub fn set_nonblocking(fd: BorrowedFd<'_>, nonblocking: bool) -> io::Result<()>;
```

## Behavior contract

1. `make_pipe` creates a Unix pipe and transfers both descriptors into `OwnedFd`.
2. `write_all_fd` keeps writing until all bytes are written or an error occurs.
3. `write_all_fd` retries on `EINTR`.
4. `read_exact_fd` reads exactly `len` bytes unless EOF or an error occurs.
5. `read_exact_fd` returns `UnexpectedEof` if EOF appears before `len` bytes.
6. `read_to_end_fd` reads until EOF or until `max_bytes` bytes have been collected.
7. `read_to_end_fd` retries on `EINTR`.
8. `set_nonblocking` preserves all existing descriptor flags except `O_NONBLOCK`.

## Requirements

1. Keep the public API exactly as specified.
2. Use `OwnedFd` for ownership and `BorrowedFd` for borrowing.
3. Do not call `close` manually on descriptors owned by `OwnedFd`.
4. Do not print from library functions.
