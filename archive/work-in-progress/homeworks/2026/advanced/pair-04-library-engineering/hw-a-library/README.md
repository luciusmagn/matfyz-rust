# A4A Async Rate Limiter Library

Course track: Advanced Rust (2026)  
Homework pair: A4  
Type: library  
Submission filename: `solution.rs`

## Task

Implement a small thread-safe async rate limiter library in `solution.rs`.

Required API:

```rust
use std::time::Duration;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RateLimitError {
    Closed,
    Timeout,
}

#[derive(Clone)]
pub struct RateLimiter;

impl RateLimiter {
    pub fn new(capacity: usize, refill: usize, interval: Duration) -> Self;
    pub fn capacity(&self) -> usize;
    pub fn available(&self) -> usize;
    pub fn close(&self);
    pub fn try_acquire(&self) -> Result<(), RateLimitError>;
    pub async fn acquire(&self) -> Result<(), RateLimitError>;
    pub async fn acquire_timeout(&self, timeout: Duration) -> Result<(), RateLimitError>;
}
```

## Behavior contract

1. The limiter starts with `capacity` available permits.
2. `try_acquire` consumes one permit if available.
3. `try_acquire` returns `Timeout` when no permit is currently available.
4. Every full `interval` refills `refill` permits, capped at `capacity`.
5. `available` performs the same refill accounting before returning.
6. `acquire` waits asynchronously until a permit is available or the limiter is closed.
7. `acquire_timeout` waits at most the supplied timeout.
8. After `close`, all acquire operations return `Closed`.
9. Clones of `RateLimiter` operate on the same shared state.

## Requirements

1. Keep the public API exactly as specified.
2. Do not block an async executor thread with `std::thread::sleep`.
3. Do not hold a mutex guard across `.await`.
4. Do not print from library functions.
