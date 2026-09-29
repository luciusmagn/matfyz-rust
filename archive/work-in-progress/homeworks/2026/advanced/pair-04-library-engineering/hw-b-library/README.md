# A4B Async Retry Policy Library

Course track: Advanced Rust (2026)  
Homework pair: A4  
Type: library  
Submission filename: `solution.rs`

## Task

Implement a reusable async retry helper in `solution.rs`.

Required API:

```rust
use std::future::Future;
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RetryPolicy;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RetryError<E> {
    InvalidPolicy,
    Failed { attempts: usize, error: E },
}

impl RetryPolicy {
    pub fn new(max_attempts: usize, base_delay: Duration, max_delay: Duration) -> Self;
    pub fn max_attempts(&self) -> usize;
    pub fn delay_after_failure(&self, failed_attempt: usize) -> Duration;
}

pub fn collect_delays(policy: RetryPolicy) -> Vec<Duration>;

pub async fn retry_async<T, E, Op, Fut, ShouldRetry>(
    policy: RetryPolicy,
    op: Op,
    should_retry: ShouldRetry,
) -> Result<T, RetryError<E>>
where
    Op: FnMut(usize) -> Fut,
    Fut: Future<Output = Result<T, E>>,
    ShouldRetry: FnMut(&E) -> bool;
```

## Behavior contract

1. Attempt numbers passed to `op` are 1-based.
2. `max_attempts == 0` is invalid and returns `RetryError::InvalidPolicy`.
3. `delay_after_failure(1)` is `base_delay`.
4. Later delays double after each failed attempt and are capped at `max_delay`.
5. `collect_delays` returns the delays before attempts 2 through `max_attempts`.
6. `retry_async` returns immediately on the first successful attempt.
7. If `should_retry(&error)` returns false, return `Failed` with that error.
8. If all attempts fail, return `Failed` with the last error.
9. Sleep only between attempts, never after the final failed attempt.

## Requirements

1. Keep the public API exactly as specified.
2. Do not print from library functions.
3. Do not assume the error type implements `Clone` or `Default`.
4. Keep retry timing deterministic from the policy values.
