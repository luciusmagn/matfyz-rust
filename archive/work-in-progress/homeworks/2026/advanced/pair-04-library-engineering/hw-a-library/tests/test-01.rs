mod solution;

use solution::{RateLimitError, RateLimiter};
use std::time::Duration;

fn main() {
    let limiter = RateLimiter::new(2, 1, Duration::from_millis(50));
    println!("cap={}", limiter.capacity());
    println!("available={}", limiter.available());
    println!("a={:?}", limiter.try_acquire());
    println!("b={:?}", limiter.try_acquire());
    println!("c={:?}", limiter.try_acquire());
    println!("empty={}", limiter.try_acquire() == Err(RateLimitError::Timeout));
}
