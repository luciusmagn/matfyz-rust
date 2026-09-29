mod solution;

use solution::RateLimiter;
use std::time::Duration;

#[tokio::main]
async fn main() {
    let limiter = RateLimiter::new(1, 1, Duration::from_millis(80));
    limiter.try_acquire().unwrap();
    println!("short={:?}", limiter.acquire_timeout(Duration::from_millis(10)).await);
    println!("long={:?}", limiter.acquire_timeout(Duration::from_millis(120)).await);
}
