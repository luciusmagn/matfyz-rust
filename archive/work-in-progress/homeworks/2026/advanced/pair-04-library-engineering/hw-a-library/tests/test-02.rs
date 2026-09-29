mod solution;

use solution::RateLimiter;
use std::time::Duration;

#[tokio::main]
async fn main() {
    let limiter = RateLimiter::new(1, 1, Duration::from_millis(20));
    println!("first={:?}", limiter.try_acquire());
    println!("now={:?}", limiter.try_acquire());
    tokio::time::sleep(Duration::from_millis(30)).await;
    println!("later={:?}", limiter.try_acquire());
}
