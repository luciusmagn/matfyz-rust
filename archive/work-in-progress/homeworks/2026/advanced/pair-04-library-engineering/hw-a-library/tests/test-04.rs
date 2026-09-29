mod solution;

use solution::RateLimiter;
use std::time::Duration;

#[tokio::main]
async fn main() {
    let limiter = RateLimiter::new(1, 1, Duration::from_millis(50));
    let clone = limiter.clone();
    println!("take={:?}", clone.try_acquire());
    limiter.close();
    println!("try={:?}", clone.try_acquire());
    println!("wait={:?}", limiter.acquire().await);
}
