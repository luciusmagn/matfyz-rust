mod solution;

use solution::{retry_async, RetryPolicy};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

static CALLS: AtomicUsize = AtomicUsize::new(0);

#[tokio::main]
async fn main() {
    let policy = RetryPolicy::new(4, Duration::ZERO, Duration::ZERO);
    let result = retry_async(
        policy,
        |attempt| async move {
            CALLS.fetch_add(1, Ordering::SeqCst);
            if attempt < 3 { Err("again") } else { Ok(attempt * 10) }
        },
        |_| true,
    ).await;
    println!("result={result:?}");
    println!("calls={}", CALLS.load(Ordering::SeqCst));
}
