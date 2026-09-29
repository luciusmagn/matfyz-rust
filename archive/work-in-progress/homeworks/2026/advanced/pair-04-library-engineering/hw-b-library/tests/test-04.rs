mod solution;

use solution::{retry_async, RetryError, RetryPolicy};
use std::time::Duration;

#[tokio::main]
async fn main() {
    let invalid = RetryPolicy::new(0, Duration::ZERO, Duration::ZERO);
    let result: Result<(), RetryError<&'static str>> = retry_async(
        invalid,
        |_| async { Ok(()) },
        |_| true,
    ).await;
    println!("invalid={result:?}");

    let policy = RetryPolicy::new(3, Duration::ZERO, Duration::ZERO);
    let result: Result<(), RetryError<&'static str>> = retry_async(
        policy,
        |attempt| async move { Err(if attempt == 3 { "last" } else { "retry" }) },
        |_| true,
    ).await;
    println!("exhausted={result:?}");
}
