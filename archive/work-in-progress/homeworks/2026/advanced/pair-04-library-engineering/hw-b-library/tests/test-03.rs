mod solution;

use solution::{retry_async, RetryError, RetryPolicy};
use std::time::Duration;

#[tokio::main]
async fn main() {
    let policy = RetryPolicy::new(5, Duration::ZERO, Duration::ZERO);
    let result: Result<(), RetryError<i32>> = retry_async(
        policy,
        |attempt| async move { Err(attempt as i32) },
        |err| *err < 2,
    ).await;
    println!("{result:?}");
}
