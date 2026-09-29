use tokio::runtime::Builder;
use tokio::time::{sleep, Duration};

pub async fn delayed_double(value: i64) -> i64 {
    sleep(Duration::from_millis(1)).await;
    value * 2
}

pub async fn async_sum(values: Vec<i64>) -> i64 {
    let mut handles = Vec::new();
    for value in values {
        handles.push(tokio::spawn(async move { delayed_double(value).await }));
    }

    let mut sum = 0_i64;
    for h in handles {
        sum += h.await.expect("task panicked");
    }
    sum
}

pub fn run_async_sum(values: Vec<i64>) -> i64 {
    let rt = Builder::new_current_thread().enable_time().build().unwrap();
    rt.block_on(async_sum(values))
}
