mod solution;

use solution::{collect_delays, RetryPolicy};
use std::time::Duration;

fn main() {
    let policy = RetryPolicy::new(5, Duration::from_millis(10), Duration::from_millis(35));
    println!("attempts={}", policy.max_attempts());
    for d in collect_delays(policy) {
        println!("{}", d.as_millis());
    }
}
