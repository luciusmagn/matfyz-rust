use std::future::Future;
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RetryPolicy {
    max_attempts: usize,
    base_delay: Duration,
    max_delay: Duration,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RetryError<E> {
    InvalidPolicy,
    Failed { attempts: usize, error: E },
}

impl RetryPolicy {
    pub fn new(max_attempts: usize, base_delay: Duration, max_delay: Duration) -> Self {
        Self {
            max_attempts,
            base_delay,
            max_delay,
        }
    }

    pub fn max_attempts(&self) -> usize {
        self.max_attempts
    }

    pub fn delay_after_failure(&self, failed_attempt: usize) -> Duration {
        if failed_attempt == 0 || self.base_delay.is_zero() {
            return Duration::ZERO;
        }

        let shift = failed_attempt.saturating_sub(1).min(31);
        let factor = 1_u32 << shift;
        self.base_delay.saturating_mul(factor).min(self.max_delay)
    }
}

pub fn collect_delays(policy: RetryPolicy) -> Vec<Duration> {
    if policy.max_attempts <= 1 {
        return Vec::new();
    }
    (1..policy.max_attempts)
        .map(|attempt| policy.delay_after_failure(attempt))
        .collect()
}

pub async fn retry_async<T, E, Op, Fut, ShouldRetry>(
    policy: RetryPolicy,
    mut op: Op,
    mut should_retry: ShouldRetry,
) -> Result<T, RetryError<E>>
where
    Op: FnMut(usize) -> Fut,
    Fut: Future<Output = Result<T, E>>,
    ShouldRetry: FnMut(&E) -> bool,
{
    if policy.max_attempts == 0 {
        return Err(RetryError::InvalidPolicy);
    }

    for attempt in 1..=policy.max_attempts {
        match op(attempt).await {
            Ok(value) => return Ok(value),
            Err(error) => {
                if attempt == policy.max_attempts || !should_retry(&error) {
                    return Err(RetryError::Failed {
                        attempts: attempt,
                        error,
                    });
                }
                tokio::time::sleep(policy.delay_after_failure(attempt)).await;
            }
        }
    }

    unreachable!()
}
