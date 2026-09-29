use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::sync::Notify;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RateLimitError {
    Closed,
    Timeout,
}

#[derive(Clone)]
pub struct RateLimiter {
    inner: Arc<Inner>,
}

struct Inner {
    state: Mutex<State>,
    notify: Notify,
}

struct State {
    capacity: usize,
    refill: usize,
    interval: Duration,
    available: usize,
    last_refill: Instant,
    closed: bool,
}

impl RateLimiter {
    pub fn new(capacity: usize, refill: usize, interval: Duration) -> Self {
        Self {
            inner: Arc::new(Inner {
                state: Mutex::new(State {
                    capacity,
                    refill,
                    interval,
                    available: capacity,
                    last_refill: Instant::now(),
                    closed: false,
                }),
                notify: Notify::new(),
            }),
        }
    }

    pub fn capacity(&self) -> usize {
        self.inner.state.lock().unwrap().capacity
    }

    pub fn available(&self) -> usize {
        let mut state = self.inner.state.lock().unwrap();
        Self::refill_locked(&mut state);
        state.available
    }

    pub fn close(&self) {
        let mut state = self.inner.state.lock().unwrap();
        state.closed = true;
        drop(state);
        self.inner.notify.notify_waiters();
    }

    pub fn try_acquire(&self) -> Result<(), RateLimitError> {
        self.try_take()
    }

    pub async fn acquire(&self) -> Result<(), RateLimitError> {
        loop {
            match self.try_take() {
                Ok(()) => return Ok(()),
                Err(RateLimitError::Closed) => return Err(RateLimitError::Closed),
                Err(RateLimitError::Timeout) => {}
            }

            let delay = self.wait_delay();
            tokio::select! {
                _ = tokio::time::sleep(delay) => {},
                _ = self.inner.notify.notified() => {},
            }
        }
    }

    pub async fn acquire_timeout(&self, timeout: Duration) -> Result<(), RateLimitError> {
        match tokio::time::timeout(timeout, self.acquire()).await {
            Ok(result) => result,
            Err(_) => Err(RateLimitError::Timeout),
        }
    }

    fn try_take(&self) -> Result<(), RateLimitError> {
        let mut state = self.inner.state.lock().unwrap();
        Self::refill_locked(&mut state);
        if state.closed {
            return Err(RateLimitError::Closed);
        }
        if state.available == 0 {
            return Err(RateLimitError::Timeout);
        }
        state.available -= 1;
        Ok(())
    }

    fn wait_delay(&self) -> Duration {
        let state = self.inner.state.lock().unwrap();
        if state.interval.is_zero() {
            Duration::from_millis(1)
        } else {
            state.interval.min(Duration::from_millis(10))
        }
    }

    fn refill_locked(state: &mut State) {
        if state.closed || state.capacity == 0 || state.refill == 0 {
            return;
        }
        if state.interval.is_zero() {
            state.available = state.capacity;
            state.last_refill = Instant::now();
            return;
        }

        let now = Instant::now();
        let elapsed = now.duration_since(state.last_refill);
        let periods = elapsed.as_nanos() / state.interval.as_nanos();
        if periods == 0 {
            return;
        }

        let add = periods.saturating_mul(state.refill as u128);
        let add = add.min(usize::MAX as u128) as usize;
        state.available = state.capacity.min(state.available.saturating_add(add));
        state.last_refill = now;
    }
}
