mod solution;

use solution::RawFixedVec;
use std::sync::atomic::{AtomicUsize, Ordering};

static DROPS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
struct CountDrop(u8);

impl Drop for CountDrop {
    fn drop(&mut self) {
        DROPS.fetch_add(self.0 as usize, Ordering::SeqCst);
    }
}

fn main() {
    {
        let mut vec = RawFixedVec::with_capacity(4);
        vec.push(CountDrop(1)).unwrap();
        vec.push(CountDrop(2)).unwrap();
        vec.push(CountDrop(4)).unwrap();
        drop(vec.pop());
        println!("after_pop={}", DROPS.load(Ordering::SeqCst));
    }
    println!("after_drop={}", DROPS.load(Ordering::SeqCst));
}
