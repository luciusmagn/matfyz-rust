mod solution;

use solution::RawFixedVec;
use std::sync::atomic::{AtomicUsize, Ordering};

static ZST_DROPS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
struct Z;

impl Drop for Z {
    fn drop(&mut self) {
        ZST_DROPS.fetch_add(1, Ordering::SeqCst);
    }
}

fn main() {
    {
        let mut vec = RawFixedVec::with_capacity(3);
        vec.push(Z).unwrap();
        vec.push(Z).unwrap();
        println!("len={} cap={} slice={}", vec.len(), vec.capacity(), vec.as_slice().len());
        drop(vec.pop());
        println!("after_pop={}", ZST_DROPS.load(Ordering::SeqCst));
        vec.push(Z).unwrap();
    }
    println!("after_drop={}", ZST_DROPS.load(Ordering::SeqCst));
}
