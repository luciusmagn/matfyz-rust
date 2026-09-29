mod solution;

use solution::{nul_terminated_bytes, strlen_checked};

fn main() {
    let bytes = nul_terminated_bytes("hello").unwrap();
    println!("bytes={:?}", bytes);
    println!("strlen={}", strlen_checked(&bytes).unwrap());
}
