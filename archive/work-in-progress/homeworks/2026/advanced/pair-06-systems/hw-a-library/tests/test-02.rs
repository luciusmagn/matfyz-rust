mod solution;

use solution::{nul_terminated_bytes, strlen_checked};

fn main() {
    println!("interior={:?}", nul_terminated_bytes("ab\0cd").unwrap_err());
    println!("missing={:?}", strlen_checked(b"unterminated").unwrap_err());
    println!("prefix={}", strlen_checked(b"abc\0ignored").unwrap());
}
