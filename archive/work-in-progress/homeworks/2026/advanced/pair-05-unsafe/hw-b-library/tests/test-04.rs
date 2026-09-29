mod solution;

use solution::{PlainOldData, PodSlice};

#[derive(Clone, Copy)]
struct Empty;

unsafe impl PlainOldData for Empty {}

fn main() {
    let bytes = [];
    let err = match PodSlice::<Empty>::from_bytes(&bytes) {
        Ok(_) => "ok".to_string(),
        Err(err) => format!("{err:?}"),
    };
    println!("zst={err}");
    println!("empty_u8={}", PodSlice::<u8>::from_bytes(&bytes).unwrap().is_empty());
}
