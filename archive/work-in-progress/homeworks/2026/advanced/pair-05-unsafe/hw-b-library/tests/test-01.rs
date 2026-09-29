mod solution;

use solution::{checksum_u32_le, PodSlice};

fn main() {
    let values = [10_u32.to_le(), 20_u32.to_le(), 30_u32.to_le()];
    let bytes = unsafe {
        std::slice::from_raw_parts(
            values.as_ptr().cast::<u8>(),
            values.len() * std::mem::size_of::<u32>(),
        )
    };
    let view = PodSlice::<u32>::from_bytes(bytes).unwrap();
    println!("len={} empty={}", view.len(), view.is_empty());
    println!("first={:?}", view.get(0));
    println!("missing={:?}", view.get(9));
    println!("checksum={}", checksum_u32_le(view));
}
