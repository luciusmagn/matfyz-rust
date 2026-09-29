mod solution;

use solution::{PodSlice, ViewError};

#[repr(align(4))]
struct Aligned([u8; 8]);

fn main() {
    let bytes = Aligned([0_u8; 8]);
    let err = match PodSlice::<u32>::from_bytes(&bytes.0[..6]) {
        Ok(_) => "ok".to_string(),
        Err(err) => format!("{err:?}"),
    };
    println!("{err}");
    println!("ok={}", PodSlice::<u16>::from_bytes(&bytes.0[..6]).unwrap().len());
    println!("expect={:?}", ViewError::SizeMismatch);
}
