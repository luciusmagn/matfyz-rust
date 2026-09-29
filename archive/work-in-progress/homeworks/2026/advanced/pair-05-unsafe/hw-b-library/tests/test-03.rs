mod solution;

use solution::PodSlice;

#[repr(align(4))]
struct Aligned([u8; 8]);

fn main() {
    let bytes = Aligned([1, 0, 0, 0, 2, 0, 0, 0]);
    println!("aligned={:?}", PodSlice::<u32>::from_bytes(&bytes.0[..4]).map(|v| v.get(0)));
    let err = match PodSlice::<u32>::from_bytes(&bytes.0[1..5]) {
        Ok(_) => "ok".to_string(),
        Err(err) => format!("{err:?}"),
    };
    println!("misaligned={err}");
}
