# A5B Typed Byte View

Course track: Advanced Rust (2026)  
Homework pair: A5  
Type: library  
Submission filename: `solution.rs`

## Task

Implement a safe typed view over a byte slice. The unsafe part is converting bytes into a slice of typed values; the public API must validate the conditions first.

Required API:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViewError {
    ZeroSizedType,
    Misaligned,
    SizeMismatch,
}

pub unsafe trait PlainOldData: Copy + 'static {}

pub struct PodSlice<'a, T: PlainOldData>;

impl<'a, T: PlainOldData> PodSlice<'a, T> {
    pub fn from_bytes(bytes: &'a [u8]) -> Result<Self, ViewError>;
    pub fn len(&self) -> usize;
    pub fn is_empty(&self) -> bool;
    pub fn get(&self, index: usize) -> Option<T>;
    pub fn as_slice(&self) -> &'a [T];
}

pub fn checksum_u32_le(values: PodSlice<'_, u32>) -> u64;
```

## Behavior contract

1. `PlainOldData` marks types whose every bit pattern is valid and which can be copied by value.
2. Implement `PlainOldData` for unsigned and signed integer primitives and `usize`/`isize`.
3. `from_bytes` rejects zero-sized `T`.
4. `from_bytes` rejects byte slices whose address is not aligned for `T`.
5. `from_bytes` rejects byte slices whose length is not a multiple of `size_of::<T>()`.
6. `get` returns a copied value, not a reference.
7. `as_slice` returns the typed view after validation.
8. `checksum_u32_le` computes `sum((index + 1) * value.to_le() as u64)`.

## Requirements

1. Keep the public API exactly as specified.
2. Use the unsafe trait as the boundary for typed byte reinterpretation.
3. Do not implement `PlainOldData` for `bool`, references, or user-defined types.
4. Do not print from library functions.
