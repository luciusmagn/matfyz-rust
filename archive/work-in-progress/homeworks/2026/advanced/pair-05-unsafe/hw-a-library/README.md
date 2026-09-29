# A5A Raw Fixed-Capacity Vector

Course track: Advanced Rust (2026)  
Homework pair: A5  
Type: library  
Submission filename: `solution.rs`

## Task

Implement a fixed-capacity vector with a small unsafe core.

Required API:

```rust
pub struct RawFixedVec<T>;

impl<T> RawFixedVec<T> {
    pub fn with_capacity(capacity: usize) -> Self;
    pub fn capacity(&self) -> usize;
    pub fn len(&self) -> usize;
    pub fn is_empty(&self) -> bool;
    pub fn push(&mut self, value: T) -> Result<(), T>;
    pub fn pop(&mut self) -> Option<T>;
    pub fn get(&self, index: usize) -> Option<&T>;
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T>;
    pub fn as_slice(&self) -> &[T];
    pub fn as_mut_slice(&mut self) -> &mut [T];
}
```

## Behavior contract

1. The vector never grows beyond its original capacity.
2. `push` returns `Err(value)` when full.
3. `pop` removes and returns the last initialized element.
4. `get`, `get_mut`, `as_slice`, and `as_mut_slice` expose only initialized elements.
5. Dropping the vector drops every still-initialized element exactly once.
6. Zero-capacity vectors are valid.
7. Zero-sized element types must be supported.

## Required invariants

1. `len <= capacity` always holds.
2. For non-zero-sized `T`, capacity greater than zero owns one allocation suitable for `capacity` values of `T`.
3. For zero-sized `T`, no allocation is needed, but `len` and `capacity` remain logical counts.
4. Slots `0..len` are initialized.
5. Slots `len..capacity` are uninitialized.
6. Deallocation happens at most once.

## Requirements

1. Keep the public API exactly as specified.
2. Do not wrap `Vec<T>` or `Box<[T]>` as the implementation.
3. Do not print from library functions.
