use std::marker::PhantomData;
use std::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViewError {
    ZeroSizedType,
    Misaligned,
    SizeMismatch,
}

pub unsafe trait PlainOldData: Copy + 'static {}

unsafe impl PlainOldData for u8 {}
unsafe impl PlainOldData for u16 {}
unsafe impl PlainOldData for u32 {}
unsafe impl PlainOldData for u64 {}
unsafe impl PlainOldData for u128 {}
unsafe impl PlainOldData for usize {}
unsafe impl PlainOldData for i8 {}
unsafe impl PlainOldData for i16 {}
unsafe impl PlainOldData for i32 {}
unsafe impl PlainOldData for i64 {}
unsafe impl PlainOldData for i128 {}
unsafe impl PlainOldData for isize {}

pub struct PodSlice<'a, T: PlainOldData> {
    slice: &'a [T],
    _marker: PhantomData<T>,
}

impl<'a, T: PlainOldData> PodSlice<'a, T> {
    pub fn from_bytes(bytes: &'a [u8]) -> Result<Self, ViewError> {
        let size = mem::size_of::<T>();
        if size == 0 {
            return Err(ViewError::ZeroSizedType);
        }
        if bytes.as_ptr().align_offset(mem::align_of::<T>()) != 0 {
            return Err(ViewError::Misaligned);
        }
        if bytes.len() % size != 0 {
            return Err(ViewError::SizeMismatch);
        }

        let len = bytes.len() / size;
        let ptr = bytes.as_ptr().cast::<T>();
        let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
        Ok(Self {
            slice,
            _marker: PhantomData,
        })
    }

    pub fn len(&self) -> usize {
        self.slice.len()
    }

    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<T> {
        self.slice.get(index).copied()
    }

    pub fn as_slice(&self) -> &'a [T] {
        self.slice
    }
}

pub fn checksum_u32_le(values: PodSlice<'_, u32>) -> u64 {
    values
        .as_slice()
        .iter()
        .enumerate()
        .map(|(i, value)| (i as u64 + 1) * u32::from_le(*value) as u64)
        .sum()
}
