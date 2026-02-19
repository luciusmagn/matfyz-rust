use std::marker::PhantomData;

pub struct CheckedSlice<'a> {
    ptr: *const i64,
    len: usize,
    _marker: PhantomData<&'a [i64]>,
}

impl<'a> CheckedSlice<'a> {
    pub fn new(slice: &'a [i64]) -> Self {
        Self {
            ptr: slice.as_ptr(),
            len: slice.len(),
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get(&self, idx: usize) -> Option<i64> {
        if idx >= self.len {
            return None;
        }
        // Safe due to explicit bounds check above.
        unsafe { Some(*self.ptr.add(idx)) }
    }

    pub fn window_sum(&self, start: usize, end: usize) -> Option<i64> {
        if start > end || end > self.len {
            return None;
        }

        let mut acc = 0_i64;
        for i in start..end {
            // Safe due to range validation above.
            acc += unsafe { *self.ptr.add(i) };
        }
        Some(acc)
    }
}

pub fn checksum(slice: &[i64]) -> i64 {
    let view = CheckedSlice::new(slice);
    let mut acc = 0_i64;
    for i in 0..view.len() {
        if let Some(v) = view.get(i) {
            acc += (i as i64 + 1) * v;
        }
    }
    acc
}
