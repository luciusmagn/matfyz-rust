use std::alloc::{self, Layout};
use std::mem;
use std::ptr::{self, NonNull};

pub struct RawFixedVec<T> {
    ptr: NonNull<T>,
    len: usize,
    cap: usize,
}

impl<T> RawFixedVec<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 || mem::size_of::<T>() == 0 {
            return Self {
                ptr: NonNull::dangling(),
                len: 0,
                cap: capacity,
            };
        }

        let layout = Layout::array::<T>(capacity).unwrap();
        let raw = unsafe { alloc::alloc(layout) };
        let ptr = NonNull::new(raw.cast::<T>())
            .unwrap_or_else(|| alloc::handle_alloc_error(layout));

        Self { ptr, len: 0, cap: capacity }
    }

    pub fn capacity(&self) -> usize {
        self.cap
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn push(&mut self, value: T) -> Result<(), T> {
        if self.len == self.cap {
            return Err(value);
        }

        if mem::size_of::<T>() == 0 {
            mem::forget(value);
        } else {
            unsafe {
                self.ptr.as_ptr().add(self.len).write(value);
            }
        }
        self.len += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }

        self.len -= 1;
        if mem::size_of::<T>() == 0 {
            Some(unsafe { mem::MaybeUninit::<T>::uninit().assume_init() })
        } else {
            Some(unsafe { self.ptr.as_ptr().add(self.len).read() })
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index >= self.len {
            return None;
        }
        unsafe { Some(&*self.ptr.as_ptr().add(index)) }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index >= self.len {
            return None;
        }
        unsafe { Some(&mut *self.ptr.as_ptr().add(index)) }
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }
}

impl<T> Drop for RawFixedVec<T> {
    fn drop(&mut self) {
        while self.len > 0 {
            self.len -= 1;
            unsafe {
                ptr::drop_in_place(self.ptr.as_ptr().add(self.len));
            }
        }

        if self.cap != 0 && mem::size_of::<T>() != 0 {
            let layout = Layout::array::<T>(self.cap).unwrap();
            unsafe {
                alloc::dealloc(self.ptr.cast::<u8>().as_ptr(), layout);
            }
        }
    }
}
