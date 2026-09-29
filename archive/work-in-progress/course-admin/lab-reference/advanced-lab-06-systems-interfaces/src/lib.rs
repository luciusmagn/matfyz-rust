pub mod c_strings {
    use std::ffi::{CStr, CString};

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum CStringError {
        InteriorNul(usize),
        MissingTerminator,
        InvalidUtf8,
    }

    pub fn to_c_bytes(input: &str) -> Result<Vec<u8>, CStringError> {
        match CString::new(input) {
            Ok(c) => Ok(c.into_bytes_with_nul()),
            Err(err) => Err(CStringError::InteriorNul(err.nul_position())),
        }
    }

    pub fn strlen_checked(bytes: &[u8]) -> Result<usize, CStringError> {
        if !bytes.contains(&0) {
            return Err(CStringError::MissingTerminator);
        }
        Ok(unsafe { libc::strlen(bytes.as_ptr().cast()) as usize })
    }

    pub fn split_c_string_block(bytes: &[u8]) -> Result<Vec<String>, CStringError> {
        if bytes.is_empty() {
            return Ok(Vec::new());
        }
        if bytes.last() != Some(&0) {
            return Err(CStringError::MissingTerminator);
        }

        let mut out = Vec::new();
        let mut start = 0;
        for (idx, byte) in bytes.iter().copied().enumerate() {
            if byte == 0 {
                let c = CStr::from_bytes_with_nul(&bytes[start..=idx])
                    .map_err(|_| CStringError::InteriorNul(start))?;
                let text = c.to_str().map_err(|_| CStringError::InvalidUtf8)?;
                out.push(text.to_string());
                start = idx + 1;
            }
        }
        Ok(out)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn builds_c_bytes() {
            assert_eq!(to_c_bytes("abc").unwrap(), b"abc\0");
            assert_eq!(to_c_bytes("a\0b"), Err(CStringError::InteriorNul(1)));
        }

        #[test]
        fn checks_strlen_preconditions() {
            assert_eq!(strlen_checked(b"abc\0ignored").unwrap(), 3);
            assert_eq!(strlen_checked(b"abc"), Err(CStringError::MissingTerminator));
        }

        #[test]
        fn splits_c_string_blocks() {
            assert_eq!(
                split_c_string_block(b"red\0\0blue\0").unwrap(),
                vec!["red".to_string(), "".to_string(), "blue".to_string()]
            );
            assert_eq!(
                split_c_string_block(b"red\0blue"),
                Err(CStringError::MissingTerminator)
            );
            assert_eq!(
                split_c_string_block(&[0xFF, 0]),
                Err(CStringError::InvalidUtf8)
            );
        }
    }
}

pub mod mapping {
    use std::io;
    use std::ptr::NonNull;

    pub struct Mapping {
        ptr: NonNull<u8>,
        len: usize,
    }

    impl Mapping {
        pub fn anonymous(len: usize) -> io::Result<Self> {
            if len == 0 {
                return Err(io::Error::new(io::ErrorKind::InvalidInput, "zero-length mapping"));
            }

            let raw = unsafe {
                libc::mmap(
                    std::ptr::null_mut(),
                    len,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                    -1,
                    0,
                )
            };
            if raw == libc::MAP_FAILED {
                return Err(io::Error::last_os_error());
            }

            let Some(ptr) = NonNull::new(raw.cast()) else {
                unsafe {
                    libc::munmap(raw, len);
                }
                return Err(io::Error::other("mmap returned null"));
            };

            Ok(Self { ptr, len })
        }

        pub fn len(&self) -> usize {
            self.len
        }

        pub fn as_slice(&self) -> &[u8] {
            unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
        }

        pub fn as_mut_slice(&mut self) -> &mut [u8] {
            unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
        }

        pub fn fill(&mut self, value: u8) {
            self.as_mut_slice().fill(value);
        }
    }

    impl Drop for Mapping {
        fn drop(&mut self) {
            unsafe {
                libc::munmap(self.ptr.as_ptr().cast(), self.len);
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::io::ErrorKind;

        #[test]
        fn maps_and_mutates_memory() {
            let mut mapping = Mapping::anonymous(16).unwrap();
            assert_eq!(mapping.len(), 16);
            mapping.fill(0xAB);
            assert_eq!(mapping.as_slice(), &[0xAB; 16]);
            mapping.as_mut_slice()[3] = 7;
            assert_eq!(mapping.as_slice()[3], 7);
        }

        #[test]
        fn rejects_zero_length() {
            let err = match Mapping::anonymous(0) {
                Ok(_) => panic!("zero-length mapping succeeded"),
                Err(err) => err,
            };
            assert_eq!(err.kind(), ErrorKind::InvalidInput);
        }
    }
}

pub mod pinned_record {
    use std::marker::PhantomPinned;
    use std::pin::Pin;
    use std::ptr;

    pub struct StableRecord {
        name: String,
        name_ptr: *const String,
        _pin: PhantomPinned,
    }

    impl StableRecord {
        pub fn new(name: String) -> Pin<Box<Self>> {
            let mut boxed = Box::pin(Self {
                name,
                name_ptr: ptr::null(),
                _pin: PhantomPinned,
            });

            let name_ptr = &boxed.name as *const String;
            unsafe {
                Pin::as_mut(&mut boxed).get_unchecked_mut().name_ptr = name_ptr;
            }
            boxed
        }

        pub fn name(self: Pin<&Self>) -> &str {
            &self.get_ref().name
        }

        pub fn pointer_matches(self: Pin<&Self>) -> bool {
            ptr::eq(&self.get_ref().name, self.get_ref().name_ptr)
        }

        pub fn rename(self: Pin<&mut Self>, name: String) {
            let this = unsafe { self.get_unchecked_mut() };
            this.name = name;
            this.name_ptr = &this.name as *const String;
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn keeps_internal_pointer_valid() {
            let mut record = StableRecord::new("alpha".to_string());
            assert_eq!(record.as_ref().name(), "alpha");
            assert!(record.as_ref().pointer_matches());
            record.as_mut().rename("beta".to_string());
            assert_eq!(record.as_ref().name(), "beta");
            assert!(record.as_ref().pointer_matches());

            let moved_pin = record;
            assert_eq!(moved_pin.as_ref().name(), "beta");
            assert!(moved_pin.as_ref().pointer_matches());
        }
    }
}
