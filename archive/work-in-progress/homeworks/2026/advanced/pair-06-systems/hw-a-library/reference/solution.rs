use std::ffi::{CStr, CString};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CStringBoundaryError {
    InteriorNul(usize),
    MissingTerminator,
}

pub fn nul_terminated_bytes(input: &str) -> Result<Vec<u8>, CStringBoundaryError> {
    match CString::new(input) {
        Ok(c) => Ok(c.into_bytes_with_nul()),
        Err(err) => Err(CStringBoundaryError::InteriorNul(err.nul_position())),
    }
}

pub fn strlen_checked(bytes: &[u8]) -> Result<usize, CStringBoundaryError> {
    if !bytes.contains(&0) {
        return Err(CStringBoundaryError::MissingTerminator);
    }
    let len = unsafe { libc::strlen(bytes.as_ptr().cast()) };
    Ok(len as usize)
}

pub fn c_strings_from_block(bytes: &[u8]) -> Result<Vec<String>, CStringBoundaryError> {
    if bytes.is_empty() {
        return Ok(Vec::new());
    }
    if bytes.last() != Some(&0) {
        return Err(CStringBoundaryError::MissingTerminator);
    }

    let mut out = Vec::new();
    let mut start = 0;
    for (idx, byte) in bytes.iter().copied().enumerate() {
        if byte == 0 {
            let c = CStr::from_bytes_with_nul(&bytes[start..=idx])
                .map_err(|_| CStringBoundaryError::InteriorNul(start))?;
            out.push(c.to_str().map_err(|_| CStringBoundaryError::InteriorNul(start))?.to_string());
            start = idx + 1;
        }
    }
    Ok(out)
}

pub fn join_c_strings(values: &[&str]) -> Result<Vec<u8>, CStringBoundaryError> {
    let mut out = Vec::new();
    for value in values {
        out.extend(nul_terminated_bytes(value)?);
    }
    Ok(out)
}
