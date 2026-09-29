mod solution;

use solution::{make_pipe, read_exact_fd, set_nonblocking};
use std::io::ErrorKind;
use std::os::fd::AsFd;

fn main() {
    let (read, _write) = make_pipe().unwrap();
    set_nonblocking(read.as_fd(), true).unwrap();
    let err = read_exact_fd(read.as_fd(), 1).unwrap_err();
    println!("would_block={}", err.kind() == ErrorKind::WouldBlock);
    set_nonblocking(read.as_fd(), false).unwrap();
    println!("reset=ok");
}
