mod solution;

use solution::{make_pipe, read_exact_fd, write_all_fd};
use std::os::fd::AsFd;

fn main() {
    let (read, write) = make_pipe().unwrap();
    write_all_fd(write.as_fd(), b"hello").unwrap();
    let data = read_exact_fd(read.as_fd(), 5).unwrap();
    println!("{}", String::from_utf8(data).unwrap());
}
