mod solution;

use solution::{make_pipe, read_to_end_fd, write_all_fd};
use std::os::fd::AsFd;

fn main() {
    let (read, write) = make_pipe().unwrap();
    write_all_fd(write.as_fd(), b"abcdef").unwrap();
    drop(write);
    let data = read_to_end_fd(read.as_fd(), 3).unwrap();
    println!("{}", String::from_utf8(data).unwrap());
}
