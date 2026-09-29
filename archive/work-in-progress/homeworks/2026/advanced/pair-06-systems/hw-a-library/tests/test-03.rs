mod solution;

use solution::c_strings_from_block;

fn main() {
    println!("empty={:?}", c_strings_from_block(b"").unwrap());
    println!("values={:?}", c_strings_from_block(b"alpha\0\0omega\0").unwrap());
    println!("bad={:?}", c_strings_from_block(b"alpha\0omega").unwrap_err());
}
