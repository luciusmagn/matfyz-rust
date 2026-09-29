mod solution;

use solution::{c_strings_from_block, join_c_strings};

fn main() {
    let block = join_c_strings(&["red", "", "blue"]).unwrap();
    println!("block={:?}", block);
    println!("roundtrip={:?}", c_strings_from_block(&block).unwrap());
    println!("bad={:?}", join_c_strings(&["ok", "no\0pe"]).unwrap_err());
}
