mod solution;

use solution::RawFixedVec;

fn main() {
    let mut vec = RawFixedVec::with_capacity(2);
    println!("cap={} len={} empty={}", vec.capacity(), vec.len(), vec.is_empty());
    println!("push1={:?}", vec.push(10));
    println!("push2={:?}", vec.push(20));
    println!("push3={:?}", vec.push(30));
    println!("slice={:?}", vec.as_slice());
}
