mod solution;

use solution::RawFixedVec;

fn main() {
    let mut vec = RawFixedVec::with_capacity(3);
    vec.push(String::from("a")).unwrap();
    vec.push(String::from("bb")).unwrap();
    vec.push(String::from("ccc")).unwrap();
    vec.get_mut(1).unwrap().push('!');
    println!("{:?}", vec.as_slice());
    println!("{:?}", vec.pop());
    println!("{:?}", vec.pop());
    println!("{:?}", vec.pop());
    println!("{:?}", vec.pop());
}
