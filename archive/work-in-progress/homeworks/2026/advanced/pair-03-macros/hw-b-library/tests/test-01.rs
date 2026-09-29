#[allow(dead_code)]
#[macro_use]
mod solution;

fn main() {
    println!("{}", count_tts!());
    println!("{}", count_tts!(foo bar (baz quux) [x y] {z}));
}
