mod solution;

fn main() {
    println!("{:?}", solution::split_extension("/tmp/archive.tar.gz"));
    println!("{:?}", solution::split_extension(".gitignore"));
}
