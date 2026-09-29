#[allow(dead_code)]
#[macro_use]
mod solution;

use solution::RenderLabel;

impl_render_label!(i32, usize);

fn main() {
    println!("{}", 7_i32.render_label());
    println!("{}", 3_usize.render_label());
}
