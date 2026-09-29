#[allow(dead_code)]
#[macro_use]
mod solution;

use solution::RenderLabel;

impl_render_label!(u8, u16,);

const VALUE: i64 = prefix_calc!((sub (add (value 10) (value 5)) (neg (value 4))));

fn main() {
    println!("{VALUE}");
    println!("{}", count_tts!(alpha (beta gamma) delta));
    println!("{}", 5_u8.render_label());
    println!("{}", 8_u16.render_label());
}
