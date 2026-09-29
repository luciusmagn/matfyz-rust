pub trait RenderLabel {
    fn render_label(&self) -> String;
}

#[macro_export]
macro_rules! count_tts {
    () => {
        0usize
    };
    ($head:tt $($tail:tt)*) => {
        1usize + $crate::count_tts!($($tail)*)
    };
}

#[macro_export]
macro_rules! prefix_calc {
    ((value $expr:expr)) => {
        ($expr)
    };
    ((neg $expr:tt)) => {
        -$crate::prefix_calc!($expr)
    };
    ((add $left:tt $right:tt)) => {
        $crate::prefix_calc!($left) + $crate::prefix_calc!($right)
    };
    ((sub $left:tt $right:tt)) => {
        $crate::prefix_calc!($left) - $crate::prefix_calc!($right)
    };
    ((mul $left:tt $right:tt)) => {
        $crate::prefix_calc!($left) * $crate::prefix_calc!($right)
    };
    ((div $left:tt $right:tt)) => {
        $crate::prefix_calc!($left) / $crate::prefix_calc!($right)
    };
}

#[macro_export]
macro_rules! impl_render_label {
    ($($t:ty),+ $(,)?) => {
        $(
            impl $crate::solution::RenderLabel for $t {
                fn render_label(&self) -> String {
                    format!("{}={}", stringify!($t), self)
                }
            }
        )+
    };
}
