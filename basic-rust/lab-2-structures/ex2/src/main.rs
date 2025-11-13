enum Roots {
    One(f32),
    Two(f32, f32),
    Complex,
}

fn solve_quadratic(a: f32, b: f32, c: f32) -> Roots {
    let d = (b.powi(2) - 4.0 * a * c).sqrt();

    if d.is_nan() {
        return Roots::Complex;
    }

    let root1 = (-b + d) / (2.0 * a);
    let root2 = (-b - d) / (2.0 * a);

    if root1 == root2 {
        Roots::One(root1)
    } else {
        Roots::Two(root1, root2)
    }
}

fn main() {
    // 2x^2+3x-5 = 0
    // Solution: x = 1 or x = -2.5
    let (a, b, c) = (2.0, 3.0, -5.0);

    use Roots::*;
    match solve_quadratic(a, b, c) {
        Complex => println!("Roots are complex"),
        One(root) => println!("x = {root}"),
        Two(root1, root2) => println!("x = {root1} or x = {root2}"),
    }
}
