#[derive(Debug, Clone)]
struct Pair<T: std::fmt::Debug + Clone, U: std::fmt::Debug + Clone> {
    fst: T,
    snd: U,
}

struct A {}

impl<T: std::fmt::Debug + Clone, U: std::fmt::Debug + Clone> Pair<T, U> {
    fn new(fst: T, snd: U) -> Self {
        Self { fst, snd }
    }

    fn fst(&self) -> &T {
        &self.fst
    }

    fn snd(&self) -> &U {
        &self.snd
    }

    fn swap(self) -> Pair<U, T> {
        Pair {
            fst: self.snd,
            snd: self.fst,
        }
    }
}

fn compose<
    T1: std::fmt::Debug + Clone,
    U1: std::fmt::Debug + Clone,
    T2: std::fmt::Debug + Clone,
    U2: std::fmt::Debug + Clone,
>(
    p1: Pair<T1, U1>,
    p2: Pair<T2, U2>,
) -> Pair<Pair<T1, U1>, Pair<T2, U2>>
where
{
    Pair::new(p1, p2)
}

#[test]
fn fst_and_snd_work() {
    let p = Pair::new(42usize, true);
    assert_eq!(p.fst(), &42);
    assert!(p.snd());
}

#[test]
fn swap_works() {
    let p = Pair::new(42usize, false);
    let swapped = p.swap();

    assert_eq!(swapped.fst(), &false);
    assert_eq!(swapped.snd(), &42);
}

#[test]
fn compose_works() {
    let p1 = Pair::new(42usize, false);
    let p2 = Pair::new(String::from("hi"), 42.0);

    let comp = compose(p1, p2);
    assert_eq!(comp.snd().fst(), &String::from("hi"));
}

fn main() {
    let p = Pair::new(1, 2);
    // NOTE: pair should be Debug and Clone
    dbg!(p.clone());

    // this shouldn't compile:
    //let p2 = compose(1, 2);
}
