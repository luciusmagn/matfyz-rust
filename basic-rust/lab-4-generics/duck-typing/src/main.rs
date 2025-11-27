trait Duck {
    fn quack(&self) -> &str {
        "quack"
    }
    fn walk(&self) -> &str {
        "waddle, waddle"
    }
}

struct Human {}
struct Dog {}
struct Cat {}
struct Goose {}
struct Rock {}

impl Duck for Human {}
impl Duck for Dog {}
impl Duck for Cat {}
impl Duck for Goose {
    fn quack(&self) -> &str {
        "goose sounds"
    }
}
impl Duck for Rock {
    fn walk(&self) -> &str {
        "refuses to move"
    }
}

fn duck_filter(ducks: Vec<Box<dyn Duck>>) -> (Vec<Box<dyn Duck>>, Vec<Box<dyn Duck>>) {
    let mut yes = vec![];
    let mut no = vec![];

    for d in ducks {
        if d.walk() == "waddle, waddle" && d.quack() == "quack" {
            yes.push(d);
        } else {
            no.push(d);
        }
    }

    (yes, no)
}

#[test]
fn duck_filter_works() {
    let ducks: Vec<Box<dyn Duck>> = vec![
        Box::new(Human {}),
        Box::new(Dog {}),
        Box::new(Goose {}),
        Box::new(Cat {}),
        Box::new(Rock {}),
    ];
    let (yes, no) = duck_filter(ducks);
    assert!(
        yes.iter()
            .all(|d| d.quack() == "quack" && d.walk() == "waddle, waddle")
    );
    assert!(
        !no.iter()
            .any(|d| d.quack() == "quack" && d.walk() == "waddle, waddle")
    );
}

fn main() {}
