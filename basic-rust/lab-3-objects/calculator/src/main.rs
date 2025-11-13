#[derive(Debug)]
enum Term {
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
    Num(i32),
}

type ParseOption<'a, T> = Option<(&'a str, T)>;

fn parse_spaces<'a>(s: &'a str) -> ParseOption<'a, ()> {
    match s.strip_prefix(" ") {
        Some(rest) => parse_spaces(rest),
        None => Some((s, ())),
    }
}

fn parse_binary_op<'a>(s: &'a str) -> ParseOption<'a, fn(Box<Term>, Box<Term>) -> Term> {
    match s.split_at_checked(1) {
        Some((op @ ("+" | "-" | "*") , rest)) => {
            Some((rest, match op {
                "+" => Term::Add,
                "-" => Term::Sub,
                "*" => Term::Mul,
                _ => unreachable!()
            }))
        }
        _ => None
    }
}

fn parse_num<'a>(s: &'a str) -> ParseOption<'a, Term> {
    let num: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();

    if num.is_empty() {
        None
    } else {
        // NOTE: be careful when indexing str-slices
        Some((&s[num.len()..], Term::Num(num.parse().ok()?)))
    }
}

fn parse_term<'a>(s: &'a str) -> ParseOption<'a, Term> {
    if let Some((rest, op)) = parse_binary_op(s) {
        let (rest, _) = parse_spaces(rest)?;
        let (rest, term1) = parse_term(rest)?;
        let (rest, _) = parse_spaces(rest)?;
        let (rest, term2) = parse_term(rest)?;

        Some((rest, op(Box::new(term1), Box::new(term2))))
    } else {
        parse_num(s)
    }
}

fn eval(t: Term) -> i32 {
    match t {
        Term::Num(n) => n,
        Term::Add(t1, t2) => eval(*t1) + eval(*t2),
        Term::Sub(t1, t2) => eval(*t1) - eval(*t2),
        Term::Mul(t1, t2) => eval(*t1) * eval(*t2),
    }
}

fn main() {
    let (_, parsed) = dbg!(parse_term("+ 1 * 2 3")).unwrap();
    println!("{}", eval(parsed))
}
