#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Add(i64),
    Sub(i64),
    Mul(i64),
    Reset,
    Print,
}

pub fn parse_op(line: &str) -> Option<Op> {
    let parts: Vec<_> = line.split_whitespace().collect();
    match parts.as_slice() {
        ["add", x] => x.parse().ok().map(Op::Add),
        ["sub", x] => x.parse().ok().map(Op::Sub),
        ["mul", x] => x.parse().ok().map(Op::Mul),
        ["reset"] => Some(Op::Reset),
        ["print"] => Some(Op::Print),
        _ => None,
    }
}

pub fn apply_op(state: &mut i64, op: &Op) -> Option<i64> {
    match op {
        Op::Add(x) => *state += x,
        Op::Sub(x) => *state -= x,
        Op::Mul(x) => *state *= x,
        Op::Reset => *state = 0,
        Op::Print => return Some(*state),
    }
    None
}

pub fn run_program(lines: &[&str]) -> Vec<i64> {
    let mut state = 0_i64;
    let mut out = Vec::new();

    for line in lines {
        if let Some(op) = parse_op(line) {
            if let Some(v) = apply_op(&mut state, &op) {
                out.push(v);
            }
        }
    }

    out
}
