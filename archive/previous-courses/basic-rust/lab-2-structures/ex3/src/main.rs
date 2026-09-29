use std::io::{Write, stdin, stdout};

fn main() {
    let mut buffer = String::new();

    print!(">");
    // NOTE: stdout is line-buffered, we'll have to manually flush it
    let _ = stdout().flush();

    while stdin().read_line(&mut buffer).is_ok() {
        let mut s = Vec::<i32>::new();

        for token in buffer.split_whitespace() {
            let res = match token {
                op @ ("+" | "-") => {
                    let Some(lhs) = s.pop() else {
                        println!("Error: wrong amount of operands");
                        break;
                    };
                    let Some(rhs) = s.pop() else {
                        println!("Error: wrong amount of operands");
                        break;
                    };

                    match op {
                        "+" => lhs + rhs,
                        "-" => lhs - rhs,
                        _ => unreachable!(),
                    }
                }
                num => {
                    let Ok(n) = num.parse() else {
                        println!("Error: '{num}' is not a number or an operator");
                        break;
                    };
                    n
                }
            };

            s.push(res);
        }

        if let Some(val) = s.pop() {
            println!("{val}");
        };

        print!(">");
        let _ = stdout().flush();

        buffer.clear();
    }
}
