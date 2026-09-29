use std::io;

fn main() {
    let number = rand::random::<u8>() % 10;

    println!("Welcome to the number game!");

    let mut buffer = String::new();

    while io::stdin().read_line(&mut buffer).is_ok() {
        // perhaps better with turbofish: parse::<u8>()
        let Ok(guess): Result<u8, _> = buffer.trim().parse() else {
            println!("?");
            continue;
        };

        if guess == number {
            println!("The number was indeed {number}!");
            break;
        } else if guess > number {
            println!("Wrong, try guessing lower");
        } else if guess < number {
            println!("Wrong, try guessing higher");
        }

        buffer.clear();
    }
}
