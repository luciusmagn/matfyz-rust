use std::env;
use std::fs;
use std::process;

// Every length is in millimetres. A negative focal length means a diverging lens.
enum Element {
    Lens(f64, f64), // focal length, clear diameter
    Gap(f64),       // distance to travel through air
    Stop(f64),      // opening diameter
}

struct Analysis {
    length: f64,
    focal_length: Option<f64>,
    back_focus: Option<f64>,
    entrance_pupil: Option<f64>,
    f_number: Option<f64>,
    limiting_element: Option<usize>,
}

fn parse_number(word: &str, line: usize) -> Result<f64, String> {
    match word.parse::<f64>() {
        Ok(number) => {
            if number.is_finite() {
                Ok(number)
            } else {
                Err(format!("line {line}: numbers must be finite"))
            }
        }
        Err(_) => Err(format!("line {line}: expected a number, got '{word}'")),
    }
}

fn parse_elements(text: &str) -> Result<Vec<Element>, String> {
    let mut elements = Vec::new();

    for (index, line) in text.lines().enumerate() {
        let line_number = index + 1;
        let words: Vec<&str> = line.split_whitespace().collect();
        if words.is_empty() {
            return Err(format!("line {line_number}: expected an optical element"));
        }

        // Check the number of words before indexing them.
        match words[0] {
            "lens" => {
                if words.len() != 3 {
                    return Err(format!(
                        "line {line_number}: expected 'lens FOCAL DIAMETER'"
                    ));
                }
                let focal = parse_number(words[1], line_number)?;
                let diameter = parse_number(words[2], line_number)?;
                if focal == 0.0 {
                    return Err(format!("line {line_number}: focal length must be nonzero"));
                }
                if diameter <= 0.0 {
                    return Err(format!("line {line_number}: diameter must be positive"));
                }
                elements.push(Element::Lens(focal, diameter));
            }
            "gap" => {
                if words.len() != 2 {
                    return Err(format!("line {line_number}: expected 'gap DISTANCE'"));
                }
                let distance = parse_number(words[1], line_number)?;
                if distance < 0.0 {
                    return Err(format!("line {line_number}: distance must be nonnegative"));
                }
                elements.push(Element::Gap(distance));
            }
            "stop" => {
                if words.len() != 2 {
                    return Err(format!("line {line_number}: expected 'stop DIAMETER'"));
                }
                let diameter = parse_number(words[1], line_number)?;
                if diameter <= 0.0 {
                    return Err(format!("line {line_number}: diameter must be positive"));
                }
                elements.push(Element::Stop(diameter));
            }
            name => {
                return Err(format!("line {line_number}: unknown element '{name}'"));
            }
        }
    }

    if elements.is_empty() {
        return Err(String::from("expected at least one optical element"));
    }
    Ok(elements)
}

fn analyse(elements: &[Element]) -> Result<Analysis, String> {
    // Follow one initially parallel ray, starting 1 mm above the axis.
    // Scaling the starting height scales every ray height by the same factor.
    let initial_height = 1.0;
    let mut height = initial_height;
    let mut slope = 0.0;
    let mut length = 0.0;
    let mut entrance_pupil = None;
    let mut limiting_element = None;
    let mut total_bending = 0.0;

    for (index, element) in elements.iter().enumerate() {
        // The numeric fields are copied out; the element itself is borrowed.
        let opening = match *element {
            Element::Lens(focal, diameter) => {
                let bending = height / focal;
                slope -= bending;
                total_bending += bending.abs();
                // A thin lens changes direction, but not height at its plane.
                Some(diameter)
            }
            Element::Gap(distance) => {
                height += distance * slope;
                length += distance;
                None
            }
            Element::Stop(diameter) => Some(diameter),
        };

        if !height.is_finite()
            || !slope.is_finite()
            || !length.is_finite()
            || !total_bending.is_finite()
        {
            return Err(format!("element {}: numerical overflow", index + 1));
        }

        if let Some(diameter) = opening {
            // At an axial focus, this ray has height zero. A positive
            // opening there does not restrict this on-axis ray bundle.
            if height != 0.0 {
                let candidate = diameter * initial_height / height.abs();
                if candidate < entrance_pupil.unwrap_or(f64::INFINITY) {
                    entrance_pupil = Some(candidate);
                    limiting_element = Some(index + 1);
                }
            }
        }
    }

    let mut focal_length = None;
    let mut back_focus = None;
    // Allow for cancellation error when the lenses form an afocal system.
    // This is relative to the individual bends, not a fixed focal-length cutoff.
    if slope.abs() > 1e-12 * total_bending {
        let focal = -initial_height / slope;
        let back = -height / slope;
        if !focal.is_finite() || !back.is_finite() {
            return Err(String::from("focal distance exceeds the numerical range"));
        }
        focal_length = Some(focal);
        back_focus = Some(back);
    }

    let mut f_number = None;
    if let (Some(focal), Some(diameter)) = (focal_length, entrance_pupil) {
        if focal > 0.0 {
            let number = focal / diameter;
            if !number.is_finite() {
                return Err(String::from("f-number exceeds the numerical range"));
            }
            f_number = Some(number);
        }
    }

    Ok(Analysis {
        length,
        focal_length,
        back_focus,
        entrance_pupil,
        f_number,
        limiting_element,
    })
}

fn print_analysis(result: &Analysis) {
    println!("System length: {:.3} mm", result.length);
    match result.focal_length {
        Some(value) => println!("Effective focal length: {value:.3} mm"),
        None => println!("Effective focal length: infinite (afocal)"),
    }
    match result.back_focus {
        Some(value) => println!("Back focus: {value:.3} mm"),
        None => println!("Back focus: none (afocal)"),
    }
    match result.entrance_pupil {
        Some(value) => println!("Entrance pupil diameter: {value:.3} mm"),
        None => println!("Entrance pupil diameter: unbounded"),
    }
    match result.f_number {
        Some(value) => println!("F-number at infinity: f/{value:.3}"),
        None => println!("F-number: not applicable"),
    }
    match result.limiting_element {
        Some(index) => println!("Limiting opening: element {index}"),
        None => println!("Limiting opening: none"),
    }
}

fn run(filename: &str) -> Result<Analysis, String> {
    let text = match fs::read_to_string(filename) {
        Ok(text) => text,
        Err(error) => return Err(format!("cannot read '{filename}': {error}")),
    };
    let elements = parse_elements(&text)?;
    analyse(&elements)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: lens-calculator FILE");
        process::exit(2);
    }

    match run(&args[1]) {
        Ok(result) => print_analysis(&result),
        Err(error) => {
            eprintln!("Error: {error}");
            process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests;
