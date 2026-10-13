//! Public, black-box tests. Only the executable interface is required.
use std::path::Path;
use std::process::{Command, Output};

fn run_file(relative: &str) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lens-calculator"))
        .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join(relative))
        .output()
        .expect("run lens-calculator")
}

fn quantity(line: &str, prefix: &str, suffix: &str, absent: &str) -> Option<f64> {
    if line == absent {
        return None;
    }
    let number = line
        .strip_prefix(prefix)
        .and_then(|s| s.strip_suffix(suffix))
        .unwrap_or_else(|| panic!("unrecognised result line: {line:?}"))
        .trim()
        .parse::<f64>()
        .expect("numeric result");
    assert!(number.is_finite(), "nonfinite numeric result: {line}");
    Some(number)
}

fn check_lens(name: &str) {
    let expected: Vec<&str> = include_str!("expected.tsv")
        .lines()
        .map(|line| line.split_whitespace().collect::<Vec<_>>())
        .find(|fields| fields.first() == Some(&name))
        .expect("fixture has expected results");
    assert_eq!(expected.len(), 7);
    let output = run_file(&format!("examples/{name}.opt"));
    assert_eq!(
        output.status.code(),
        Some(0),
        "{name}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("UTF-8 output");
    let lines: Vec<_> = stdout
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect();
    assert_eq!(
        lines.len(),
        6,
        "{name}: expected six result lines\n{stdout}"
    );
    let actual = [
        quantity(lines[0], "System length:", "mm", ""),
        quantity(
            lines[1],
            "Effective focal length:",
            "mm",
            "Effective focal length: infinite (afocal)",
        ),
        quantity(lines[2], "Back focus:", "mm", "Back focus: none (afocal)"),
        quantity(
            lines[3],
            "Entrance pupil diameter:",
            "mm",
            "Entrance pupil diameter: unbounded",
        ),
        quantity(
            lines[4],
            "F-number at infinity: f/",
            "",
            "F-number: not applicable",
        ),
    ];
    for (index, got) in actual.into_iter().enumerate() {
        let wanted = expected[index + 1];
        if wanted == "-" {
            assert!(
                got.is_none(),
                "{name}, field {index}: expected no finite value"
            );
        } else {
            let wanted = wanted.parse::<f64>().unwrap();
            let got = got.unwrap_or_else(|| panic!("{name}, field {index}: missing value"));
            // Half a unit at three decimal places, plus large-value roundoff.
            let tolerance = 0.00051 + 1e-12 * wanted.abs();
            assert!(
                (got - wanted).abs() <= tolerance,
                "{name}, field {index}: {got} != {wanted} (tolerance {tolerance})"
            );
        }
    }
    let limiting = if lines[5] == "Limiting opening: none" {
        None
    } else {
        Some(
            lines[5]
                .strip_prefix("Limiting opening: element ")
                .expect("limiting opening result")
                .parse::<usize>()
                .expect("element number"),
        )
    };
    let wanted = if expected[6] == "-" {
        None
    } else {
        Some(expected[6].parse::<usize>().unwrap())
    };
    assert_eq!(limiting, wanted, "{name}: limiting opening");
}

macro_rules! lens_tests {
    ($($test:ident => $file:literal),* $(,)?) => {
        $(#[test] fn $test() { check_lens($file); })*
    };
}

lens_tests! {
    single_lens => "single-lens",
    two_lenses => "two-lenses",
    internal_stop => "internal-stop",
    compound => "compound",
    keplerian_telescope => "telescope",
    diverging => "diverging",
    stop_at_focus => "stop-at-focus",
    tessar => "tessar-model",
    cooke_triplet => "cooke-triplet-model",
    double_gauss => "double-gauss-model",
    retrofocus => "retrofocus-model",
    telephoto => "telephoto-model",
    galilean_telescope => "galilean-telescope",
    contact_pair => "contact-pair",
    trailing_gap => "trailing-gap",
    front_stop => "front-stop",
    rear_lens_limited => "rear-lens-limited",
    smaller_hole => "smaller-hole",
    stop_after_focus => "stop-after-focus",
    aperture_only => "aperture-only",
    air_only => "air-only",
    weak_lens => "weak-lens",
    tied_openings => "tied-openings",
}

fn check_error(file: &str, location: Option<&str>) {
    let output = run_file(&format!("tests/invalid/{file}.opt"));
    assert_eq!(
        output.status.code(),
        Some(1),
        "{file}: expected input error"
    );
    let stderr = String::from_utf8(output.stderr).expect("UTF-8 error");
    assert!(!stderr.trim().is_empty(), "{file}: missing diagnostic");
    if let Some(location) = location {
        assert!(
            stderr.contains(location),
            "{file}: expected {location:?}, got {stderr:?}"
        );
    }
}

macro_rules! error_tests {
    ($($test:ident => ($file:literal, $location:expr)),* $(,)?) => {
        $(#[test] fn $test() { check_error($file, $location); })*
    };
}

error_tests! {
    empty_input => ("empty", None),
    zero_focal => ("zero-focal", Some("line 1:")),
    zero_diameter => ("zero-diameter", Some("line 2:")),
    negative_gap => ("negative-gap", Some("line 2:")),
    nonfinite => ("nonfinite", Some("line 1:")),
    unknown_record => ("unknown", Some("line 1:")),
    extra_field => ("extra-field", Some("line 1:")),
    bad_number => ("bad-number", Some("line 1:")),
    overflow => ("overflow", Some("element 2:")),
}

#[test]
fn incorrect_argument_count() {
    for args in [vec![], vec!["one.opt", "two.opt"]] {
        let output = Command::new(env!("CARGO_BIN_EXE_lens-calculator"))
            .args(args)
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(2));
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn missing_file() {
    // A regular fixture file cannot also be a directory.
    let output = run_file("examples/single-lens.opt/missing.opt");
    assert_eq!(output.status.code(), Some(1));
    assert!(!output.stderr.is_empty());
}
