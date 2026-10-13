use super::*;

fn close(actual: f64, expected: f64) {
    let tolerance = 1e-9 * expected.abs().max(1.0);
    assert!(
        (actual - expected).abs() <= tolerance,
        "{actual} != {expected}"
    );
}

fn calculate(text: &str) -> Analysis {
    let elements = parse_elements(text).expect("valid optical prescription");
    analyse(&elements).expect("finite optical calculation")
}

#[test]
fn single_lens() {
    let result = calculate("lens 50 25");
    close(result.focal_length.unwrap(), 50.0);
    close(result.back_focus.unwrap(), 50.0);
    close(result.entrance_pupil.unwrap(), 25.0);
    close(result.f_number.unwrap(), 2.0);
    assert_eq!(result.limiting_element, Some(1));
}

#[test]
fn two_lenses_match_the_analytic_formula() {
    for first in [50.0, 100.0, -75.0] {
        for second in [80.0, 150.0, -40.0] {
            for gap in [0.0, 20.0, 60.0] {
                let text = format!("lens {first} 30\ngap {gap}\nlens {second} 30");
                let result = calculate(&text);
                if first + second - gap == 0.0 {
                    assert!(result.focal_length.is_none());
                    assert!(result.back_focus.is_none());
                    continue;
                }
                let expected = first * second / (first + second - gap);
                close(result.focal_length.unwrap(), expected);
                close(result.back_focus.unwrap(), expected * (1.0 - gap / first));
                close(result.length, gap);
            }
        }
    }
}

#[test]
fn pupil_is_the_apparent_opening_not_the_physical_stop() {
    let result = calculate("lens 50 40\ngap 25\nstop 10");
    close(result.focal_length.unwrap(), 50.0);
    close(result.back_focus.unwrap(), 25.0);
    close(result.entrance_pupil.unwrap(), 20.0);
    close(result.f_number.unwrap(), 2.5);
    assert_eq!(result.limiting_element, Some(3));
}

#[test]
fn smallest_physical_opening_need_not_limit_the_beam() {
    let result = calculate("lens 50 30\ngap 25\nstop 20");
    close(result.entrance_pupil.unwrap(), 30.0);
    assert_eq!(result.limiting_element, Some(1));
}

#[test]
fn later_lens_can_limit_the_beam() {
    let result = calculate("lens 100 40\ngap 50\nlens 100 10");
    close(result.entrance_pupil.unwrap(), 20.0);
    assert_eq!(result.limiting_element, Some(3));
}

#[test]
fn stop_before_lens() {
    let result = calculate("stop 10\ngap 20\nlens 50 30");
    close(result.entrance_pupil.unwrap(), 10.0);
    close(result.f_number.unwrap(), 5.0);
    close(result.back_focus.unwrap(), 50.0);
}

#[test]
fn compound_example() {
    let result = calculate(include_str!("../examples/compound.opt"));
    close(result.focal_length.unwrap(), 8000.0 / 130.0);
    close(result.back_focus.unwrap(), 4000.0 / 130.0);
    close(result.entrance_pupil.unwrap(), 12.5);
    close(result.f_number.unwrap(), 640.0 / 130.0);
}

#[test]
fn final_gap_changes_back_focus_but_not_effective_focal_length() {
    let result = calculate("lens 50 25\ngap 80");
    close(result.focal_length.unwrap(), 50.0);
    close(result.back_focus.unwrap(), -30.0);
    close(result.length, 80.0);
}

#[test]
fn stop_at_axial_focus_does_not_clip_the_axial_bundle() {
    let result = calculate(include_str!("../examples/stop-at-focus.opt"));
    close(result.entrance_pupil.unwrap(), 20.0);
    close(result.back_focus.unwrap(), 0.0);
    assert_eq!(result.limiting_element, Some(1));
}

#[test]
fn opening_after_a_focus_uses_absolute_ray_height() {
    let result = calculate("lens 50 40\ngap 75\nstop 5");
    close(result.entrance_pupil.unwrap(), 10.0);
    close(result.f_number.unwrap(), 5.0);
}

#[test]
fn afocal_telescope_including_floating_point_cancellation() {
    for text in [
        "lens 100 30\ngap 150\nlens 50 30",
        "lens 150 30\ngap 200\nlens 50 30",
    ] {
        let result = calculate(text);
        assert!(result.focal_length.is_none());
        assert!(result.back_focus.is_none());
        assert!(result.f_number.is_none());
        close(result.entrance_pupil.unwrap(), 30.0);
    }
}

#[test]
fn diverging_lens_has_negative_focal_length() {
    let result = calculate("lens -50 20");
    close(result.focal_length.unwrap(), -50.0);
    close(result.back_focus.unwrap(), -50.0);
    close(result.entrance_pupil.unwrap(), 20.0);
    assert!(result.f_number.is_none());
}

#[test]
fn aperture_only_and_gap_only_systems() {
    let aperture = calculate("gap 10\nstop 8");
    assert!(aperture.focal_length.is_none());
    close(aperture.entrance_pupil.unwrap(), 8.0);
    let air = calculate("gap 10");
    assert!(air.focal_length.is_none());
    assert!(air.entrance_pupil.is_none());
    assert!(air.limiting_element.is_none());
}

#[test]
fn long_focal_length_is_not_automatically_afocal() {
    let result = calculate("lens 1e15 20");
    close(result.focal_length.unwrap(), 1e15);
}

#[test]
fn parser_rejects_malformed_records() {
    for text in [
        "",
        "mirror 10",
        "Lens 50 25",
        "lens",
        "lens 50",
        "lens 50 25 10",
        "gap",
        "gap 1 2",
        "stop",
        "stop 10 20",
        "lens zero 25",
        "gap 10mm",
        "lens 0 25",
        "lens -0 25",
        "lens 50 0",
        "lens 50 -2",
        "gap -1",
        "stop 0",
        "stop -1",
        "lens NaN 25",
        "lens inf 25",
        "stop NaN",
        "stop inf",
        "gap -inf",
        "gap 1e999",
    ] {
        assert!(parse_elements(text).is_err(), "accepted {text:?}");
    }
}

#[test]
fn arithmetic_overflow_is_reported() {
    let elements = parse_elements("lens 1e-308 20\ngap 1e308").unwrap();
    assert!(analyse(&elements).is_err());
}
