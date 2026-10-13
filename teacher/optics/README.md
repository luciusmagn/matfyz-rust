# Lens calculator

A reference implementation for the first Rust lab. The program reads a list of
optical elements and reports effective focal length, back focal distance,
entrance-pupil diameter, and photographic f-number at infinity.

The model uses centred ideal thin lenses in air, paraxial rays, and circular
openings. It describes first-order geometry; aberrations, diffraction, and
transmission losses require a richer model.

## Running

Rust 1.88 or newer, standard library only.

```sh
cd ~/lens-calculator
cargo run -- examples/compound.opt
```

Output:

```text
System length: 50.000 mm
Effective focal length: 61.538 mm
Back focus: 30.769 mm
Entrance pupil diameter: 12.500 mm
F-number at infinity: f/4.923
Limiting opening: element 3
```

## File format

One element per line, in the order encountered by light travelling left to right.
All lengths are in millimetres. Separate fields with spaces.

```text
lens 100 30
gap 20
stop 10
gap 30
lens 80 25
```

| Record | Meaning | Valid values |
|---|---|---|
| `lens F D` | Thin lens, focal length F, clear diameter D | F nonzero; D positive |
| `gap L` | Propagate through an air gap of length L | L nonnegative |
| `stop D` | Circular opening of diameter D | D positive |

Negative focal lengths represent diverging lenses. Numbers must be finite;
decimal and scientific notation are accepted. Each record must have exactly the
specified number of fields. Names are lowercase. At least one record is required.

Errors include source line numbers. In the result, element numbers count records,
including gaps, starting at one. If two openings impose exactly the same bound,
the first is reported.

Exit status: 0 for success, 1 for an input or calculation error, 2 for incorrect
command-line usage.

## Calculation

The implementation traces a reference ray starting parallel to the optical axis
at height `h0 = 1 mm`. Its state is height `y` and small-angle slope `u`:

- Air gap of length `d`: `y = y + d * u`.
- Thin lens of focal length `f`: `u = u - y / f`.
- Aperture stop: direction and height unchanged; impose a bound on ray height.

Track just the height and slope. One initially horizontal ray is enough for
these calculations.

At the final plane:

- Effective focal length: `-h0 / u`.
- Back focal distance: `-y / u`.

The second quantity is measured from the end of the input sequence. A trailing
air gap therefore changes the reported back focal distance. Positive distances
point to the right; a negative value puts the paraxial focus to the left of the
final plane. System length is the sum of all gaps.

An afocal system sends parallel input rays out parallel and has no finite focal
length. The implementation treats residual slope up to `1e-12` times the sum of
the absolute individual lens bends as numerical cancellation.

### Aperture

The physical opening and the entrance pupil can have different diameters.
For an opening of diameter `D` at reference-ray height `y`, the largest admitted
incident beam diameter is:

```text
D * h0 / abs(y)
```

Take the smallest such value over the lenses' clear apertures and explicit
stops. The reference ray itself may be too large to pass; linearity lets us
rescale it to find the accepted beam. This also accounts for a ray crossing the
axis and continuing on the other side.

At `y = 0`, a positive opening does not constrain this axial bundle. It may
constrain the field of view, which is a different calculation. A system containing
only gaps has an unbounded opening in this model.

For positive effective focal length and a finite entrance pupil,
`f-number = effective focal length / entrance-pupil diameter`. Diverging and
afocal systems are reported with their signed or infinite focal length and
without a photographic f-number.

## Examples

| File | Result to investigate |
|---|---|
| `single-lens.opt` | 50 mm focal length, 25 mm entrance pupil, f/2 |
| `two-lenses.opt` | 66.667 mm effective focal length, 33.333 mm back focus |
| `internal-stop.opt` | A physical 10 mm opening has a 20 mm entrance pupil |
| `compound.opt` | Two lenses and an internal aperture stop |
| `telescope.opt` | An afocal combination of two positive lenses |
| `diverging.opt` | Negative focal length and virtual focus |
| `stop-at-focus.opt` | An opening at the axial focus does not limit axial rays |

## Source and checks

- `src/main.rs`: parser, optical calculation, and command-line entry point.
- `src/tests.rs`: parser cases and comparisons with analytic optical results.
- `TEACHING.md`: Rust constructs and standard-library items to introduce.

```sh
cargo fmt --check
cargo test
cargo clippy --all-targets -- -D warnings
```

## Public test lenses

`examples/` contains 23 valid inputs. The named designs include Tessar, Cooke
triplet, double Gauss, retrofocus and telephoto teaching models. Additional cases cover pupil limits, signed focus distances and
afocal systems. See [the test catalogue](tests/README.md) for details and exact
numerical checks.

Run the solution-independent student test suite with:

```sh
cargo test --test lenses
```

The 34 integration tests call only the command-line program; they impose no Rust
API. Expected numbers are in `tests/expected.tsv`. Keep `tests/` and `examples/`
when preparing the empty student copy.

## Optics reference

- `optics-overview.pdf`: six pages: assignment, English and Czech optics overviews,
  separate English and Czech symbol-and-term pages, then the Tessar diagram.
- `optics-overview-student.pdf`: the first five pages, without the diagram.
- `optics-overview.typ`: shared source; `assignment.typ`: task page;
  `symbols.typ`: the two symbol-and-term pages.
- `tessar-layout.svg` and `tessar-rays.svg`: physical layout and thin-lens model.

Rebuild with Typst 0.14.2, Times New Roman MT Std and CMU Typewriter Text installed:

```sh
typst compile optics-overview.typ optics-overview.pdf
typst compile --input student=true optics-overview.typ optics-overview-student.pdf
```

## Special output values

Print the same six quantities in the order shown in the running example. Use
these complete lines when a numeric result is unavailable:

| Condition | Output line |
|---|---|
| Afocal system | `Effective focal length: infinite (afocal)` |
| Afocal system | `Back focus: none (afocal)` |
| No finite aperture bound | `Entrance pupil diameter: unbounded` |
| F-number not defined | `F-number: not applicable` |
| No limiting opening | `Limiting opening: none` |

For finite results, print decimal numbers with at least three digits after the
decimal point, using `.` as the decimal separator. Print diagnostics to stderr
and results to stdout.

## A4 printing

Use `optics-overview-a4.pdf` for the six-page full document or
`optics-overview-student-a4.pdf` for the five-page handout. Both use landscape A4.
Generate them after compiling the source PDFs:

```sh
pdfjam --paper a4paper --landscape --outfile optics-overview-a4.pdf optics-overview.pdf
pdfjam --paper a4paper --landscape --outfile optics-overview-student-a4.pdf optics-overview-student.pdf
```
