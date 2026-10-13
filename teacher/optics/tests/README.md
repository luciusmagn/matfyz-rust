# Public lens tests

Run `cargo test --test lenses` from the project root. The tests launch the
`lens-calculator` binary with an input filename. They use no implementation
functions, types or modules, so they can be copied into the student repository.
Rust 1.88 and the standard library are sufficient.

## Inputs and expected results

- `../examples/*.opt`: 23 valid optical inputs, including the named teaching models.
- `expected.tsv`: system length, effective focal length, focus distance from the
  final input plane, entrance-pupil diameter, f-number and limiting element.
- `invalid/*.opt`: nine malformed or numerically overflowing inputs.
- `lenses.rs`: 34 tests, including command-line argument and file errors.

The output format is specified in the project README. Numeric results may differ
by `0.00051 + 1e-12 * abs(expected)` to accommodate output rounded to three decimal
places. Nonfinite numeric output is rejected. A dash in the expected-results table
means afocal, unbounded, not applicable or no limiting opening, according to the
column. Limiting-element indices count all input records, including gaps.

Error tests check the exit status and source location where applicable, rather
than requiring a particular explanation. Successful output uses the documented
six result labels, in order. Leading/trailing whitespace and blank output lines
are ignored.

## Lens families

| File | Arrangement and purpose |
|---|---|
| `tessar-model.opt` | Positive singlet, negative singlet, stop, equivalent positive rear doublet; the board example |
| `cooke-triplet-model.opt` | Three separated lenses, positive / negative / positive |
| `double-gauss-model.opt` | Six lenses, symmetric about the stop: + + − / − + + |
| `retrofocus-model.opt` | Negative front group, positive rear group; back focus exceeds effective focal length |
| `telephoto-model.opt` | Positive front group, negative rear group; front-to-focus distance is shorter than focal length |
| `telescope.opt` | Two positive lenses forming an afocal Keplerian telescope |
| `galilean-telescope.opt` | Positive objective and negative eyepiece forming an afocal Galilean telescope |

The five `*-model.opt` designs have invented first-order parameters. Their names
identify the arrangement being illustrated. Each `lens` record represents an
ideal thin lens or a group reduced to one thin lens. In particular, the Tessar
rear doublet is one record. Glass types, surface curvatures, thicknesses and
aberration correction are outside this input model.

For the physical Tessar arrangement, see
[H. H. Nasse, *Tessar*, ZEISS, 2011](https://lenspire.zeiss.com/photo/app/uploads/2022/02/technical-article-lens-names-tessar.pdf).

Other cases cover lenses in contact, an opening before the first lens, a
lens rim as the limiting opening, a smaller physical hole that is not limiting,
stops at and beyond a focus, a trailing gap, a negative lens, equal aperture
bounds, a weak but finite lens, empty air and an aperture without lenses.

## Independent optical values

All 23 expected cases were independently checked by exact rational ray tracing
in Common Lisp. Start with height `y = 1` mm and slope `u = 0`:

- Gap of length `d`: update `y = y + d * u`.
- Lens of focal length `f`: update `u = u - y / f`.
- At each opening: compare `diameter / abs(y)` with the current pupil limit;
  skip `y = 0`.
- At the end: `focal_length = -1 / u`, `back_focus = -y / u`; zero slope is afocal.

For the Tessar model, the final height is `68/75` mm and slope is `-7/375`.
The focal length is `375/7` mm and back focus is `340/7` mm. The four aperture
bounds are `30`, `360/13`, `1200/133` and `450/17` mm. The stop is record 5, its
entrance pupil is `1200/133` mm, and its f-number is `95/16`.

## Student starter

- `../lens-calculator-student/`: crate, examples and tests, without PDFs.
- `src/main.rs`: `Analysis`, an unimplemented `calculate` and an empty `main`.
- `src/tests.rs`: 16 tests adapted to `calculate(&str) -> Result<Analysis, String>`.
- `tests/lenses.rs`: 34 executable-interface tests.
