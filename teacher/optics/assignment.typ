// Included after the shared page and font settings.
#let assignment(topic) = [
#[
  #set text(size: 16pt, lang: "en")
  #set par(leading: 0.2em, spacing: 7pt)
  #show raw: set text(size: 14pt)
  #text(size: 26pt, weight: "bold")[Lens calculator]
  #v(4pt)
  Write a Rust program that reads a thin-lens system from a file and calculates its focal properties.
  #v(7pt)
  #line(length: 100%, stroke: 0.6pt)
  #v(10pt)
  #grid(columns: (1fr, 1fr), gutter: 12mm,
    [
      #topic[Input]
      One record per line, in the order encountered by light:
      #table(
        columns: (38mm, 1fr), stroke: none, inset: (x: 0pt, y: 4pt),
        [`lens F D`], [Focal length and clear diameter],
        [`gap L`], [Distance through air],
        [`stop D`], [Opening diameter],
      )
      All lengths are in mm. Negative $F$ means a diverging lens.
      #v(4pt)
      *Example: `examples/internal-stop.opt`*
      ```text
      lens 50 40
      gap 25
      stop 10
      ```
    ],
    [
      #topic[Output]
      ```text
      cargo run -- examples/internal-stop.opt
      ```
      ```text
      System length: 25.000 mm
      Effective focal length: 50.000 mm
      Back focus: 25.000 mm
      Entrance pupil diameter: 20.000 mm
      F-number at infinity: f/2.500
      Limiting opening: element 3
      ```
    ],
  )
  #v(10pt)
  #topic[Requirements]
  - Calculate with the paraxial thin-lens model on the following pages. Check lens openings as well as explicit stops.
  - Handle afocal and diverging systems, unbounded pupils and signed focus distances.
  - Report errors to stderr. Exit codes: `0` success, `1` input or calculation error, `2` incorrect arguments.
  #v(5pt)
  #line(length: 100%, stroke: 0.6pt)
  #v(7pt)
  *Tests:* `cargo test`
]
#pagebreak()
]
