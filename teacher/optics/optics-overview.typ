// Rebuild: typst compile optics-overview.typ optics-overview.pdf
#set document(title: "Smol optics reference", author: "Lukáš Hozda")
#set page(width: 384mm, height: 216mm, margin: (x: 12mm, y: 8mm))
#set text(font: "Times New Roman MT Std", size: 14pt, lang: "en",
  top-edge: 0.8em, bottom-edge: -0.2em)
#set par(leading: 0.12em, spacing: 4pt)
#set list(indent: 10pt, body-indent: 5pt, spacing: 3pt)
#set math.equation(numbering: none)
#show math.equation.where(block: true): set text(size: 15.5pt)
#show math.equation.where(block: true): set block(above: 4pt, below: 4pt)
#show raw: set text(font: "CMU Typewriter Text", size: 12pt)

#let topic(title) = block(above: 0pt, below: 7pt)[
  #text(size: 18pt, weight: "bold", title)
]

#import "assignment.typ": assignment
#assignment(topic)

#text(size: 26pt, weight: "bold")[Smol optics reference]
#v(2pt)
Centred thin lenses in air · paraxial rays · object at infinity · all lengths in millimetres
#v(5pt)
#line(length: 100%, stroke: 0.6pt)
#v(7pt)

#grid(
  columns: (1fr, 1fr, 1fr),
  gutter: 9mm,
  [
    #topic[1. Ray model]
    - $z$: optical axis, positive to the right.
    - $y$: signed height above the axis.
    - $u$: ray slope, the height change per 1 mm of forward travel. Positive slopes go up, negative slopes go down; $u = 0$ is horizontal.

    #v(3pt)
    *Air gap of length $d$*
    $ y' = y + d u, quad u' = u $

    *Thin lens of focal length $f$*
    $ y' = y, quad u' = u - y/f $
    $f > 0$: converging; $f < 0$: diverging.

    *Opening of diameter $D$:*
    A ray passes if $abs(y) <= D/2$. Direction is unchanged. Each lens has a clear opening too.

  ],
  [
    #topic[2. System focus]
    Trace one initially parallel reference ray:
    $ y_0 = h_0 = 1 thin "mm", quad u_0 = 0. $
    Let $y_o, u_o$ be its final height and slope.

    *Effective focal length*
    $ f_("eff") = -h_0/u_o $
    Measured from the *rear principal plane*, the reference plane of the equivalent thin lens.

    *Back focal distance*
    $ b = -y_o/u_o $
    Measured from the *final plane in the input*.
    - $b > 0$: focus to its right; $b < 0$: to its left.
    - A trailing gap changes $b$, not $f_("eff")$.

    #v(3pt)
    *Afocal system:*
    $u_o = 0$: parallel rays emerge parallel; no finite focal length. Use a relative near-zero tolerance in floating-point calculations.
  ],
  [
    #topic[3. Aperture and f-number]
    *Entrance pupil:* image of the limiting opening through the preceding optics, viewed from the object side.

    Opening $i$ has physical diameter $D_i$ and reference-ray height $y_i$. A scale factor $s$ must satisfy $abs(s y_i) <= D_i/2$.

    *Accepted incident beam diameter*
    $ D_("in",i) = D_i h_0/abs(y_i) $
    *Entrance-pupil diameter*
    $ D_("EP") = min_(i: y_i != 0) D_("in",i) $
    - Check all lens openings and explicit stops.
    - The smallest physical hole need not limit the incident beam.
    - At $y_i = 0$, a positive opening passes this axial bundle; it may limit the field of view.

    #v(3pt)
    *Photographic f-number at infinity*
    $ N = f_("eff")/D_("EP") $
    For a converging system with a finite pupil. Use the *entrance-pupil diameter*, not the physical stop diameter.
  ],
)

#v(8pt)
#line(length: 100%, stroke: 0.6pt)
#v(6pt)
#grid(
  columns: (0.8fr, 1.05fr, 1.15fr),
  gutter: 9mm,
  [
    #text(size: 16pt, weight: "bold")[Worked example]
    #v(3pt)
    ```text
    lens 50 40
    gap 25
    stop 10
    ```
  ],
  [
    #table(
      columns: (1.5fr, 1fr, 1fr),
      stroke: none,
      inset: (x: 3pt, y: 2pt),
      align: (left, right, right),
      [*Reference ray*], [$y$ (mm)], [$u$],
      [Input], [1.0], [0],
      [After lens], [1.0], [−0.02],
      [At stop], [0.5], [−0.02],
    )
  ],
  [
    *Focus:* $f_("eff") = 50 thin "mm"$, $b = 25 thin "mm"$.

    *Pupil:* $D_("EP") = min(40, 10/0.5) = 20 thin "mm"$.

    *F-number:* $N = 50/20 = 2.5$ → *f/2.5*.
  ],
)

#pagebreak()
#set text(lang: "cs")
#text(size: 26pt, weight: "bold")[Smol příručka optiky]
#v(2pt)
Centrované tenké čočky ve vzduchu · paraxiální aproximace · osový předmět v nekonečnu · délky v milimetrech
#v(5pt)
#line(length: 100%, stroke: 0.6pt)
#v(7pt)

#grid(
  columns: (1fr, 1fr, 1fr), gutter: 9mm,
  [
    #topic[1. Model paprsku]
    - $z$: souřadnice podél optické osy, kladný směr doprava.
    - $y$: výška paprsku se znaménkem.
    - $u$: směrnice paprsku, změna výšky na 1 mm posunu podél osy. Kladná směrnice znamená růst, záporná pokles; při $u = 0$ je paprsek vodorovný.

    #v(3pt)
    *Vzduchová mezera délky $d$*
    $ y' = y + d u, quad u' = u $

    *Tenká čočka s ohniskovou vzdáleností $f$*
    $ y' = y, quad u' = u - y/f $
    $f > 0$: spojka; $f < 0$: rozptylka.

    *Otvor o průměru $D$:* paprsek projde, pokud $abs(y) <= D/2$. Směr se nemění. Také každá čočka má omezený světlý průměr.

  ],
  [
    #topic[2. Ohnisko soustavy]
    Sledujeme referenční paprsek rovnoběžný s osou:
    $ y_0 = h_0 = 1 thin "mm", quad u_0 = 0. $
    Na výstupu má výšku $y_o$ a směrnici $u_o$.

    *Efektivní ohnisková vzdálenost soustavy*
    $ f_("eff") = -h_0/u_o $
    Měří se od *obrazové hlavní roviny* k obrazovému ohnisku.

    *Vzdálenost ohniska od výstupní roviny*
    $ b = -y_o/u_o $
    Počátek: *poslední rovina zadané posloupnosti*.
    - $b > 0$: ohnisko vpravo; $b < 0$: vlevo.
    - Koncová mezera změní $b$, nikoli $f_("eff")$.
    Je-li výstupní rovina poslední optickou plochou, jde o *obrazovou sečnou (vrcholovou) ohniskovou vzdálenost*.

    #v(3pt)
    *Afokální soustava:* $u_o = 0$. Rovnoběžné paprsky vystupují rovnoběžně; konečná ohnisková vzdálenost neexistuje. Při výpočtu použijte relativní toleranci pro nulovou směrnici.
  ],
  [
    #topic[3. Pupila a clonové číslo]
    *Vstupní pupila:* obraz aperturní clony vytvořený předcházející částí soustavy do předmětového prostoru.

    Otvor $i$ má skutečný průměr $D_i$ a referenční paprsek v něm výšku $y_i$. Pro násobek $s$ referenčního paprsku musí platit $abs(s y_i) <= D_i/2$.

    *Přípustný průměr vstupního svazku*
    $ D_("in",i) = D_i h_0/abs(y_i) $
    *Průměr vstupní pupily*
    $ D_("EP") = min_(i: y_i != 0) D_("in",i) $
    - Zkontrolujte otvory čoček i samostatné clony.
    - Nejmenší skutečný otvor nemusí svazek omezovat nejvíce.
    - Při $y_i = 0$ otvor s kladným průměrem tento osový svazek neomezuje; může omezovat zorné pole.

    #v(3pt)
    *Clonové číslo při zaostření na nekonečno*
    $ N = f_("eff")/D_("EP") $
    Pro spojnou soustavu s konečnou pupilou. Použijte *průměr vstupní pupily*, nikoli skutečný průměr clony.
  ],
)

#v(8pt)
#line(length: 100%, stroke: 0.6pt)
#v(6pt)
#grid(
  columns: (0.8fr, 1.05fr, 1.15fr), gutter: 9mm,
  [
    #text(size: 16pt, weight: "bold")[Řešený příklad]
    #v(3pt)
    ```text
    lens 50 40
    gap 25
    stop 10
    ```
  ],
  [
    #table(
      columns: (1.5fr, 1fr, 1fr), stroke: none,
      inset: (x: 3pt, y: 2pt), align: (left, right, right),
      [*Referenční paprsek*], [$y$ (mm)], [$u$],
      [Vstup], [1,0], [0],
      [Za čočkou], [1,0], [−0,02],
      [V rovině clony], [0,5], [−0,02],
    )
  ],
  [
    *Ohnisko:* $f_("eff") = 50 thin "mm"$, $b = 25 thin "mm"$.

    *Pupila:* $D_("EP") = min(40, 10/(0.5)) = 20 thin "mm"$.

    *Clonové číslo:* $N = 50/20 = 2.5$ → *f/2,5*.
  ],
)

#import "symbols.typ": symbol-reference
#symbol-reference("en", topic)
#symbol-reference("cs", topic)

// Compile with --input student=true to omit this final teacher page.
#if sys.inputs.at("student", default: "false") != "true" [
  #set text(size: 12pt)
  #show math.equation.where(block: true): set text(size: 13.5pt)
  #show math.equation.where(block: true): set block(above: 2.5pt, below: 2.5pt)
  #pagebreak()
  #text(size: 26pt, weight: "bold")[Tessar]
  #v(2pt)
  #v(5pt)
  #line(length: 100%, stroke: 0.6pt)
  #v(5pt)
  #image("tessar-layout.svg", width: 100%)
  #v(2pt)
  #image("tessar-rays.svg", width: 100%)
  #grid(columns: (0.95fr, 1.3fr, 1.35fr), gutter: 9mm,
    [
      #text(size: 14pt, weight: "bold")[`examples/tessar-model.opt`]
      #set par(leading: 0pt)
      #show raw: set text(size: 11pt)
      ```text
      lens 75 30
      gap 10
      lens -50 24
      gap 5
      stop 8
      gap 5
      lens 40 24
      ```
    ],
    [
      *Referenční paprsek:* $h_0 = 1 thin "mm"$, $u_0 = 0$.
      $ y_o = 68/75 thin "mm", quad u_o = -7/375 $
      $ f_("eff") = 375/7 thin "mm" approx 53.571 thin "mm" $
      $ b = 340/7 thin "mm" approx 48.571 thin "mm" $
    ],
    [
      *Clona:* $y_i = 133/150 thin "mm"$, $D_i = 8 thin "mm"$.
      $ D_("EP") = 1200/133 thin "mm" approx 9.023 thin "mm" $
      $ N = 95/16 = 5.9375 quad ("f/5,938") $
    ],
  )
]
