#let symbol-rows = (
  ($z$, [Axial coordinate], [Souřadnice podél optické osy], [mm]),
  ($y$, [Signed ray height], [Výška paprsku se znaménkem], [mm]),
  ($u$, [Ray slope: height change per 1 mm forward], [Směrnice: změna výšky na 1 mm posunu podél osy], [1]),
  ($y', u'$, [Ray after one operation], [Paprsek po jednom kroku], [mm; 1]),
  ($d$, [Air-gap length], [Délka vzduchové mezery], [mm]),
  ($f$, [Signed lens focal length], [Ohnisková vzdálenost čočky], [mm]),
  ($h_0 = y_0$, [Initial reference height, 1 mm], [Počáteční referenční výška, 1 mm], [mm]),
  ($u_0$, [Initial slope, zero], [Počáteční směrnice, nula], [1]),
  ($y_o, u_o$, [Output height and slope], [Výstupní výška a směrnice], [mm; 1]),
  ($f_("eff")$, [Effective focal length], [Efektivní ohnisková vzdálenost], [mm]),
  ($b$, [Focus distance from final plane], [Vzdálenost ohniska od výstupní roviny], [mm]),
  ($D, D_i$, [Physical opening diameter], [Světlý průměr otvoru], [mm]),
  ($y_i$, [Reference height at opening $i$], [Referenční výška v otvoru $i$], [mm]),
  ($s$, [Reference-ray scale factor], [Násobek referenčního paprsku], [1]),
  ($D_("in",i)$, [Incident beam limit at opening $i$], [Mez vstupního průměru pro otvor $i$], [mm]),
  ($D_("EP")$, [Entrance-pupil diameter], [Průměr vstupní pupily], [mm]),
  ($N$, [F-number], [Clonové číslo], [1]),
  ($i$, [Opening index], [Index otvoru], [1]),
  ($F', H'$, [Rear focus; rear principal point], [Obrazové ohnisko; obrazový hlavní bod], []),
)

#let symbol-reference(language, topic) = {
  let english = language == "en"
  [
    #pagebreak()
    #set text(lang: language, size: 14pt)
    #text(size: 26pt, weight: "bold", if english { [Symbols and terms] } else { [Značky a pojmy] })
    #v(2pt)
    #if english [Lengths in mm.] else [Délky v mm.]
    #v(5pt)
    #line(length: 100%, stroke: 0.6pt)
    #v(7pt)
    #grid(columns: (1.65fr, 1fr), gutter: 9mm,
      [
        #table(
          columns: (24mm, 1fr, 17mm),
          stroke: none, inset: (x: 3pt, y: 1.2pt),
          align: (left, left, left),
          table.header(
            strong(if english { "Symbol" } else { "Značka" }),
            strong(if english { "Meaning" } else { "Význam" }),
            strong(if english { "Unit" } else { "Jednotka" }),
          ),
          table.hline(stroke: 0.5pt),
          ..symbol-rows.map(row => (row.at(0), row.at(if english { 1 } else { 2 }), row.at(3))).flatten(),
        )
      ],
      [
        #if english [
          #topic[Optical terms]
          *Paraxial approximation* \
          Rays make small angles to the optical axis.

          *Thin lens* \
          A zero-thickness model: direction changes, height does not.

          *Rear principal plane* \
          Measure $f_("eff")$ from this plane to $F'$. Its axial intersection is $H'$.

          *Aperture stop* \
          The opening that limits the bundle from an axial object point. It may be a lens rim.

          *Entrance pupil* \
          The image of the aperture stop through the preceding optics, seen from the object side.

          *F-number* \
          The ratio $f_("eff")/D_("EP")$. A smaller value means a larger relative aperture.

          *Afocal system* \
          Parallel input rays emerge parallel; no finite focus.

          #v(7pt)
          #topic[Notation]
          Subscript $o$ means output, $0$ input. The pair $y',u'$ describes the ray after one step.
        ] else [
          #topic[Optické pojmy]
          *Paraxiální aproximace* \
          Paprsky svírají s optickou osou malé úhly.

          *Tenká čočka* \
          Idealizace s nulovou tloušťkou; mění směr, nikoli výšku paprsku.

          *Obrazová hlavní rovina* \
          Od této roviny měříme $f_("eff")$ k $F'$. Její průsečík s osou je $H'$.

          *Aperturní clona* \
          Otvor, který omezuje svazek z osového předmětového bodu. Může jím být i obruba čočky.

          *Vstupní pupila* \
          Obraz aperturní clony vytvořený předcházející optikou do předmětového prostoru.

          *Clonové číslo* \
          Poměr $f_("eff")/D_("EP")$; menší hodnota odpovídá větší relativní apertuře.

          *Afokální soustava* \
          Rovnoběžný vstupní svazek je rovnoběžný i na výstupu; nemá konečné ohnisko.

          #v(7pt)
          #topic[Zápis]
          Dolní $o$ značí výstup, $0$ vstup. Dvojice $y',u'$ popisuje paprsek po jednom kroku.
        ]
        #v(7pt)
        #text(size: 10.5pt)[
          #if english [Terminology:] else [Terminologie:]
          #link("https://optics.upol.cz/userfiles/file/Opticke_pristroje_1.pdf")[J. Ponec: Optické přístroje 1, § 2.2.1];
          #link("https://is.muni.cz/el/1411/jaro2010/BOBO0221p/um/11994899/Zakl_BO_OO_1_Zakl_pojmy.pdf")[Základy brýlové optiky: Základní pojmy].
        ]
      ],
    )
  ]
}
