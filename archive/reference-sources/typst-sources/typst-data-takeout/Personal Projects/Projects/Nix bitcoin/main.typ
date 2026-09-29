#import "@preview/touying:0.5.3": *
#import themes.simple: *
#show: simple-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Wait, it's all Lisp?],
   author: [Lukáš Hozda],
   date: datetime.today(),
   institution: [Braiins Systems s.r.o],
 ),
)
#set page(margin: (x: 0pt, y: 0pt), header: none)
#show raw: set text(size: 0.7em)
#align(center)[
  #image("waititsall.png", height: 89%)
]
#align(center)[
   #image("obrazek (16).png", height: 80%)
]

#title-slide()[
  #align(center)[
  = Wait, it's all Lisp?
  Lukáš Hozda, Braiins Systems s.r.o.
  ]

  27.5.2025
]

== Já
- Zlý korporátní špeh z Braiins #pause
  - Původně Embedded Rust Engineer #pause
  - Teď vedu veřejné i interní vzdělávání #pause
    - Jeden z technicky kompetentnějších HR :PP #pause
- Rust enjoyer od cca března 2015 #pause
- Lisp a Emacs enjoyer (zejména Common Lisp a Scheme) poslední \~3 roky #pause
  - Fanoušek obskurnějších technologií, jazyků (např. Idris) atp.

== Já
- Kontakty:
  - X/Twitter: \@LukasHozda
  - Telegram: \@luciusmagn
  - Discord: \@magnusi
  - Signal: \@magnusi.39

#image("obrazek (3).png", height: 89%) // My recently released Rust book

