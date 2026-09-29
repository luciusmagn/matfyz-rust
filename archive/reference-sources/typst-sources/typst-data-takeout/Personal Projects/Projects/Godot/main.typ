#import "@preview/touying:0.3.2": *

// Themes: default, simple, metropolis, dewdrop, university, aqua
#let s = themes.metropolis.register(aspect-ratio: "16-9")
#let s = (s.methods.info)(
  self: s,
  title: [Knee Grows Tech stack],
  subtitle: [Bottom text],
  author: [Built different],
  date: datetime.today(),
  institution: [Rasismus s.r.o],
)
#let (init, slides, touying-outline, alert) = utils.methods(s)
#show: init

#show strong: alert

#let (slide, title-slide, focus-slide) = utils.slides(s)
#show: slides


= Gitlab

== Gitlab

- Gitlab na repozitáře
- Grupa: https://gitlab.com/flipped-a-whole-brick-into-an-empire/
  - Potřebuju emaily lol
- Repozitář: https://gitlab.com/flipped-a-whole-brick-into-an-empire/godot-template
  - Naklonujem a squashneme/smažeme historii
- Gitlab CI pipeline
  - Runner
  - Skript: https://gitlab.com/flipped-a-whole-brick-into-an-empire/godot-template/-/blob/master/.gitlab-ci.yml?ref_type=heads
  - Chce to doinstalovat cross-rs nebo napsat nix flake a vyrábět buildy i pro Widle

= Template

== godot-template

- `rust/`
  - Rust crate, kompiluje se do .sočka a je přilinkovanej k Godot hře
- `godot/`
  - Základní godot projekt, co importuje Rust
  - Na skriptování můžeme používat jak Rust, tak GDScript
- `./setup.sh`
  - Závislosti
  - Neřeší editor - VSCode je asi nejjednodušší
    - Rozšíření `rust-analyzer`
  - Godot má zabudovaný editor na GDScript
- `rust/Justfile`
  - Ez příkazy na development
  - `just --list`:

```
Available recipes:
    build       # for Windows, Linux/X11, and Mac (if running on Mac)
    build-debug # Build debug artifacts
    check       # Run cargo check, cargo fmt, and cargo clippy
    lint        # Same, but fail on warnings or misformatted files
    release     # Run the build recipe and zip up the results
    watch       # Watch the Rust sources and rebuild in debug mode
```

TODO:
- nix flake nebo `cross-rs`
  - Tj. další závislost bude buď docker nebo nix
- packaging
- Upravit init funkci, aby nekřičela, že nemůže běžet dvakrát

= Godot engine
- Relativně lightweight engine
- Jazyky:
  - Nativně GDScript nebo C++ (hypoteticky existuje ještě special verze Godota s Monem - C\#)
  - GDScript:
    - Prakticky Python
      - Jen drobné rozdíly
    - Můžem používat, zvládne kdokoliv
  - C++:
    - Kill yourself
  - Jde nabindovat řadu dalších jazyků
    - Máme setup na Rust, má docela good bindings
      - https://godot-rust.github.io/book/index.html
    - JS/TS, Lua, Nim, Java, Go, Zig, bůhvíco
  
- Základní koncepty
  - Scéna
    - Strom uzlů
    - Slouží jednak jako scény, jednak jako prefabs
  - Node (uzel)
  - Strom scén
  - Signály
    - Základní způsob komunikace mezi nodes
    - Node vyšle signál, pokud dojde k nějaké události
      - Např. kliknutí na tlačítko
      - Ostatní nodes reagují na signály
      - Observer pattern: https://gameprogrammingpatterns.com/observer.html

= Scéna

== Scéna
#image("obrazek.png")

== Scéna
- Scény jsou re-usable
- Můžu mít scény pro:
  - Menu
  - Hráče
  - Levely
  - Jednotlivé enemáky
- Scény instancují a dají se dynamicky spawnovat/ničit
- Tj. je to hlavně organizační nástroj, tvorba abstrakcí
- Scény jsou taky uzly (z pohledu dalších scén)

== Vnořování scén
#image("obrazek2.png")

= Node / Uzel

== Node / Uzel
- Scény se skládají z 1+ nodes
- V Godotu je prakticky všechno Node
- Node reprezentují třídy
  - Nové Nodes se typicky vytváří děděním
    - `Node2D`
    - `Node3D`
    - `RefCounted` - pro nodes, které pouze ukládají data

#image("obrazek3.png")

= Strom scén

== Strom scén
- Hru v Godotu tvoří strom scén
- Jedna scéna musí být výchozí, v té hra začíná
#image("obrazek4.png", height:80%)

= Signály

== Signály
- Jsou vidět v Node panelu

#image("obrazek5.png", height:80%)

= Godot - instalace

- Z URL: https://godotengine.org/
- Ideálně z balíčkovače Linux distribuce
- Verze:
  - >= 4.0
  - Musí to být standardní verze, ne Mono/.NET
- Nový projekt, export a download šablon na export
  - Projekt>Exportovat>Přidat...>(Linux, Widle, je to jedno)
- Godot umí s Blenderem (umí někdo s Blenderem?)

#image("obrazek6.png")

= Rust - instalace

- VSCode: https://code.visualstudio.com/
  - Ale ideálně z balíčkovače lol
- Rustup: https://rustup.rs/
  - Ideálně ne přes balíčkovač, ale přes Rustup

  Setup:
  ```bash
  rustup component add rust-analyzer
  rustup component add rust-src
  ```

= Testování

- Godot - Spustit editor, nový projekt, Play tlačítko
  - Ideálně, aby nám fungoval Forward+ / Mobile renderer
#image("obrazek7.png")

== Testování
- Rust:

  ```bash
  cargo init rust-test && cd rust-test && cargo run
  ```
  ```rust
  // rust-testsrc/lib.rs
  fn main() {
    println!("Hello, world!");
  }
  ```
- Rust + Godot
  1. Naklonovat godot-template
  2. Upravit rust/src/lib.rs (třeba ten text "Hello, world!")
  3. `cargo build`
  4. Otevřít godot/ projekt v Editoru
  5. V konzoli se objeví:

#image("obrazek8.png")

= Nový Node v Rustu

- Obecný postup:
  1. import Node2D a INode2D (trait pro metody "parent" classy)
  2. Atributy na struct, implementace INode2D
  3. Použití v editoru
  4. Profit?

```rs
use godot::engine::Node2D;
use nanorand::{Rng, WyRand};

/// Node, který křičí do konzole
#[derive(GodotClass)]
#[class(base=Node2D)]
struct Screamer {
    rng: WyRand,
}

```

== INode2D
```rs
#[godot_api]
impl INode2D for Screamer {
    fn init(_base: Base<Node2D>) -> Self;
    fn process(&mut self, delta: f64);
}
```

== init()

```rs
fn init(_base: Base<Node2D>) -> Self {
    Self { rng: WyRand::new() }
}
```

== process()

```rs
fn process(&mut self, delta: f64) {
    if (delta as usize % 200 == 0) && self.rng.generate::<bool>() {
        let mut message = String::new();

        for _ in 0..(self.rng.generate::<u8>() % 32) {
            if self.rng.generate::<bool>() {
                message.push('A');
            } else {
                message.push('a');
            }
        }

        godot_print!("{message}");
    }
}
```

== Výsledek

#image("obrazek9.png")

= Patterny

- Signály - Observer pattern
  - https://gameprogrammingpatterns.com/observer.html
- Kompozice
  - Skládáme nody do scén
  - Např. hráč může mít:
    - AnimatedSprite2D (vzhled)
    - CollisionShape2D (kolize)
    - CharacterBody2D (fyzika+input)
  - Výsledek - Player scéna -> Player node v levelech
- 2D Node typy - https://www.youtube.com/watch?v=22VYNOtrcgM
- Singleton - auto-loaded Nody
  - Zas tak nepotřebujeme, máme srandy jako OnceCell v Rustu pro lazy inicializaci globálních prostředků
- Stavové automaty
  - Menu, chování "bytostí" atp.

= Další kroky

1. Nasetuppovat - nesmíme ztrácet čas setupem na GameJamu
2. Udělat průzkum free assetů
  - Nesmíme zapomenout, že kromě grafických assetů řešíme i zvuky
    - `sfxr`
    - https://modarchive.org/ - ale bacha na správnou licenci
    - Další OSS zdroje zvuků
  - Grafika:
    - Free assety na itch.io a další zdroje
    - AI Lol
3. Godot videa
  - 2D node typy: https://www.youtube.com/watch?v=22VYNOtrcgM
  - Bonus: všechny nody (zajímaj nás 3D?):
    - https://www.youtube.com/watch?v=tO2gthp45MA
  - Komprehensivní tutoriál: https://www.youtube.com/watch?v=S8lMTwSRoRg
4. Jméno týmu