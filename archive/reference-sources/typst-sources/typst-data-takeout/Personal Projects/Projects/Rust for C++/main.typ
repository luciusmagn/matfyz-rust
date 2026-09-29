#import "@preview/touying:0.5.3": *
#import themes.simple: *
#show: simple-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Rust pro C++kaře],
   author: [Lukáš Hozda],
   date: datetime.today(),
   institution: [Braiins Systems s.r.o],
 ),
)
#set page(margin: (x: 0pt, y: 0pt), header: none)
#show raw: set text(size: 0.7em)
#align(center)[
  #image("obrazek (1).png", height: 89%)
]

#title-slide()[
  #align(center)[
  = Rust pro C++kaře
  Lukáš Hozda, Braiins Systems s.r.o.
  ]

  30.4.2025
]

== Já
- Zlý korporátní špeh z Braiins
  - Původně Embedded Rust Engineer
  - Teď vedu veřejné i interní vzdělávání
    - Možná technicky nejkompetentnější HR na světě :p
- Učím Rust na Matfyzu poslední 3 semestry
  - Katedra spolehlivých a distribuovaných systémů
- Rust enjoyer od cca března 2015
- Mimo to:
  - Lisp a Emacs enjoyer (zejména Common Lisp a Scheme)
  - Fanoušek obskurních technologií, jazyků atp.

== Já
- Kontakty:
  - X/Twitter: \@LukasHozda
  - Telegram: \@luciusmagn
  - Discord: \@magnusi
  - Signal: \@magnusi.39
- Taky píšu knížky:
  - Building Bitcoin in Rust (vyšlo)
  - Lambdas and Logos: On Writing Elegant Code (WIP)
  - Embedded Rust intro kniha (WIP)

#image("obrazek (3).png")

= C++

== C++
- Rust 🤝 C++
- C with Classes?
  - Obousečný meč
- Složitý jazyk se složitou historií
  - Zdědil historické problémy, např.:
    - Headery (vs modulový systém)
    - Null pointery a nedefinované chování
    - OOP ze Simuly
    - Velký a složitý standard
    - Chybějící konsensus a konvence

== C++
#image("obrazek (4).png", height: 80%)
== C++

- Programátoři se neúspěšně (a občas trochu slepě) snaží C++ zabít posledních několik dekád
- C++ je složitý nástroj na složitou práci, těžko nahraditelný
- Jazyky, co narušují jeho teritorium:
  - D, Carbon, Rust, Swift
  - Vale, Zig, Go, V
  - Nim, Jai, Odin, CLASP
  - Hare, Lobster, Vala, Ada (95 a dál)
  - Cyclone, BitC, ATS, Chapel, i Java a spol.

== C++

- Proč se pořád někdo snaží C++ zabít?
  - Zkušenosti a výzkum
  - Cena vývoje a maintenance
  - Prokletí zpětné kompatibility
  - Osobní názory
- Proč to nejde?
  - Neflexibilita a overhead
  - Setrvačnost v průmyslu (a C++ se taky posouvá vpřed)
  - Marketing
  - Vstupní cena

== C++

- Zabije někdo C++?
  - Asi ne, ale můžeme ho nahradit v některých případech, když najdeme jazyk, který se pro daný usecase víc vyplatí
- Rust je jeden z těch jazyků

= Rust

== Rust
- Vznik 2006, původně pet project Graydona Hoarea (nesouvisí s C.A.R. Hoarem)
- Pak se do toho vložila Mozilla
- Etymologie:
  - Rust fungus nebo RobUST
- Hodnoty a cíle
  - Nevymýšlet nic moc nového, snažit se integrovat research prakticky
  - Explicitní robustní kód (mj. memory safety)
  - Pragmatický přístup
  - Performance a "fearless" concurrency (type systém nám říká co je a není ok)

== Rust

- Raná historie Rustu (2006 až cca. 2014) je protkaná drastickými změnami a experimentací
  - Rust měl classy, garbage collector, green thready (aka goroutines), třípísmená klíčová slova, compiler pluginy a další výstřelky 
- Rust není primárně inspirován C++kem, ale OCamlem (!!)
  - První verze překladače byla v OCamlu
- Změna vize Rustu je důvodem pro historickou rivalitu s Go
  - Původně Rust byl užitím mnohem blíž Go, ale pak divergoval směrem systémového programování a low-levelu

== Rust

```ocaml
type expr =
  | Int of int
  | Add of expr * expr
  | Mul of expr * expr

let rec eval = function
  | Int n -> n
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b

let () =
  let expr = Add(Mul(Int 2, Int 3), Int 4) in
  Printf.printf "(OCaml) Result: %d\n" (eval expr)  (* => 10 *)
```

== Rust

```rust
enum Expr {
    Int(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

fn eval(expr: &Expr) -> i32 {
    match expr {
        Expr::Int(n) => *n,
        Expr::Add(a, b) => eval(a) + eval(b),
        Expr::Mul(a, b) => eval(a) * eval(b),
    }
}

```

== Rust

```rust
fn main() {
    let expr = Expr::Add(
        Box::new(Expr::Mul(
            Box::new(Expr::Int(2)),
            Box::new(Expr::Int(3))
        )),
        Box::new(Expr::Int(4))
    );
    println!("(Rust) Result: {}", eval(&expr));  // => 10
}
```

== Rust

- Co tu vidíme a co se liší?
  - Pattern matching
  - Makra - *println!()*
  - Enumy aka sum typy
  - Závorky lol
  - Box
- Je chyba k Rustu přistupovat jako k C++
  - Preferujeme jiné přístupy a patterny
  - Důraz na funkcionální fíčury, RAII
- Do Rustu se nedá moc skákat po hlavě

== Další inspirace Rustu

- Cyclone - memory safety fíčury
- Haskell - typeclassy (traity) a trait-bounded polymorfismus
- Scheme/Lisp - procedurální a deklarativní makra
- C++ - smart pointery, reference, atomika, model paměti, RAII
- C\# - atributy
- Newsqeak/Alef/Limbo - kanály

== Kde Rust září

- Projekty, kde potřebujeme výkon anebo multithreading
- Velké projekty, kde je těžké udržet všechno pohromadě
- Projekty, kde najdeme příležitost pro code reuse
- Domény:
  - Systémové programování
  - Embedded
  - Low-latency networking
  - Cloud infrastruktura
  - Backend, Web-Assembly (v Rustu se _dá_ psát i frontend)
  - Integrace s C

== Kde Rust moc nezáří

- Automotive a space industry (WIP - složitá certifikace)
- AI/ML
- Game Development
  - Vyžaduje vysvětlení: gud na enginy, negud na prototypování
- Data Science
- Mobilní aplikace
- Low-stakes aplikace a aplikace vyžadující rapidní vývoj
- Integrace s C++
- Masivní distribuované programování (BEAM jazyky)

= Showcase

==
```rust
fn main() {
  println!("Hello, FIT++!");
}
```

==
```rust
fn main() {
  let owo = "FIT++";
  println!("Hello, {}", owo);
}
```

==
```rust
fn main() {
  let owo = "FIT++";
  println!("Hello, {}");
}
```

==
#image("obrazek (2).png", width: 100%, height:80%)

== Co má Rust s C++ společného
- Reference
  - const T& -> &T
  - T& -> &mut T
  - V Rustu ale mají reference přilepený lifetime (často implicitně)
- Smart pointery
  - unique_ptr -> Box
  - shared_ptr -> Arc/Rc
- Atomika
  - Rust nemá *consume* ordering, ale ten bude deprecated od C++26

== Smart pointery
```rust
struct Node {
    value: i32,
    next: Option<Box<Node>>
}
```

== Smart pointery

```rust
fn create_linked_list() -> Box<Node> {
    let node3 = Box::new(Node { value: 3, next: None });
    let node2 = Box::new(Node { value: 2, next: Some(node3) });
    let node1 = Box::new(Node { value: 1, next: Some(node2) });
    
    node1 // return head
}

fn main() {
    let list = create_linked_list();
    println!("Head value: {}", list.value);
    
    if let Some(ref next) = list.next {
        println!("Second value: {}", next.value);
    }
}
```

== Smart pointery
```cpp
#include <iostream>
#include <memory>

struct Node {
    int value;
    std::shared_ptr<Node> next;

    Node(int v, std::shared_ptr<Node> n = nullptr) 
        : value(v), next(std::move(n)) {}
};
```


== Smart pointery
```cpp
std::shared_ptr<Node> create_linked_list() {
    auto node3 = std::make_shared<Node>(3);
    auto node2 = std::make_shared<Node>(2, node3);
    auto node1 = std::make_shared<Node>(1, node2);
    
    return node1;
}

int main() {
    auto list = create_linked_list();
    std::cout << "Head value: " << list->value << std::endl;
    
    if (list->next) {
        std::cout << "Second value: " << list->next->value << std::endl;
    }
}
```

== Atomika
```rust
fn main() {
    let flag = Arc::new(AtomicBool::new(false));
    let flag_clone = Arc::clone(&flag);
  
    let handle = thread::spawn(move || {
        println!("Worker: Setting flag...");
        flag_clone.store(true, Ordering::SeqCst);
    });

    while !flag.load(Ordering::SeqCst) {
        thread::yield_now();
    }
    
    println!("Main: Flag was set!");
    handle.join().unwrap();
}
```

== Atomika
```cpp
int main() {
    std::atomic<bool> flag(false);

    std::thread worker([&flag]() {
        std::cout << "Worker: Setting flag..." << std::endl;
        flag.store(true, std::memory_order_seq_cst);
    });

    while (!flag.load(std::memory_order_seq_cst)) {
        std::this_thread::yield();
    }
    
    std::cout << "Main: Flag was set!" << std::endl;
    worker.join();
}
```

== Hlavní rozdíly

- Move sémantika by default, ownership/borrowing/lifetimy
- Safe rust nemá žádné nedefinované chování
- Rust nedělá implicitní konverze typů (mimo auto-deref &&T -> &T)
- Inference typů pro lokální proměnné by default
- Fokus na nějaké FP fíčury (immutabilita, iterátory, ADTs - variant typy)
- Fokus na generiku přes trait bounds (žádný duck typing)
- Kompoziční OOP místo dědění
- Úzká integrace se testováním, dokumentací a package managerem
  - Rust preferuje statické linkování a kompiluje závislosti ze zdroje

== Hlavní rozdíly
- Žádné headery - modulový systém jako hlavní způsob organizace kódu
  - A teď už máme moduly v C++ taky :))
- Žádný nullptr v safe Rustu (-> Option\<T\> pro chybějící hodnoty)
- Žádné exceptions v Rustu (-> Result\<T, E\> pro věci co mohou selhat)
  - Výsledkem je:
    1. Z předpisu funkce víme docela dost o jejím chování
    2. Jsme nuceni explicitně handlovat případy, kdy hodnota chybí nebo operace selže (protože je nějak z těch kontejnerů musíme dostat, žejo)
- Rust má taky paniky pro irrecoverable chyby
- Rust nemá třídy, má struktury i implementační bloky k nim asociované
  obsahující metody a konstanty
- Obecně: bezpečné a explicitní chování je default a opt-out, ne opt-in

== Ownership a borrowing

- Klíčový rys Rustu pro paměťovou bezpečnost
- Základní pravidla:
  - Každá hodnota má právě jednoho vlastníka
  - Když vlastník zanikne, hodnota je dealokována
  - Vlastnictví lze předat (move sémantika)
- Borrowing:
  - Buď 0..1 mutable ref odkaz NEBO 0..N immutable
    - Tj. žádný mutable aliasing
  - Žádná reference nesmí přežít svého referenta
- Předchází data races, dangling pointerům a use-after-free chybám
- Kompilační chyby místo runtime chyb

```rust
fn main() {
  let r;
  {
    let s: String = "hello".to_string();
  }
}
```

== Lifetime
- Compile-time abstrakce pro tracking referencí
- Určuje, jak dlouho reference zůstane validní
- Většinou implicitní (lifetime elision)
- Explicitní lifetime anotace:
```rust
// 'a je lifetime
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```
Lifetimy jsou nástroj, kterým dáváme překladači informaci,
co chtěl básník říci

== Ownership vs C++ RAII

- RAII (Resource Acquisition Is Initialization) je v Rustu preferovaný
- Dobře koresponduje s ownership modelem

```cpp 
// C++ - manuální správa paměti
std::string* create_message() {
    auto* message = new std::string("Hello, World!");
    return message;  // de-alokace problém callera
}

void use_message() {
    auto message = create_message();
    std::cout << *message << std::endl;
    delete message;  // <--
}
```

== Ownership vs C++ RAII

```rust
// Rust - ownership system
fn create_message() -> String {
    String::from("Hello, World!")
}

fn use_message() {
    let message = create_message();
    println!("{}", message);
    // Drop na konci scopu
}
```

== Traity, templaty, generika
- Traity cca odpovídají templates
  - Trait == sada metod/funkcí, asociovaných typů a konstant
  - V podstatě typeclassy z Haskellu
- Rozdíly oproti templates:
  - Fungují v opačném směru:
    - Typ explicitně implementuje trait
    - Template se snažíme napasovat na typ
  - Výběr mezi statickým a dynamickým dispatchem


== Traity, templaty, generika
```rust
trait Display {
    fn display(&self) -> String;
}

impl Display for i32 {
    fn display(&self) -> String {
        format!("Číslo: {}", self)
    }
}
```
== Traity, templaty, generika
```rust
// Generická funkce s trait bound (static dispatch)
fn show<T: Display>(value: T) {
    println!("{}", value.display());
}

// Generická funkce s trait objektem (dynamic dispatch)
fn show2(value: &dyn Display) {
    println!("{}", value.display());
}
```

== Kompozice vs Dědičnost

- Rust nemá dědičnost ani třídy
- Místo "is-a" preferuje "has-a" vztahy
- Code reuse skrz traity a kompozici trait bounds
- Příklad:

```cpp
// C++ s dědičností
class Animal {
public:
    virtual void make_sound() = 0;
};

class Dog : public Animal {
    void make_sound() override { std::cout << "Haf!"; }
};
```

== Kompozice vs Dědičnost

```rust
trait MakeSound {
    fn make_sound(&self);
}

struct Dog;
impl MakeSound for Dog {
    fn make_sound(&self) { println!("Haf!"); }
}
```

- To má svoje výhody a nevýhody, některé problémy se lépe modelují s
  dědičností


== Kompozice vs Dědičnost
```rust
fn consume_meal_and_let_them_now<A, F(animal: A, food: F)
where
  F: Food,
  A: MakeSound + Eat<F>
{
  animal.eat(food);
  animal.make_sound();
}
```
v boundech může být skoro cokoliv, labužníci to občas přehánějí :)

== Option
- Jako *optional* v C++, ale v Rustu je naprosto klíčový
- Taky odpovídá typu Maybe v řadě jazyků
```rust
fn find_user(id: u64) -> Option<String> {
    if id == 0 {
        None
    } else {
        Some(format!("User_{}", id))
    }
}
```

== Option
```rust
fn main() {
    let user_id = 0;
    
    // option je kontejner, takže to z něj musíme dostat
    let username = match find_user(user_id) {
        Some(name) => name,
        None => "Guest".to_string(),
    };
    // nebo např.
    let username2 = find_user(user_id).unwrap_or_else(|| "Guest".to_string());
    
    // tohle zpanikaří
    let will_crash = find_user(0).unwrap();
    println!("User: {}", username);
}
```

== Option
```cpp
std::optional<std::string> find_user(uint64_t id) {
    if (id == 0) {
        return std::nullopt;
    } else {
        return "User_" + std::to_string(id);
    }
}
```

== Option
```cpp
int main() {
    uint64_t user_id = 0;
    std::string username;
    // fuck it, we ball (potenciální runtime error)
    auto result = find_user(user_id);
    if (result) {
        username = *result; // nebo result.value()
    } else {
        username = "Guest";
    }
    auto username2 = find_user(user_id).value_or("Guest");    
    // hodí exception:
    auto will_throw = find_user(0).value();

    std::cout << "User: " << username << std::endl;
}
```
== Result
- Hlavní nástroj pro error handling, podobný typu Either
```rust
enum FileError {
    NotFound,
    PermissionDenied,
} // měl by implementovat Error trait pro backtrace apod.

fn read_file(path: &str) -> Result<String, FileError> {
    if path == "secret.txt" {
        Err(FileError::PermissionDenied)
    } else if path == "missing.txt" {
        Err(FileError::NotFound)
    } else {
        Ok(format!("Contents of {}", path))
    }
}

fn process_file() -> Result<(), FileError> {
    // ? operátor propaguje errory nahoru
    let contents = read_file("config.txt")?;
    println!("Read: {}", contents);
    
    // s tím resultem se musíme vypořádat
    let result = read_file("secret.txt");
    match result {
        Ok(data) => println!("{}", data),
        Err(FileError::NotFound) => println!("File not found"),
        Err(FileError::PermissionDenied) => println!("Access denied"),
    }
    
    Ok(())
}
```

== Ekosystém a pragmatismus
- Cargo - unified package manager a build systém
  - vs. Make, CMake, Meson, pkg-config...
- crates.io - centrální registr balíčků
  - Distribuce zdrojáku závislostí
  - Sémantické verzování a resolution verzí
- Dokumentace a testy integrovány do jazyka
- Bezpečnost jako default - větší důvěra vůči závislostem
- Jednoduchá cross kompilace

== Makra
- Inspirovaná Lispem, konkrétně Schemem
- Deklarativní makra
  - Pattern matching na vstupním zdrojáku
  - Hygienická
- Procedurální makra
  - Program (externí knihovna přilinkovaná k překladači), co dostane na vstup stream tokenů a jeho výstupem je taky stream tokenů

== Makra
#image("obrazek (5).png", height: 80%)

== Unsafe Rust

- Kompilátor nemůže garantovat bezpečnost, ale to neznamená, že vypne safety fíčury
  - "Trust me, bro"
- Co unsafe umožňuje:
  - Dereferencovat raw pointery
  - Volat unsafe/FFI funkce
  - Přistupovat k mutable globálním proměnným, co nepoužívají Mutex atp.
  - Implementovat unsafe traits
- Cíl: izolovat unsafe kód do malých částí, co se dají dobře auditovat

```rust
pub struct RawBuffer {
    ptr: *mut u8,
    len: usize,
}

impl RawBuffer {
    pub fn new(size: usize) -> Self {
        let layout = std::alloc::Layout::array::<u8>(size).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) };
        RawBuffer { ptr, len: size }
    }
}
```

== Proč je Rust robustní?

- Dostatečně silný typový systém, který zakódovává validitu referencí, thread safety a jestli něco může selhat/chybět
- Explicitní a konzistentní, žádná magie nebo pasti na nováčky
- Ve srovnání s C++ zatím stále docela malý jazyk co do počtu fíčur
- Dobrý interop s C, WIP s C++
  - Super na jádra knihoven pro Python, Ruby, Javu atp...
- Velmi silné asynchronní programování
  - Které je zároveň kompletně agnostické vůči async runtimu
  - Dá se i naroubovat na interrupty a modelovat embedded bare metal aplikace v async konceptech

== Rust práce

- Market share nám roste
- Do Rustu investují velké firmy (tady v Praze má Rust oddělení např. Microsoft)
- Po Rust programátorech je docela hlad
  - Vyplatí si je vycvičit, protože kandidátů není tolik
  - Lokace: čím víc na západ od nás, tim víc, hotspot v Kalifornii
- Populární pro crypto shitcoin projekty
  - Ty ale rozhodně nejsou většina trhu ->
    - Velké projekty, operační systémy, tooling
    - Firmware, embedded, projekty citlivé na výkon/latenci
== Kde začít
- Uuuh, přečíst si mojí BTC/Rust knihu (ebook je zdarma)
- The Rust Programming Language
- Rustlings
- Programming Rust
- Zapsat si Rust na MatFyzu, nebo mě nainstalovat na FIT
- Rust má trochu strmější learning curve, protože nás nutí dělat věci pořádně hned napoprvé, ale všechno se to vstřebá
= Otázky?