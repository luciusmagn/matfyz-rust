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

= Lisp

#image("obrazek (6).png") // Lisp XKCD

== Lisp
- 1958 - John McCarthy z MITu #pause
- Druhý nejstarší high-level stále-používaný jazyk po FORTRANu

== Lisp
#image("obrazek (17).png", height: 80%)

== Lisp
#image("obrazek (9).png", height: 80%)

== Lisp
- Původně zamýšlen jako matematická notace, ne programovací jazyk #pause
- Steve Russell implementoval první interpreter na základě McCarthyho článku

== Lisp

#image("obrazek (8).png")

== Lisp

#image("obrazek (18).png")

== Motivace
- Fortran byl hlavně výpočetní, procedurální jazyk a McCarthy byl hater #pause
  - Musíme si uvědomit, že současný Fortran vypadá docela odlišně #pause
- Tehdejší AI výzkum potřeboval:
  - Symbolickou manipulaci
  - Dynamické datové struktury (heterogenní listy)
  - Rekurzi (Fortran ji původně neměl) #pause
- Lambda kalkulus jako teoretický základ

== Syntaxe
```lisp
; Vše je seznam
(operator argument1 argument2 ...)

; Definice funkce
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Volání
(factorial 5) ; => 120
```

Mj. *if* statement je taky vynález Lispu 

#image("obrazek (10).png")

== Další "novinky"
- Garbage collection #pause
- REPL (Read Eval Print Loop) #pause
- Stromy (datová struktura) #pause
- Dynamické programování #pause
- Plnohodnotná makra/metaprogramování

U Common Lispu např. podmínky místo výjimek, a u Schemu
continuations

== S-expressions
S-expression = Symbolic Expression

Všechno jsou seznamy nebo atomy
```lisp
; Čísla (atomy)
42
; Symboly (atomy)
factorial
; Seznamy
(1 2 3)
; Vnořené seznamy
(defun foo (x) 
  (+ x 1))
```

Jednotná syntaxe pro data i kód

== Homoikonicita
Kód je reprezentován stejnými datovými strukturami jako data

```lisp
; Data - seznam symbolů a čísel (quoting)
'(+ 1 2 3)

; Kód - volání funkce
(+ 1 2 3)

; Manipulace kódu jako dat
(eval '(+ 1 2 3)) ; => 6
```

= Web

== JavaScript

Brendan Eich pracoval v Netscapu: #pause
- Úkol: "Vytvořit programovací jazyk na skriptování webu" #pause
- Deadline: 10 dní

Hmm, jaký jazyk se dá naimplementovat za 10 dní?

== JavaScript
#image("obrazek (11).png", height: 70%)

== Javascript

Management: "Musí vypadat jako Java btw lol"

Výsledek: Scheme sémantika v C-like syntaxi

#image("obrazek (7).png", height: 80%) // Brendan Eich following me on twitter

== JavaScript

First-class funkce:
```javascript
const add = x => y => x + y;
const add5 = add(5);
console.log(add5(3)); // 8
```
Scheme:
```lisp
(define add
  (lambda (x)
    (lambda (y)
      (+ x y))))
      
(define add5 (add 5))
(display (add5 3)) ; 8
```

== Closures a lexikální scoping
```javascript
function makeCounter() {
  let count = 0;
  return function() {
    return ++count;
  };
}
```

```lisp
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))
```

== Prototypy
Ty jsou ze Selfu, Smalltalku-podobný jazyk
```javascript
// Prototypová dědičnost z Self
const animal = {
  speak() { console.log(this.sound); }
};

const dog = Object.create(animal);
dog.sound = "Woof";
dog.speak(); // "Woof"
```

= CSS
== CSS

DSSSL (Document Style Semantics and Specification Language): #pause
- ISO standard z 1996 #pause
- Schemovitej styling jazyk pro SGML #pause
- Flexibilní, ale považován za příliš komplexní

== DSSSL
```lisp
(root
  (make simple-page-sequence
    font-family-name: "Times"
    font-size: 12pt
    line-spacing: 14pt))

(element chapter
  (make paragraph
    font-weight: 'bold
    font-size: 18pt
    space-before: 12pt
    space-after: 6pt))
```

== CSS
```css
:root {
  font-family: Times;
  font-size: 12pt;
  line-height: 14pt;
}

chapter {
  font-weight: bold;
  font-size: 18pt;
  margin-top: 12pt;
  margin-bottom: 6pt;
}
```

== CSS selektory a kaskáda
DSSSL koncepty v CSS: #pause
- Pattern matching na elementy (selektory) #pause
- Inheritance a kaskádování #pause
- Výpočty hodnot #pause

CSS odstranilo: #pause
- Plnou programovatelnost #pause
- Podmínky a smyčky (ale teď zas máme CSS3 custom properties...)

== HTML

SGML (Standard Generalized Markup Language, 1986): #pause
- Někteří kontributoři (např. Jelliffe) měli zkušenosti s Lispem #pause
- Hierarchická struktura dokumentů #pause
- Uvažovalo se o S-expression syntaxi

== Proč ne S-expressions pro markup?
```lisp
(html
  (head 
    (title "Můj web"))
  (body
    (h1 "Nadpis")
    (p "Text s " (em "důrazem") ".")))
```

Důvody odmítnutí: #pause
- Závorky? :( #pause
- Angle brackets považovány za "přirozenější" pro text #pause
- Vliv existujících značkovacích jazyků (RUNOFF, GML)

== JSON
```javascript
{
  "name": "Lukáš",
  "age": 24,
  "languages": ["Czech", "English", "Rust"]
}
```

```lisp
(object
  (name "Lukáš")
  (age 24)
  (languages (array "Czech" "English" "Rust")))
```

= Co je vlastně Lisp?

Nemůžeme se dohodnout B)

== Minimální požadavky

Některé definice vyžadují pouze: #pause
1. First-class funkce #pause
2. Dynamické typování #pause
3. Garbage collection #pause
4. Mít eval() funkci #pause

Podle této definice: Python, Ruby, JavaScript...

== Tradiční požadavky

Tradičnější pohled: #pause
1. S-expressions #pause
2. Homoikonicita #pause
3. Makra #pause
4. REPL #pause
5. Symbolická manipulace #pause

Např. Common Lisp, Scheme, Clojure splňují

== Maximalistické požadavky

Nejsilnější nároky:
1. Cons cells listy jako primitivní typ
2. Car/cdr operace

Common Lisp a Scheme stále splňují, ale Clojure už ne

== Dylan

```dylan
define method factorial(n :: <integer>)
  if (n = 0)
    1
  else
    n * factorial(n - 1)
  end
end method;
```

Proč je považován za Lisp:
- Původně měl S-expression syntaxi
- Dynamický method dispatching (v podstatě CLOS)
- First-class functions
- Makra (i bez homoikonicity)

== Dylan
#image("obrazek (13).png", height: 75%)

= Lisp v 2025 (jo, ne?)

== Common Lisp: Stabilita standardu

ANSI Common Lisp (X3.226-1994): #pause
- 30 let starý standard #pause
- Stále relevantní #pause
- Žádné breaking changes

== Extensibilita
Common Lisp makra přidávat nativně-vypadající fíčury:

```lisp
(-> respond (&key (:body string) (:headers alist) (:status (integer 100 599)))
            list)
(defun respond (&key
                  (body "")
                  (headers '())
                  (status 200))
  (list status headers
        (if (alexandria:proper-list-p body)
            body
            (list body))))

```
(to platí i o Schemu, kde řada řídících struktur se dá implementovat jako makra)
== Extensibilita
```lisp
(declaim (ftype (function (&key (:body string)
                                (:headers list)
                                (:status (integer 100 599)))
                          list)
                respond))
(defun respond (&key
                  (body "")
                  (headers '())
                  (status 200))
  (list status headers
        (if (alexandria:proper-list-p body)
            body
            (list body))))
```

== CLOS: Common Lisp Object System
```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age  :initarg :age  :accessor person-age)))

(defgeneric interact (thing1 thing2)
  (:documentation "Two things interact"))

(defmethod interact ((p1 person) (p2 person))
  (format t "~A meets ~A" 
          (person-name p1) 
          (person-name p2)))
```

== Coalton: Statické typování v CL
```lisp
(coalton-toplevel
  (define-type (Result :a :b)
    (Ok :a)
    (Err :b))
    
  (declare safe-divide (Integer -> Integer -> (Result Integer String)))
  (define (safe-divide x y)
    (if (== y 0)
        (Err "Division by zero")
        (Ok (/ x y)))))
```

Haskellovitý statický type system jako knihovna. Type-checking za compile-time. V produkci v quantum-computingu

== Praktické využití

Common Lisp se používá:
- Grammarly (gramatická kontrola)
- ITA Software (letecké rezervace), Airbus SW
- NASA (plánovací systémy)
- CAD systémy (AutoCAD má AutoLISP)

Clojure: #pause
- Velké finanční instituce
- Walmart, Amazon
- CircleCI, Puppet

== Praktické využití

- Common Lisp i např. Chez Scheme jsou velmi výkonné #pause
- Ale nesmíne dělat extrémně dynamické věci #pause
- A bacha na linked listy #pause

SBCL je velmi rychlá implementace #pause

- CL je velmi populární např. v Japonsku #pause
- Scheme je moc malý, v praxi se používá max Racket #pause
- Clojure je v pohodě

= Prokletí (?) extensibility

== Každý větší projekt je dialekt

Např. web frameworky
```lisp
; Caveman2
(defroute "/" ()
  (render #P"index.html"))

; Hunchentoot
(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (s)
    (:html (:body "Hello"))))

; Clack
(defun app (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello")))
```

== DSL exploze

V Lispu často vytváříme vlastní jazyky (a Lispeři často vytvářejí další Lispy):
```lisp
; SQL DSL
(select (:id :name)
  (from :users)
  (where (:> :age 18)))

; prolog v Lispu
(defrel parent "Tom" "Bob")
(defrel parent "Tom" "Liz")
(query (parent ?x ?y))
```

== BASIC?????
#image("obrazek (21).png", height: 80%)

== LOOP makro

- DSL na iterování a vše okolo
- Common Lisp nemá tail call optimization, narozdíl od Schemu,
  tak nemůžeme implementovat iteraci přes rekurzi

== LOOOP makro

#image("obrazek (19).png")

== LOOOOP makro

#image("obrazek (20).png")

== Racket (Scheme dialekt)

```racket
#lang racket
; standardní Racket

#lang typed/racket
; staticky typovaný
(: add (-> Number Number Number))
(define (add x y) (+ x y))

#lang datalog
; datalog databáze
edge(a, b). edge(b, c).
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).
```

== Racket - Rhombus

#image("obrazek (12).png", height: 70%)

== Výhody DSLs

- Ergonomický jazyk pro danou doménu #pause
- Žádné kompromisy v syntaxi #pause
- Maximální expresivita #pause
- Snadná kompozice

== Nevýhody DSLs

- Každý projekt má vlastní "jazyk" #pause
- Dokumentace je často slabá #pause
- Debugging napříč DSL vrstvami

== Prokletí Lispu
Důsledky: #pause
- Fragmentace ekosystému #pause
- Musím se naučit "syntaxi" pro daný projekt #pause

Situace ekosystému je lepší od toho, co používáme ASDF a quicklisp

== Závorky

#image("obrazek (14).png", height: 70%)

== Závorky
- Parinfer a smartparens #pause
- Syntax highlighting #pause
  - V emacs i pro vlastní makra #pause
- Formátování #pause

Ale ne každý má náturu na S-expressions :)

== Závorky

#image("obrazek (15).png", height: 80%)

= Otázky?