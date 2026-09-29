#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => self.info.institution,
  config-info(
    title: [Obrázky z historie programování],
    subtitle: [],
    author: [Lukáš Hozda],
    date: datetime.today(),
    logo: emoji.pumpkin,
    institution: [Braiins Systems s.r.o],
  ),
)


//#set heading(numbering: numbly("{1}.", default: "1.1"))

#title-slide()

== Lukáš Hozda
- SWE Metodolog v Braiins #pause
  - TL našeho junior týmu / hatchery #pause
  - Vzdělávání všeho druhu a onboarding #pause
  - Zaměření na Rust a Nix #pause
  - Dříve Embedded Rust Dev #pause
  - Shitposting #pause
- V některých systémech je překlep "metolog", což ze mě dělá odborníka na pervitin :) #pause
- Vyučující Rustu na MatFyzu #pause

signal: *\@magnusi.39* telegram: *\@luciusmagn* twitter: *\@LukasHozda*

== Lukáš Hozda

- Linux a Emacs auťák #pause
  - Artix s S6 initem, balíčky z Nixu, Void Linux a DragonflyBSD subsystémy
  - Niri scrolling window manager
  - Přihlašuji se do tty a mám v systému celkem 4 GUI programy #pause
- Téměř 10 let Rustu, a překvapivě nejsem femboy #pause
- Na sockách (Xwitter) předstírám, že jsem Superman a postuju cringe (přidejte si mě) #pause
- Fanoušek obskurních a historických technologií

= Historie

== Historie

- Programování je relativně filozofická disciplína #pause
- Konflikt různých pohledů #pause
- Technické limitace

==

#image("obrazek (1).png", height: 80%)
ENIAC

==

#image("obrazek (2).png", height: 90%)
První obecně uznávaný program - Výpočet Bernoulliho čísel

==

#image("obrazek (3).png", height: 90%)

Ada Lovelace

==

#image("obrazek (4).png", height: 90%)

Ada Lovelace - její anime profile picture

==
- V 20. století jsme ženám sebrali slova "programátor" a "počítač" a pak to tbh šlo z kopce #pause
- Počítače původně bylo potřeba fyzicky zapojit jinak pro každý účel/program #pause
  - Operační systémy? Multi-tasking? lol #pause
- To trvá dlouho -> potřeba programovacích jazyků a externího načítání programů

==

#image("obrazek (5).png", height: 80%)

Harvard Computers, někdy kolem první světový. Programovací jazyk: Angličtina

Cena: 25-50 centů na hodinu :(

== Programovaci jazyky

- Účel - popis programu #pause
- Kompilace/Interpretace programu vyžaduje výpočetní prostředky #pause
  - Ze začátku jsme nemohli implementovat high-level přátelské jazyky #pause
- Rozlišujeme pět generací programovacích jazyků (1GL-5GL) #pause
  - (zatím) #pause
  - Myslíte si, že moderní jazyky jako Rust a Nim jsou poslední generace? #pause
  - :) #pause
- Od 3. generace dál je v tom trochu bordel, zejména kvůli programátorům a tvůrcům jazyků

== 1GL

- Strojové jazyky / machine code #pause
- Zadávej nuly a jedničky, brácho #pause

#image("obrazek (6).png", height: 50%)

8080 program, co nasype 5 + 3 do registru A

==

#image("obrazek (7).png")

==

#image("obrazek (8).png")

==

- Jazyky 1. generace byly poměrne rychle překonány druhou generací #pause
- Důvody
  - Portabilita? #pause
  - Bolest to debuggovat a číst #pause
  - Bolest to zadávat do počítače #pause
  - Obecne extrémně nepraktické #pause
- Nicméně je to rychlé, a extrémně levné z pohledu počítače #pause
- Žádná kompilace neni potřeba, jenom to tam nasázet #pause
- Je otázka, jestli se tu vůbec da hovořit o kompilaci

== 2GL

- Assembly #pause
- Portabilita stále problém  #pause
  - V moderním světě máme relativně málo architektur  #pause
  - Drtivou většinu trhu pokryje podpora x86, ARM, RISC-V  #pause
  - Historicky si každý vymyslel svoji architekturu  #pause
  - Řada architektur je poměrně na odchodu  #pause
    - Berkeley RISC -> SPARC vs MIPS  #pause
      - obrazek slzíciho koťátka  #pause
    - Itanium a iAPX   #pause
      - pokusy intelu se konečně zbavit x86  #pause
- Konflikty mezi designéry architektur a 3GL jazyků  #pause
  - High-level instrukce nejsou používány překladači  #pause
  - Napr. string instrukce v x86 

==

#image("obrazek (9).png", width: 100%)

RISC-V suma čísel od 0 do 5 (resp. od 5 do nuly)

==

#image("obrazek (10).png", height: 90%)

x86 string instrukce - kopírování stringu z `source` do `dest`

== 3GL

- Chléb náš vezdejší dej nám dnes #pause
- 3GL je to, co si většina z nás představí pod "programovací jazyk" #pause
- Relativně portable (spektrum), lepší debuggovani #pause
- Vyhodnocování interpretem nebo kompilace do strojového kódu, popř. VM byte kódu #pause
- Třetí generace vedla k revoluci v programování #pause
  - Mnohem rychlejší vývoj #pause
  - Portabilita kodu #pause
  - Programování mnohem přístupnější #pause
  - Drasticky větší volnost v přístupech #pause
- Vedla k explozi rady programovacích jazyků, paradigmat a duševních chorob #pause
- Různé úrovně abstrakce (schovaváme detaily hardware)

== Programovací jazyky znovu (v kontextu 3GL a dal)

- V IT máme spoustu jazyků #pause
- Některé mají dokonce slovo "language" v názvu #pause
  - ... Ale nejsou programovací jazyky #pause
- Např. #pause
  - HTML #pause
  - TOML #pause
  - CSS #pause
  - Markdown #pause
- Dle mojí oblíbené terminologie *počítačové jazyky* #pause
  - Mají specifický účel - popis struktury, konfigurace, stylu... #pause
  - Ale ne vyjadřování algoritmu #pause
  - Filozofická otázka: Je Makefile programovací jazyk?

==

- Programovací jazyky jsou jazyky, kde hlavním účelem je popis algoritmů #pause
- Většina jazyků, se kterými se setkáte, je Turing-complete #pause
  - Dokáží simulovat Turingův stroj a tudíž provést libovolný algoritmus #pause
- Ne všechny programovací jazyky jsou Turing-complete #pause
  - Samozřejmě esoterické #pause
  - A např. Dhall

== Dhall

- Úplný (totální) jazyk #pause
- Každý program dokazatelně skončí #pause
- Účel - programy jejichž výstupem je konfigurace

==
#image("obrazek (11).png", height: 70%)

==

- Nicméně, většina jazyků spadá do dvou hlavnich myšlenkových proudů #pause
  - Imperativni #pause
  - Deklarativni #pause
- Definice ze školy #pause
  - Imperativni jazyky popisují kroky k dosažení cíle #pause
  - Deklarativní přístup popisuje cíl samotný #pause
- Ehh, vysvětlíme si to lépe 

== Imperativní programování 

- Popisuje výpočty pomocí příkazů, které mění stav programu  #pause
- Definuje logiku výpočtu pomocí sekvence instrukcí k vykonání ve specifickém pořadí #pause
- Mohli bychom v tom vidět von Neumannovu architekturu, což činí imperativní přístupy #pause
  bližší dnešním počítačům a grug-brained programátorům #pause
  - Tím neříkám, že dnešní počítače jsou von Neumann, schovejte vidle

==

#image("obrazek (12).png", height: 70%)

== Co je von Neumannova architektura (připomenutí)

- Návrhový model počítače #pause
  - CPU #pause
  - ALU #pause
  - Paměť #pause
  - Externí úložiště (popř.) #pause
  - I/O #pause
- Hlavní charakteristiky #pause
  - Sdílená paměť pro instrukce a data (Oproti Harvard architektuře) #pause
  - Sekvenční vyhodnocování instrukcí #pause
- Moderní počítače - modifikovaná Harvard architektura #pause
  - Zvenku von Neumann (RAM paměť), v procesoru Harvard (registry, mezipaměti)

== Hlavní charakteristiky

- Explicitní sekvence příkazů k provedení #pause
- Přímá manipulace stavu programu #pause
- Použití řídících struktur - smyčky, podmínky #pause 
  - GOTO Considered Harmful - Edsger "gigachad" Dijkstra, původní grug-brained developer #pause
- Čas si zase ukázat nějaký kód 

==

#image("obrazek (15).png", height: 80%)

goes hard
== BASIC

#image("obrazek (16).png", height: 80%)

Koukněte na tu krásu číslování řádku 

== Další citát 

#image("obrazek (17).png")

== A ještě jeden

#image("obrazek (18).png")

==

#image("obrazek (13).png", height: 70%, width: 100%)

Algol 60, pradědeček moderních programovacích jazyků. Starší než vaší rodiče 
- Algol vypadá relativně moderně a přátelsky, ne? Oproti třeba C

==

#image("obrazek (14).png", height: 80%)

Identický C program. C byl designový zázrak kvůli svoje efektivitě 
a flexibilitě ve velmi omezeném prostředí.

== Pro srovnání Python

#image("obrazek (19).png", height: 80%)

Jednodušší pro lidi, fajn na prototypování, ale dál od HW - degradována efektivita a výkonnost 

== Raná paradigmata

- Mnoho z prvnich jazyků měli něco jako GOTO na skoky mezi částmi programu #pause
- Posléze jsme přešli na procedurální a strukturované programování #pause
  - Struktury zde jsou řídící struktury, ne datové struktury #pause
    - Bloky kódu #pause
    - Podmínky, smyčky #pause
  - Procedury/podprogramy (procedurální) #pause
- Procedurální a strukturované jdou ruku v ruce, ale nemusí to tak být vždy #pause 
  - Jde napsat nestrukturovaný procedurální kód -- GOTO + procedury #pause
  - I strukturovaný neprocedurálni kód -- jeden blok kódu + control flow #pause
- Pak jsme začali stavit další věci... #pause
- Další gigachad: Sir Anthony Hoare AKA Tony Hoare AKA C. A. R. Hoare

==

#image("obrazek (20).png", height: 90%)
Tony Hoare taky měl prsty v Algolu

==

#image("obrazek (21).png")

== Fortran

#image("obrazek (22).png")

== A zije do dnes

#image("obrazek (23).png", height: 90%)

A GCC vam ten fortran zkompiluje :)

== Objektové a Objektově Orientované Programování 

- Cílem objektového programování je zavést abstrakci - objekt -
  a definovat programy ve smyslu interakcí mezi objekty #pause
- Simula byla první OOP jazyk #pause
  - Původně domenově specifický jazyk na programování simulací (Simula I) #pause
  - Poté obecný jazyk (Simula II) #pause
  - Opět v tom má prsty Hoare #pause
- Objekty spravují svuj vnitřní stav a baví se prostřednictvím zpráv #pause 
  - Buď vysloveně message passing nebo volání metod #pause
  - Objekty by neměly šahat na vnitřnosti jiných objektu #pause
  - Fujtajbl klíčové slovo `friend` v C++ #pause
- Smalltalk, jazyk bohem osvícený 

== Smalltalk

- Čisté objektové programovani #pause
- Vsechno je objekt #pause
- Vsechno se děje skrz message passing #pause
- Řídící struktury jsou taky objekty #pause
- Živý systém - modifikace za běhu #pause
- Bohem osvícený, mimozemský jazyk #pause
- Popularizace konceptu IDE #pause
  - Nepoužívám, Emacs mi vezmete jedině z mrtvých, studených rukou

==

#image("obrazek (24).png", height: 90%)
(GNU Smalltalk) - Moderní Smalltalk existuje jako Pharo

==

- OOP může byt velmi flexibilní a příjemné na použití, ale docela heavyweight #pause
- Tři přístupy #pause
  - Třídové/dědičné OOP #pause
  - Prototypové OOP #pause
  - Kompoziční OOP #pause
- Eventuálně jsme sešli ze stezky pravdy, vynalezli Javu a svět je od té doby temné a kruté místo #pause 
- V posledních letech se pomalu léčíme ze zlozvyku cpát OOP naprosto všude

= Deklarativni programování 
- We are so back

== Deklarativni programování 

- Přístup, kde specifikujeme, čeho má program dosáhnout, spíš než jak toho dosáhnout #pause 
- Navzdory nedávné popularitě se nejedná o novinku #pause
  - Bohatá historie sahající do 30. let 20. století #pause
  - Alonzo "taky chad" Church a jeho lambda kalkulus

==

#image("obrazek (25).png")

== 

- V posledních dekádách jsme se akorát zlepšili v implementování a zpřístupňování deklarativních přístupů smrtelníkům #pause
- Dříve spíše záležitost pohádkového světa akademické půdy #pause
- Hlavní cíle #pause
  - Abstrakce Řídícího Toku (control flow):
    - Deklarativní programy popisují požadovaný výsledek bez explicitního výčtu kroků #pause
  - Neměnnost
    - Důraz na immutable datové struktury, které nelze po vytvoření měnit #pause
  - Referenční transparentnost
    - Funkce konzistentně vrací stejný výstup pro daný vstup, bez vedlejších účinků #pause

== Co jsou to ty vedlejší efekty, ktere děsí nás nerdy?

- Vedlejší účinky = změny mimo scope funkce #pause
- Příklady: globální proměnné, I/O, síť #pause
- Problémy: #pause
  - horší debugging #pause
  - race conditions #pause
  - snížená code re-usability #pause
  - blokuje optimalizace #pause
  - znemožňuje memoizaci #pause
  - komplikuje analýzu kódu

== 
- Deklarativní jazyky obvykle omezují vedlejší účinky #pause
  - podporují čisté funkce #pause
  - stejný vstup = stejný výstup #pause
  - bez modifikace vnějšího stavu #pause

- Imperativní jazyky jsou benevolentnější #pause
  - programátor má volnou ruku #pause
  - může je omezit dobrovolně #pause

- Vedlejší účinky nejsou vždy špatné #pause
  - někdy zjednodušují řešení #pause
  - záleží na kontextu #pause
  - neexistuje univerzální pravidlo #pause

- S absencí vedlejších účinků lze využít Church-Rosserův teorém

== Church-Rosserův teorém
- Church-Rosserův teorém (1936) #pause
  - Dokázán gigachady Alonzo Churchem a J. Barkley Rosserem #pause
  - Pořadí vyhodnocení lambda výrazů neovlivní výsledek #pause
  - Různé cesty redukce vedou ke stejnému výsledku #pause

- Důsledky pro deklarativní programování: #pause
  - Umožňuje líné vyhodnocování #pause
  - Podporuje paralelní zpracování #pause
  - Teoretický základ pro správnost

==
- Optimalizační výhody: #pause
  - Kompilátor může zvolit nejvhodnější pořadí #pause
  - Lepší využití CPU #pause
  - Překvapivě dobrý výkon #pause

- Princip redukce: #pause
  - programy můžeme považovat za matematický výraz #pause
  - vyhodnocení je redukce na konečný výsledek #pause
  - podobné řešení rovnic v matematice 

== SQL je deklarativni

#image("obrazek (26).png", width: 100%)

SQL není Turing-complete (bez rozšíření)

== It's FPing time

- Funkcionální programování #pause
  - významná podmnožina deklarativního programování #pause
  - dlouhá historie mnoha implementací #pause
  - dodnes aktivně používaná paradigma #pause

- LISP (LISt Processing) #pause
  - jeden z prvních vysokoúrovňových jazyků #pause
  - zavedl klíčové koncepty funkcionálního programování #pause
  - vytvořen gigachadem Johnem McCarthym #pause
  - McCarthy také: #pause
    - průkopník umělé inteligence #pause
    - autor samotného termínu "AI" #pause
  - denní chléb nás Emacs bláznů #pause
  - pravděpodobně výsledek 90% "make your own language" tutoriálů 

== McCarthy

#image("obrazek (27).png", height: 90%)

Jonh McCarthy, Rizzler roku 1970

== Lisp new

#image("obrazek (28).png")

== Lisp old

#image("obrazek (29).png")

== Lisp machine

#image("obrazek (30).png", height: 80%)

Lisp byl dost populární na to, aby vzniknul vlastní typ počítače zvané lisp machines

== Haskell time

- Haskell #pause
  - pojmenován po logikovi Haskellu Currym #pause
    - další gigachad, známý díky "curryfikaci" #pause
  - čistě funkcionální jazyk #pause
- Čistá funkcionalita #pause
  - žádné imperativní prvky #pause
  - funkce musí být bez vedlejších účinků #pause
  - IO řešeno speciálním způsobem (to je na delší povídání :)) #pause
- Curryfikace #pause
  - převod n-ární funkce na řetěz unárních (arita = počet parametrů) #pause
  - např: `add(x,y) → add(x)(y)` #pause
  - umožňuje částečnou aplikaci funkcí
  - `f(x,y,z) → ((f(x))(y))(z)` #pause

==

#image("obrazek (31).png", width: 100%)

Haskell

==

- Funkcionální přístup ke stavu: #pause
  - nemutujeme = žádné změny na místě #pause
  - místo toho mapujeme stavy #pause
  - stav₁ → funkce → stav₂ #pause
- Princip: #pause
  - původní stav zůstává nezměněn #pause
  - nový stav je výsledkem transformace #pause
  - každá změna = nová hodnota

== SML

- Můj oblíbený klasický FP jazyk #pause
- Standard ML je staticky typovaný funkcionální jazyk #pause
- Využívá Hindley-Milner typovou inferenci #pause
- Významně ovlivnil design programovacích jazyků #pause
- Zavedl funkce běžné v moderních jazycích #pause
- Soucasti bohate rodiny ML jazyku

==

#image("obrazek (32).png")

==

#image("obrazek (33).png")

== Deklarativni vs Imperativni znovu

- Imperativní přístup je jako kuchařka #pause
- Popisujete jak se dostat k cíli: #pause
"Pro výpočet Fibonacciho čísla vezmi a=0 a b=1, sečti je, přiřaď staré b do a, přiřaď součet do b, a vracej staré a, tolikrát, kolik Fibonacciho čísel potřebuješ vygenerovat."

- Deklarativní přístup říká, co je cílem: #pause
"Fibonacciho číslo je součet dvou předchozích Fibonacciho čísel. První Fibonacciho číslo je 0, druhé je 1."
#pause
- Druhý popis je přívětivější pro mathbros (imo)

== Mene zname imperativni pristupy - Stack based programming

- Stack-based programming manipuluje daty pomocí zásobníku (LIFO) #pause
- Forth (1960s) je hlavním představitelem tohoto paradigmatu #pause
- Programy jsou složeny ze "slov" (ekvivalent funkcí) #pause
- Používá postfixovou (RPN) notaci místo klasické infixové #pause
- Je extrémně minimalistický (\<10kb interpretery) #pause
- Ovlivnil návrh virtuálních strojů (JVM, CLR) #pause
- Hodí se pro embedded systémy a DSL #pause
- Klade důraz na jednoduchost a přímý přístup k hardware #pause
- Umožňuje rozšiřitelnost definicí nových slov

==

#image("obrazek (35).png")

==

- Logické programování je deklarativní přístup (4. generace) #pause
- Popisuje problém pomocí faktů, pravidel a dotazů #pause
- Prolog (1970s) je hlavním představitelem #pause
- Program neříká "jak", ale definuje vztahy mezi daty #pause
- Využívá backtracking pro hledání řešení #pause
- Hodí se na: #pause
  - zpracování přirozeného jazyka #pause
  - expertní systémy #pause
  - symbolickou AI #pause
  - hledání cest v grafech #pause
- Mercury (1990s) kombinuje logické a funkcionální programování #pause
- Výkon není prioritou, důležitější je vyjadřovací síla

==

#image("obrazek (37).png", height: 80%)

Prolog

== A 5GL?
4GL:
- Programátor specifikuje CO chce #pause
- Stále musí definovat určitou logiku #pause
- Příklady: SQL, Prolog, MATLAB

5GL:
- Programátor specifikuje JEN výsledek #pause
- Systém sám řeší JAK ho dosáhnout #pause
- Příklady: Constraint solvery, vizuální modelovací nástroje

==

#image("obrazek (36).png", height: 90%)
OPS5 - Official Production System

== Kdysi

#image("obrazek (38).png", height: 90%)

WITCH - Wolverhampton Instrument for Teaching Computing from Harwell (1951)

== Nyni

#image("obrazek (39).png", height: 90%)

Aurora - \$500mil.
