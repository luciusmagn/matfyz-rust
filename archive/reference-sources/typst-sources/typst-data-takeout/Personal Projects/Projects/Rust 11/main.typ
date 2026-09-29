#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Rust meetup 2025-03-07],
   author: [Lukáš Hozda],
   date: datetime.today(),
   logo: emoji.gear,
   institution: [Braiins Systems s.r.o],
 ),
)

#title-slide()

= Rust Meetup \#11

== Rust Meetup

#pause
- Speaker:
 - Pavel Šimerda: *Data Accessed in Parallel* #pause
- Next event
  - March 20th at Charles University