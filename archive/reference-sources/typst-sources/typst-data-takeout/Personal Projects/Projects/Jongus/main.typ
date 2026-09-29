#align(center)[
  #text(size: 1.5em)[On Lisp]
]

#align(center)[
  To my family, and to Jackie.
]

#align(center)[
  λ
]

#heading(level: 1)[Preface]

This book is intended for anyone who wants to become a better Lisp programmer. It assumes some familiarity with Lisp, but not necessarily extensive programming experience. The first few chapters contain a fair amount of review. I hope that these sections will be interesting to more experienced Lisp programmers as well, because they present familiar subjects in a new light.

It's difficult to convey the essence of a programming language in one sentence, but John Foderaro has come close:

#quote[
  Lisp is a programmable programming language.
]

There is more to Lisp than this, but the ability to bend Lisp to one's will is a large part of what distinguishes a Lisp expert from a novice. As well as writing their programs down toward the language, experienced Lisp programmers build the language up toward their programs. This book teaches how to program in the bottom-up style for which Lisp is inherently well-suited.

#heading(level: 2)[Bottom-up Design]

Bottom-up design is becoming more important as software grows in complexity. Programs today may have to meet specifications which are extremely complex, or even open-ended. Under such circumstances, the traditional top-down method sometimes breaks down. In its place there has evolved a style of programming quite different from what is currently taught in most computer science courses: a bottom-up style in which a program is written as a series of layers, each one acting as a sort of programming language for the one above. X Windows and TEX are examples of programs written in this style.

The theme of this book is twofold: that Lisp is a natural language for programs written in the bottom-up style, and that the bottom-up style is a natural way to write Lisp programs. On Lisp will thus be of interest to two classes of readers. For people interested in writing extensible programs, this book will show what you can do if you have the right language. For Lisp programmers, this book offers a practical explanation of how to use Lisp to its best advantage.

The title is intended to stress the importance of bottom-up programming in Lisp. Instead of just writing your program in Lisp, you can write your own language on Lisp, and write your program in that.

It is possible to write programs bottom-up in any language, but Lisp is the most natural vehicle for this style of programming. In Lisp, bottom-up design is not a special technique reserved for unusually large or difficult programs. Any substantial program will be written partly in this style. Lisp was meant from the start to be an extensible language. The language itself is mostly a collection of Lisp functions, no different from the ones you define yourself. What's more, Lisp functions can be expressed as lists, which are Lisp data structures. This means you can write Lisp functions which generate Lisp code.

A good Lisp programmer must know how to take advantage of this possibility. The usual way to do so is by defining a kind of operator called a macro. Mastering macros is one of the most important steps in moving from writing correct Lisp programs to writing beautiful ones. Introductory Lisp books have room for no more than a quick overview of macros: an explanation of what macros are, together with a few examples which hint at the strange and wonderful things you can do with them. Those strange and wonderful things will receive special attention here. One of the aims of this book is to collect in one place all that people have till now had to learn from experience about macros.

Understandably, introductory Lisp books do not emphasize the differences between Lisp and other languages. They have to get their message across to students who have, for the most part, been schooled to think of programs in Pascal terms. It would only confuse matters to explain that, while defun looks like a procedure definition, it is actually a program-writing program that generates code which builds a functional object and indexes it under the symbol given as the first argument.

One of the purposes of this book is to explain what makes Lisp different from other languages. When I began, I knew that, all other things being equal, I would much rather write programs in Lisp than in C or Pascal or Fortran. I knew also that this was not merely a question of taste. But I realized that if I was actually going to claim that Lisp was in some ways a better language, I had better be prepared to explain why.

When someone asked Louis Armstrong what jazz was, he replied "If you have to ask what jazz is, you'll never know." But he did answer the question in a way: he showed people what jazz was. That's one way to explain the power of Lisp—to demonstrate techniques that would be difficult or impossible in other languages. Most books on programming—even books on Lisp programming—deal with the kinds of programs you could write in any language. On Lisp deals mostly with the kinds of programs you could only write in Lisp. Extensibility, bottom-up programming, interactive development, source code transformation, embedded languages—this is where Lisp shows to advantage.

In principle, of course, any Turing-equivalent programming language can do the same things as any other. But that kind of power is not what programming languages are about. In principle, anything you can do with a programming language you can do with a Turing machine; in practice, programming a Turing machine is not worth the trouble.

So when I say that this book is about how to do things that are impossible in other languages, I don't mean "impossible" in the mathematical sense, but in the sense that matters for programming languages. That is, if you had to write some of the programs in this book in C, you might as well do it by writing a Lisp compiler in C first. Embedding Prolog in C, for example—can you imagine the amount of work that would take? Chapter 24 shows how to do it in 180 lines of Lisp.

I hoped to do more than simply demonstrate the power of Lisp, though. I also wanted to explain why Lisp is different. This turns out to be a subtle question—too subtle to be answered with phrases like "symbolic computation." What I have learned so far, I have tried to explain as clearly as I can.

#heading(level: 2)[Plan of the Book]

Since functions are the foundation of Lisp programs, the book begins with several chapters on functions. Chapter 2 explains what Lisp functions are and the possibilities they offer. Chapter 3 then discusses the advantages of functional programming, the dominant style in Lisp programs. Chapter 4 shows how to use functions to extend Lisp. Then Chapter 5 suggests the new kinds of abstractions we can define with functions that return other functions. Finally, Chapter 6 shows how to use functions in place of traditional data structures.

The remainder of the book deals more with macros than functions. Macros receive more attention partly because there is more to say about them, and partly because they have not till now been adequately described in print. Chapters 7–10 form a complete tutorial on macro technique. By the end of it you will know most of what an experienced Lisp programmer knows about macros: how they work; how to define, test, and debug them; when to use macros and when not; the major types of macros; how to write programs which generate macro expansions; how macro style differs from Lisp style in general; and how to detect and cure each of the unique problems that afflict macros.

Following this tutorial, Chapters 11–18 show some of the powerful abstractions you can build with macros. Chapter 11 shows how to write the classic macros—those which create context, or implement loops or conditionals. Chapter 12 explains the role of macros in operations on generalized variables. Chapter 13 shows how macros can make programs run faster by shifting computation to compile-time. Chapter 14 introduces anaphoric macros, which allow you to use pronouns in your programs. Chapter 15 shows how macros provide a more convenient interface to the function-builders defined in Chapter 5. Chapter 16 shows how to use macro-defining macros to make Lisp write your programs for you. Chapter 17 discusses read-macros, and Chapter 18, macros for destructuring.

With Chapter 19 begins the fourth part of the book, devoted to embedded languages. Chapter 19 introduces the subject by showing the same program, a program to answer queries on a database, implemented first by an interpreter and then as a true embedded language. Chapter 20 shows how to introduce into Common Lisp programs the notion of a continuation, an object representing the remainder of a computation. Continuations are a very powerful tool, and can be used to implement both multiple processes and nondeterministic choice. Embedding these control structures in Lisp is discussed in Chapters 21 and 22, respectively. Nondeterminism, which allows you to write programs as if they had foresight, sounds like an abstraction of unusual power. Chapters 23 and 24 present two embedded languages which show that nondeterminism lives up to its promise: a complete ATN parser and an embedded Prolog which combined total about 200 lines of code.

The fact that these programs are short means nothing in itself. If you resorted to writing incomprehensible code, there's no telling what you could do in 200 lines. The point is, these programs are not short because they depend on programming tricks, but because they're written using Lisp the way it's meant to be used. The point of Chapters 23 and 24 is not how to implement ATNs in one page of code or Prolog in two, but to show that these programs, when given their most natural Lisp implementation, simply are that short. The embedded languages in the latter chapters provide a proof by example of the twin points with which I began: that Lisp is a natural language for bottom-up design, and that bottom-up design is a natural way to use Lisp.

The book concludes with a discussion of object-oriented programming, and particularly CLOS, the Common Lisp Object System. By saving this topic till last, we see more clearly the way in which object-oriented programming is an extension of ideas already present in Lisp. It is one of the many abstractions that can be built on Lisp.

A chapter's worth of notes begins on page 387. The notes contain references, additional or alternative code, or descriptions of aspects of Lisp not directly related to the point at hand. Notes are indicated by a small circle in the outside margin, like this. There is also an Appendix (page 381) on packages.

Just as a tour of New York could be a tour of most of the world's cultures, a study of Lisp as the programmable programming language draws in most of Lisp technique. Most of the techniques described here are generally known in the Lisp community, but many have not till now been written down anywhere. And some issues, such as the proper role of macros or the nature of variable capture, are only vaguely understood even by many experienced Lisp programmers.

#heading(level: 2)[Examples]

Lisp is a family of languages. Since Common Lisp promises to remain a widely used dialect, most of the examples in this book are in Common Lisp. The language was originally defined in 1984 by the publication of Guy Steele's Common Lisp: the Language (CLTL1). This definition was superseded in 1990 by the publication of the second edition (CLTL2), which will in turn yield place to the forthcoming ANSI standard.

This book contains hundreds of examples, ranging from single expressions to a working Prolog implementation. The code in this book has, wherever possible, been written to work in any version of Common Lisp. Those few examples which need features not found in CLTL1 implementations are explicitly identified in the text. Later chapters contain some examples in Scheme. These too are clearly identified.

The code is available by anonymous FTP from endor.harvard.edu, where it's in the directory pub/onlisp. Questions and comments can be sent to onlisp\@das.harvard.edu.

#heading(level: 2)[Acknowledgements]

While writing this book I have been particularly thankful for the help of Robert Morris. I went to him constantly for advice and was always glad I did. Several of the examples in this book are derived from code he originally wrote, including the version of for on page 127, the version of aand on page 191, match on page 239, the breadth-first true-choose on page 304, and the Prolog interpreter in Section 24.2. In fact, the whole book reflects (sometimes, indeed, transcribes) conversations I've had with Robert during the past seven years. (Thanks, rtm!)

I would also like to give special thanks to David Moon, who read large parts of the manuscript with great care, and gave me very useful comments. Chapter 12 was completely rewritten at his suggestion, and the example of variable capture on page 119 is one that he provided.

I was fortunate to have David Touretzky and Skona Brittain as the technical reviewers for the book. Several sections were added or rewritten at their suggestion. The alternative true nondeterministic choice operator on page 397 is based on a suggestion by David Toureztky.

Several other people consented to read all or part of the manuscript, including Tom Cheatham, Richard Draves (who also rewrote alambda and propmacro back in 1985), John Foderaro, David Hendler, George Luger, Robert Muller, Mark Nitzberg, and Guy Steele.

I'm grateful to Professor Cheatham, and Harvard generally, for providing the facilities used to write this book. Thanks also to the staff at Aiken Lab, including Tony Hartman, Janusz Juda, Harry Bochner, and Joanne Klys.

The people at Prentice Hall did a great job. I feel fortunate to have worked with Alan Apt, a good editor and a good guy. Thanks also to Mona Pompili, Shirley Michaels, and Shirley McGuire for their organization and good humor. The incomparable Gino Lee of the Bow and Arrow Press, Cambridge, did the cover. The tree on the cover alludes specifically to the point made on page 27.

This book was typeset using LATEX, a language written by Leslie Lamport atop Donald Knuth's TEX, with additional macros by L. A. Carr, Van Jacobson, and Guy Steele. The diagrams were done with Idraw, by John Vlissides and Scott Stanton. The whole was previewed with Ghostview, by Tim Theisen, which is built on Ghostscript, by L. Peter Deutsch. Gary Bisbee of Chiron Inc. produced the camera-ready copy.

I owe thanks to many others, including Paul Becker, Phil Chapnick, Alice Hartley, Glenn Holloway, Meichun Hsu, Krzysztof Lenk, Arman Maghbouleh, Howard Mullings, Nancy Parmet, Robert Penny, Gary Sabot, Patrick Slaney, Steve Strassman, Dave Watkins, the Weickers, and Bill Woods.

Most of all, I'd like to thank my parents, for their example and encouragement; and Jackie, who taught me what I might have learned if I had listened to them.

I hope reading this book will be fun. Of all the languages I know, I like Lisp the best, simply because it's the most beautiful. This book is about Lisp at its lispiest. I had fun writing it, and I hope that comes through in the text.

Paul Graham


#heading(level: 1)[The Extensible Language]

Not long ago, if you asked what Lisp was for, many people would have answered "for artificial intelligence." In fact, the association between Lisp and AI is just an accident of history. Lisp was invented by John McCarthy, who also invented the term "artificial intelligence." His students and colleagues wrote their programs in Lisp, and so it began to be spoken of as an AI language. This line was taken up and repeated so often during the brief AI boom in the 1980s that it became almost an institution.

Fortunately, word has begun to spread that AI is not what Lisp is all about. Recent advances in hardware and software have made Lisp commercially viable: it is now used in Gnu Emacs, the best Unix text-editor; Autocad, the industry standard desktop CAD program; and Interleaf, a leading high-end publishing program. The way Lisp is used in these programs has nothing whatever to do with AI.

If Lisp is not the language of AI, what is it? Instead of judging Lisp by the company it keeps, let's look at the language itself. What can you do in Lisp that you can't do in other languages? One of the most distinctive qualities of Lisp is the way it can be tailored to suit the program being written in it. Lisp itself is a Lisp program, and Lisp programs can be expressed as lists, which are Lisp data structures. Together, these two principles mean that any user can add operators to Lisp which are indistinguishable from the ones that come built-in.

#heading(level: 2)[Design by Evolution]

Because Lisp gives you the freedom to define your own operators, you can mold it into just the language you need. If you're writing a text-editor, you can turn Lisp into a language for writing text-editors. If you're writing a CAD program, you can turn Lisp into a language for writing CAD programs. And if you're not sure yet what kind of program you're writing, it's a safe bet to write it in Lisp. Whatever kind of program yours turns out to be, Lisp will, during the writing of it, have evolved into a language for writing that kind of program.

If you're not sure yet what kind of program you're writing? To some ears that sentence has an odd ring to it. It is in jarring contrast with a certain model of doing things wherein you (1) carefully plan what you're going to do, and then (2) do it. According to this model, if Lisp encourages you to start writing your program before you've decided how it should work, it merely encourages sloppy thinking.

Well, it just ain't so. The plan-and-implement method may have been a good way of building dams or launching invasions, but experience has not shown it to be as good a way of writing programs. Why? Perhaps it's because computers are so exacting. Perhaps there is more variation between programs than there is between dams or invasions. Or perhaps the old methods don't work because old concepts of redundancy have no analogue in software development: if a dam contains 30% too much concrete, that's a margin for error, but if a program does 30% too much work, that is an error.

It may be difficult to say why the old method fails, but that it does fail, anyone can see. When is software delivered on time? Experienced programmers know that no matter how carefully you plan a program, when you write it the plans will turn out to be imperfect in some way. Sometimes the plans will be hopelessly wrong. Yet few of the victims of the plan-and-implement method question its basic soundness. Instead they blame human failings: if only the plans had been made with more foresight, all this trouble could have been avoided. Since even the very best programmers run into problems when they turn to implementation, perhaps it's too much to hope that people will ever have that much foresight. Perhaps the plan-and-implement method could be replaced with another approach which better suits our limitations.

We can approach programming in a different way, if we have the right tools. Why do we plan before implementing? The big danger in plunging right into a project is the possibility that we will paint ourselves into a corner. If we had a more flexible language, could this worry be lessened? We do, and it is. The flexibility of Lisp has spawned a whole new style of programming. In Lisp, you can do much of your planning as you write the program.

Why wait for hindsight? As Montaigne found, nothing clarifies your ideas like trying to write them down. Once you're freed from the worry that you'll paint yourself into a corner, you can take full advantage of this possibility. The ability to plan programs as you write them has two momentous consequences: programs take less time to write, because when you plan and write at the same time, you have a real program to focus your attention; and they turn out better, because the final design is always a product of evolution. So long as you maintain a certain discipline while searching for your program's destiny—so long as you always rewrite mistaken parts as soon as it becomes clear that they're mistaken—the final product will be a program more elegant than if you had spent weeks planning it beforehand.

Lisp's versatility makes this kind of programming a practical alternative. Indeed, the greatest danger of Lisp is that it may spoil you. Once you've used Lisp for a while, you may become so sensitive to the fit between language and application that you won't be able to go back to another language without always feeling that it doesn't give you quite the flexibility you need.

#heading(level: 2)[Programming Bottom-Up]

It's a long-standing principle of programming style that the functional elements of a program should not be too large. If some component of a program grows beyond the stage where it's readily comprehensible, it becomes a mass of complexity which conceals errors as easily as a big city conceals fugitives. Such software will be hard to read, hard to test, and hard to debug.

In accordance with this principle, a large program must be divided into pieces, and the larger the program, the more it must be divided. How do you divide a program? The traditional approach is called top-down design: you say "the purpose of the program is to do these seven things, so I divide it into seven major subroutines. The first subroutine has to do these four things, so it in turn will have four of its own subroutines," and so on. This process continues until the whole program has the right level of granularity—each part large enough to do something substantial, but small enough to be understood as a single unit.

Experienced Lisp programmers divide up their programs differently. As well as top-down design, they follow a principle which could be called bottom-up design—changing the language to suit the problem. In Lisp, you don't just write your program down toward the language, you also build the language up toward your program. As you're writing a program you may think "I wish Lisp had such-and-such an operator." So you go and write it. Afterward you realize that using the new operator would simplify the design of another part of the program, and so on. Language and program evolve together. Like the border between two warring states, the boundary between language and program is drawn and redrawn, until eventually it comes to rest along the mountains and rivers, the natural frontiers of your problem. In the end your program will look as if the language had been designed for it. And when language and program fit one another well, you end up with code which is clear, small, and efficient.

It's worth emphasizing that bottom-up design doesn't mean just writing the same program in a different order. When you work bottom-up, you usually end up with a different program. Instead of a single, monolithic program, you will get a larger language with more abstract operators, and a smaller program written in it. Instead of a lintel, you'll get an arch.

In typical code, once you abstract out the parts which are merely bookkeeping, what's left is much shorter; the higher you build up the language, the less distance you will have to travel from the top down to it. This brings several advantages:

1. By making the language do more of the work, bottom-up design yields programs which are smaller and more agile. A shorter program doesn't have to be divided into so many components, and fewer components means programs which are easier to read or modify. Fewer components also means fewer connections between components, and thus less chance for errors there. As industrial designers strive to reduce the number of moving parts in a machine, experienced Lisp programmers use bottom-up design to reduce the size and complexity of their programs.

2. Bottom-up design promotes code re-use. When you write two or more programs, many of the utilities you wrote for the first program will also be useful in the succeeding ones. Once you've acquired a large substrate of utilities, writing a new program can take only a fraction of the effort it would require if you had to start with raw Lisp.

3. Bottom-up design makes programs easier to read. An instance of this type of abstraction asks the reader to understand a general-purpose operator; an instance of functional abstraction asks the reader to understand a special-purpose subroutine.

4. Because it causes you always to be on the lookout for patterns in your code, working bottom-up helps to clarify your ideas about the design of your program. If two distant components of a program are similar in form, you'll be led to notice the similarity and perhaps to redesign the program in a simpler way.

Bottom-up design is possible to a certain degree in languages other than Lisp. Whenever you see library functions, bottom-up design is happening. However, Lisp gives you much broader powers in this department, and augmenting the language plays a proportionately larger role in Lisp style—so much so that Lisp is not just a different language, but a whole different way of programming.

It's true that this style of development is better suited to programs which can be written by small groups. However, at the same time, it extends the limits of what can be done by a small group. In The Mythical Man-Month, Frederick Brooks proposed that the productivity of a group of programmers does not grow linearly with its size. As the size of the group increases, the productivity of individual programmers goes down. The experience of Lisp programming suggests a more cheerful way to phrase this law: as the size of the group decreases, the productivity of individual programmers goes up. A small group wins, relatively speaking, simply because it's smaller. When a small group also takes advantage of the techniques that Lisp makes possible, it can win outright.

#heading(level: 2)[Extensible Software]

The Lisp style of programming is one that has grown in importance as software has grown in complexity. Sophisticated users now demand so much from software that we can't possibly anticipate all their needs. They themselves can't anticipate all their needs. But if we can't give them software which does everything they want right out of the box, we can give them software which is extensible. We transform our software from a mere program into a programming language, and advanced users can build upon it the extra features that they need.

Bottom-up design leads naturally to extensible programs. The simplest bottom-up programs consist of two layers: language and program. Complex programs may be written as a series of layers, each one acting as a programming language for the one above. If this philosophy is carried all the way up to the topmost layer, that layer becomes a programming language for the user. Such a program, where extensibility permeates every level, is likely to make a much better programming language than a system which was written as a traditional black box, and then made extensible as an afterthought.

X Windows and TEX are early examples of programs based on this principle. In the 1980s better hardware made possible a new generation of programs which had Lisp as their extension language. The first was Gnu Emacs, the popular Unix text-editor. Later came Autocad, the first large-scale commercial product to provide Lisp as an extension language. In 1991 Interleaf released a new version of its software that not only had Lisp as an extension language, but was largely implemented in Lisp.

Lisp is an especially good language for writing extensible programs because it is itself an extensible program. If you write your Lisp programs so as to pass this extensibility on to the user, you effectively get an extension language for free. And the difference between extending a Lisp program in Lisp, and doing the same thing in a traditional language, is like the difference between meeting someone in person and conversing by letters. In a program which is made extensible simply by providing access to outside programs, the best we can hope for is two black boxes communicating with one another through some predefined channel. In Lisp, extensions can have direct access to the entire underlying program. This is not to say that you have to give users access to every part of your program—just that you now have a choice about whether to give them access or not.

When this degree of access is combined with an interactive environment, you have extensibility at its best. Any program that you might use as a foundation for extensions of your own is likely to be fairly big—too big, probably, for you to have a complete mental picture of it. What happens when you're unsure of something? If the original program is written in Lisp, you can probe it interactively: you can inspect its data structures; you can call its functions; you may even be able to look at the original source code. This kind of feedback allows you to program with a high degree of confidence—to write more ambitious extensions, and to write them faster. An interactive environment always makes programming easier, but it is nowhere more valuable than when one is writing extensions.

An extensible program is a double-edged sword, but recent experience has shown that users prefer a double-edged sword to a blunt one. Extensible programs seem to prevail, whatever their inherent dangers.

#heading(level: 2)[Extending Lisp]

There are two ways to add new operators to Lisp: functions and macros. In Lisp, functions you define have the same status as the built-in ones. If you want a new variant of mapcar, you can define one yourself and use it just as you would use mapcar. For example, if you want a list of the values returned by some function when it is applied to all the integers from 1 to 10, you could create a new list and pass it to mapcar:

```lisp
(mapcar fn
  (do* ((x 1 (1+ x))
        (result (list x) (push x result)))
       ((= x 10) (nreverse result))))
```

but this approach is both ugly and inefficient. Instead you could define a new mapping function map1-n (see page 54), and then call it as follows:

```lisp
(map1-n fn 10)
```

Defining functions is comparatively straightforward. Macros provide a more general, but less well-understood, means of defining new operators. Macros are programs that write programs. This statement has far-reaching implications, and exploring them is one of the main purposes of this book.

The thoughtful use of macros leads to programs which are marvels of clarity and elegance. These gems are not to be had for nothing. Eventually macros will seem the most natural thing in the world, but they can be hard to understand at first. Partly this is because they are more general than functions, so there is more to keep in mind when writing them. But the main reason macros are hard to understand is that they're foreign. No other language has anything like Lisp macros. Thus learning about macros may entail unlearning preconceptions inadvertently picked up from other languages. Foremost among these is the notion of a program as something afflicted by rigor mortis. Why should data structures be fluid and changeable, but programs not? In Lisp, programs are data, but the implications of this fact take a while to sink in.

If it takes some time to get used to macros, it is well worth the effort. Even in such mundane uses as iteration, macros can make programs significantly smaller and cleaner. Suppose a program must iterate over some body of code for x from a to b. The built-in Lisp do is meant for more general cases. For simple iteration it does not yield the most readable code:

```lisp
(do ((x a (+ 1 x)))
    ((> x b))
  (print x))
```

Instead, suppose we could just say:

```lisp
(for (x a b)
  (print x))
```

Macros make this possible. With six lines of code (see page 154) we can add for to the language, just as if it had been there from the start. And as later chapters will show, writing for is only the beginning of what you can do with macros.

You're not limited to extending Lisp one function or macro at a time. If you need to, you can build a whole language on top of Lisp, and write your programs in that. Lisp is an excellent language for writing compilers and interpreters, but it offers another way of defining a new language which is often more elegant and certainly much less work: to define the new language as a modification of Lisp. Then the parts of Lisp which can appear unchanged in the new language (e.g. arithmetic or I/O) can be used as is, and you only have to implement the parts which are different (e.g. control structure). A language implemented in this way is called an embedded language.

Embedded languages are a natural outgrowth of bottom-up programming. Common Lisp includes several already. The most famous of them, CLOS, is discussed in the last chapter. But you can define embedded languages of your own, too. You can have the language which suits your program, even if it ends up looking quite different from Lisp.

#heading(level: 2)[Why Lisp (or When)]

These new possibilities do not stem from a single magic ingredient. In this respect, Lisp is like an arch. Which of the wedge-shaped stones (voussoirs) is the one that holds up the arch? The question itself is mistaken; they all do. Like an arch, Lisp is a collection of interlocking features. We can list some of these features—dynamic storage allocation and garbage collection, runtime typing, functions as objects, a built-in parser which generates lists, a compiler which accepts programs expressed as lists, an interactive environment, and so on—but the power of Lisp cannot be traced to any single one of them. It is the combination which makes Lisp programming what it is.

Over the past twenty years, the way people program has changed. Many of these changes—interactive environments, dynamic linking, even object-oriented programming—have been piecemeal attempts to give other languages some of the flexibility of Lisp. The metaphor of the arch suggests how well they have succeeded.

It is widely known that Lisp and Fortran are the two oldest languages still in use. What is perhaps more significant is that they represent opposite poles in the philosophy of language design. Fortran was invented as a step up from assembly language. Lisp was invented as a language for expressing algorithms. Such different intentions yielded vastly different languages. Fortran makes life easy for the compiler writer; Lisp makes life easy for the programmer. Most programming languages since have fallen somewhere between the two poles. Fortran and Lisp have themselves moved closer to the center. Fortran now looks more like Algol, and Lisp has given up some of the wasteful habits of its youth.

The original Fortran and Lisp defined a sort of battlefield. On one side the battle cry is "Efficiency! (And besides, it would be too hard to implement.)" On the other side, the battle cry is "Abstraction! (And anyway, this isn't production software.)" As the gods determined from afar the outcomes of battles among the ancient Greeks, the outcome of this battle is being determined by hardware. Every year, things look better for Lisp. The arguments against Lisp are now starting to sound very much like the arguments that assembly language programmers gave against high-level languages in the early 1970s. The question is now becoming not Why Lisp?, but When?

#heading(level: 1)[Functions]

Functions are the building-blocks of Lisp programs. They are also the building-blocks of Lisp. In most languages the + operator is something quite different from user-defined functions. But Lisp has a single model, function application, to describe all the computation done by a program. The Lisp + operator is a function, just like the ones you can define yourself.

In fact, except for a small number of operators called special forms, the core of Lisp is a collection of Lisp functions. What's to stop you from adding to this collection? Nothing at all: if you think of something you wish Lisp could do, you can write it yourself, and your new function will be treated just like the built-in ones.

This fact has important consequences for the programmer. It means that any new function could be considered either as an addition to Lisp, or as part of a specific application. Typically, an experienced Lisp programmer will write some of each, adjusting the boundary between language and application until the two fit one another perfectly. This book is about how to achieve a good fit between language and application. Since everything we do toward this end ultimately depends on functions, functions are the natural place to begin.

#heading(level: 2)[Functions as Data]

Two things make Lisp functions different. One, mentioned above, is that Lisp itself is a collection of functions. This means that we can add to Lisp new operators of our own. Another important thing to know about functions is that they are Lisp objects.

Lisp offers most of the data types one finds in other languages. We get integers and floating-point numbers, strings, arrays, structures, and so on. But Lisp supports one data type which may at first seem surprising: the function. Nearly all programming languages provide some form of function or procedure. What does it mean to say that Lisp provides them as a data type? It means that in Lisp we can do with functions all the things we expect to do with more familiar data types, like integers: create new ones at runtime, store them in variables and in structures, pass them as arguments to other functions, and return them as results.

The ability to create and return functions at runtime is particularly useful. This might sound at first like a dubious sort of advantage, like the self-modifying machine language programs one can run on some computers. But creating new functions at runtime turns out to be a routinely used Lisp programming technique.

#heading(level: 2)[Defining Functions]

Most people first learn how to make functions with defun. The following expression defines a function called double which returns twice its argument.

```
> (defun double (x) (* x 2))
DOUBLE
```

Having fed this to Lisp, we can call double in other functions, or from the toplevel:

```
> (double 1)
2
```

A file of Lisp code usually consists mainly of such defuns, and so resembles a file of procedure definitions in a language like C or Pascal. But something quite different is going on. Those defuns are not just procedure definitions, they're Lisp calls. This distinction will become clearer when we see what's going on underneath defun.

Functions are objects in their own right. What defun really does is build one, and store it under the name given as the first argument. So as well as calling double, we can get hold of the function which implements it. The usual way to do so is by using the \#' (sharp-quote) operator. This operator can be understood as mapping names to actual function objects. By affixing it to the name of double

```
> \#'double
\#\<Interpreted-Function C66ACE\>
```

we get the actual object created by the definition above. Though its printed representation will vary from implementation to implementation, a Common Lisp function is a first-class object, with all the same rights as more familiar objects like numbers and strings. So we can pass this function as an argument, return it, store it in a data structure, and so on:

```
> (eq \#'double (car (list \#'double)))
T
```

We don't even need defun to make functions. Like most Lisp objects, we can refer to them literally. When we want to refer to an integer, we just use the integer itself. To represent a string, we use a series of characters surrounded by double-quotes. To represent a function, we use what's called a lambda-expression. A lambda-expression is a list with three parts: the symbol lambda, a parameter list, and a body of zero or more expressions. This lambda-expression refers to a function equivalent to double:

```lisp
(lambda (x) (* x 2))
```

It describes a function which takes one argument x, and returns 2x.

A lambda-expression can also be considered as the name of a function. If double is a proper name, like "Michelangelo," then (lambda (x) (\* x 2)) is a definite description, like "the man who painted the ceiling of the Sistine Chapel." By putting a sharp-quote before a lambda-expression, we get the corresponding function:

```
> \#'(lambda (x) (* x 2))
\#\<Interpreted-Function C674CE\>
```

This function behaves exactly like double, but the two are distinct objects.

In a function call, the name of the function appears first, followed by the arguments:

```
> (double 3)
6
```

Since lambda-expressions are also names of functions, they can also appear first in function calls:

```
> ((lambda (x) (* x 2)) 3)
6
```

In Common Lisp, we can have a function named double and a variable named double at the same time.

```
> (setq double 2)
2
> (double double)
4
```

When a name occurs first in a function call, or is preceded by a sharp-quote, it is taken to refer to a function. Otherwise it is treated as a variable name.

It is therefore said that Common Lisp has distinct name-spaces for variables and functions. We can have a variable called foo and a function called foo, and they need not be identical. This situation can be confusing, and leads to a certain amount of ugliness in code, but it is something that Common Lisp programmers have to live with.

If necessary, Common Lisp provides two functions which map symbols to the values, or functions, that they represent. The function symbol-value takes a symbol and returns the value of the corresponding special variable:

```
> (symbol-value 'double)
2
```

while symbol-function does the same for a globally defined function:

```
> (symbol-function 'double)
\#\<Interpreted-Function C66ACE\>
```

Note that, since functions are ordinary data objects, a variable could have a function as its value:

```
> (setq x \#'append)
\#\<Compiled-Function 46B4BE\>
> (eq (symbol-value 'x) (symbol-function 'append))
T
```

Beneath the surface, defun is setting the symbol-function of its first argument to a function constructed from the remaining arguments. The following two expressions do approximately the same thing:

```lisp
(defun double (x) (* x 2))
(setf (symbol-function 'double)
      \#'(lambda (x) (\* x 2)))
```

So defun has the same effect as procedure definition in other languages—to associate a name with a piece of code. But the underlying mechanism is not the same. We don't need defun to make functions, and functions don't have to be stored away as the value of some symbol. Underlying defun, which resembles procedure definition in any other language, is a more general mechanism: building a function and associating it with a certain name are two separate operations. When we don't need the full generality of Lisp's notion of a function, defun makes function definition as simple as in more restrictive languages.

#heading(level: 2)[Functional Arguments]

Having functions as data objects means, among other things, that we can pass them as arguments to other functions. This possibility is partly responsible for the importance of bottom-up programming in Lisp.

A language which allows functions as data objects must also provide some way of calling them. In Lisp, this function is apply. Generally, we call apply with two arguments: a function, and a list of arguments for it. The following four expressions all have the same effect:

```lisp
(+ 1 2)
(apply \#'+ '(1 2))
(apply (symbol-function '+) '(1 2))
(apply \#'(lambda (x y) (+ x y)) '(1 2))
```

In Common Lisp, apply can take any number of arguments, and the function given first will be applied to the list made by consing the rest of the arguments onto the list given last. So the expression

```lisp
(apply \#'+ 1 '(2))
```

is equivalent to the preceding four. If it is inconvenient to give the arguments as a list, we can use funcall, which differs from apply only in this respect. This expression

```lisp
(funcall \#'+ 1 2)
```

has the same effect as those above.

Many built-in Common Lisp functions take functional arguments. Among the most frequently used are the mapping functions. For example, mapcar takes two or more arguments, a function and one or more lists (one for each parameter of the function), and applies the function successively to elements of each list:

```
> (mapcar \#'(lambda (x) (+ x 10))
          '(1 2 3))
(11 12 13)
> (mapcar \#'+
          '(1 2 3)
          '(10 100 1000))
(11 102 1003)
```

Lisp programs frequently want to do something to each element of a list and get back a list of results. The first example above illustrates the conventional way to do this: make a function which does what you want done, and mapcar it over the list.

Already we see how convenient it is to be able to treat functions as data. In many languages, even if we could pass a function as an argument to something like mapcar, it would still have to be a function defined in some source file beforehand. If just one piece of code wanted to add 10 to each element of a list, we would have to define a function, called plus ten or some such, just for this one use. With lambda-expressions, we can refer to functions directly.

One of the big differences between Common Lisp and the dialects which preceded it are the large number of built-in functions that take functional arguments. Two of the most commonly used, after the ubiquitous mapcar, are sort and remove-if. The former is a general-purpose sorting function. It takes a list and a predicate, and returns a list sorted by passing each pair of elements to the predicate.

```
> (sort '(1 4 2 5 6 7 3) \#'<)
(1 2 3 4 5 6 7)
```

To remember how sort works, it helps to remember that if you sort a list with no duplicates by <, and then apply < to the resulting list, it will return true.

If remove-if weren't included in Common Lisp, it might be the first utility you would write. It takes a function and a list, and returns all the elements of the list for which the function returns false.

```
> (remove-if \#'evenp '(1 2 3 4 5 6 7))
(1 3 5 7)
```

As an example of a function which takes functional arguments, here is a definition of a limited version of remove-if:

```lisp
(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))
```

Note that within this definition fn is not sharp-quoted. Since functions are data objects, a variable can have a function as its regular value. That's what's happening here. Sharp-quote is only for referring to the function named by a symbol—usually one globally defined as such with defun.

As Chapter 4 will show, writing new utilities which take functional arguments is an important element of bottom-up programming. Common Lisp has so many utilities built-in that the one you need may exist already. But whether you use built-ins like sort, or write your own utilities, the principle is the same. Instead of wiring in functionality, pass a functional argument.

#heading(level: 2)[Functions as Properties]

The fact that functions are Lisp objects also allows us to write programs which can be extended to deal with new cases on the fly. Suppose we want to write a function which takes a type of animal and behaves appropriately. In most languages, the way to do this would be with a case statement, and we can do it this way in Lisp as well:

```lisp
(defun behave (animal)
  (case animal
    (dog (wag-tail)
         (bark))
    (rat (scurry)
         (squeak))
    (cat (rub-legs)
         (scratch-carpet))))
```

What if we want to add a new type of animal? If we were planning to add new animals, it would have been better to define behave as follows:

```lisp
(defun behave (animal)
  (funcall (get animal 'behavior)))
```

and to define the behavior of an individual animal as a function stored, for example, on the property list of its name:

```lisp
(setf (get 'dog 'behavior)
      \#'(lambda ()
          (wag-tail)
          (bark)))
```

This way, all we need do in order to add a new animal is define a new property. No functions have to be rewritten.

The second approach, though more flexible, looks slower. It is. If speed were critical, we would use structures instead of property lists and, especially, compiled instead of interpreted functions. (Section 2.9 explains how to make these.) With structures and compiled functions, the more flexible type of code can approach or exceed the speed of versions using case statements.

This use of functions corresponds to the concept of a method in object-oriented programming. Generally speaking, a method is a function which is a property of an object, and that's just what we have. If we add inheritance to this model, we'll have all the elements of object-oriented programming. Chapter 25 will show that this can be done with surprisingly little code.

One of the big selling points of object-oriented programming is that it makes programs extensible. This prospect excites less wonder in the Lisp world, where extensibility has always been taken for granted. If the kind of extensibility we need does not depend too much on inheritance, then plain Lisp may already be sufficient.

#heading(level: 2)[Scope]

Common Lisp is a lexically scoped Lisp. Scheme is the oldest dialect with lexical scope; before Scheme, dynamic scope was considered one of the defining features of Lisp.

The difference between lexical and dynamic scope comes down to how an implementation deals with free variables. A symbol is bound in an expression if it has been established as a variable, either by appearing as a parameter, or by variable-binding operators like let and do. Symbols which are not bound are said to be free. In this example, scope comes into play:

```lisp
(let ((y 7))
  (defun scope-test (x)
    (list x y)))
```

Within the defun expression, x is bound and y is free. Free variables are interesting because it's not obvious what their values should be. There's no uncertainty about the value of a bound variable—when scope-test is called, the value of x should be whatever is passed as the argument. But what should be the value of y? This is the question answered by the dialect's scope rules.

In a dynamically scoped Lisp, to find the value of a free variable when executing scope-test, we look back through the chain of functions that called it. When we find an environment where y was bound, that binding of y will be the one used in scope-test. If we find none, we take the global value of y. Thus, in a dynamically scoped Lisp, y would have the value it had in the calling expression:

```
> (let ((y 5))
    (scope-test 3))
(3 5)
```

With dynamic scope, it means nothing that y was bound to 7 when scope-test was defined. All that matters is that y had a value of 5 when scope-test was called.

In a lexically scoped Lisp, instead of looking back through the chain of calling functions, we look back through the containing environments at the time the function was defined. In a lexically scoped Lisp, our example would catch the binding of y where scope-test was defined. So this is what would happen in Common Lisp:

```
> (let ((y 5))
    (scope-test 3))
(3 7)
```

Here the binding of y to 5 at the time of the call has no effect on the returned value.

Though you can still get dynamic scope by declaring a variable to be special, lexical scope is the default in Common Lisp. On the whole, the Lisp community seems to view the passing of dynamic scope with little regret. For one thing, it used to lead to horribly elusive bugs. But lexical scope is more than a way of avoiding bugs. As the next section will show, it also makes possible some new programming techniques.

#heading(level: 2)[Closures]

Because Common Lisp is lexically scoped, when we define a function containing free variables, the system must save copies of the bindings of those variables at the time the function was defined. Such a combination of a function and a set of variable bindings is called a closure. Closures turn out to be useful in a wide variety of applications.

Closures are so pervasive in Common Lisp programs that it's possible to use them without even knowing it. Every time you give mapcar a sharp-quoted lambda-expression containing free variables, you're using closures. For example, suppose we want to write a function which takes a list of numbers and adds a certain amount to each one. The function list+

```lisp
(defun list+ (lst n)
  (mapcar \#'(lambda (x) (+ x n))
          lst))
```

will do what we want:

```
> (list+ '(1 2 3) 10)
(11 12 13)
```

If we look closely at the function which is passed to mapcar within list+, it's actually a closure. The instance of n is free, and its binding comes from the surrounding environment. Under lexical scope, every such use of a mapping function causes the creation of a closure.

Closures play a more conspicuous role in a style of programming promoted by Abelson and Sussman's classic Structure and Interpretation of Computer Programs. Closures are functions with local state. The simplest way to use this state is in a situation like the following:

```lisp
(let ((counter 0))
  (defun new-id ()
    (incf counter))
  (defun reset-id () (setq counter 0)))
```

These two functions share a variable which serves as a counter. The first one returns successive values of the counter, and the second resets the counter to 0. The same thing could be done by making the counter a global variable, but this way it is protected from unintended references.

It's also useful to be able to return functions with local state. For example, the function make-adder

```lisp
(defun make-adder (n)
  \#'(lambda (x) (+ x n)))
```

takes a number, and returns a closure which, when called, adds that number to its argument. We can make as many instances of adders as we want:

```
> (setq add2 (make-adder 2)
        add10 (make-adder 10))
\#\<Interpreted-Function BF162E\>
> (funcall add2 5)
7
> (funcall add10 3)
13
```

In the closures returned by make-adder, the internal state is fixed, but it's also possible to make closures which can be asked to change their state.

```lisp
(defun make-adderb (n)
  \#'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))
```

This new version of make-adder returns closures which, when called with one argument, behave just like the old ones.

```
> (setq addx (make-adderb 1))
\#\<Interpreted-Function BF1C66\>
> (funcall addx 3)
4
```

However, when the new type of adder is called with a non-nil second argument, its internal copy of n will be reset to the value passed as the first argument:

```
> (funcall addx 100 t)
100
> (funcall addx 3)
103
```

It's even possible to return a group of closures which share the same data objects. Figure 2.1 contains a function which creates primitive databases. It takes an assoc-list (db), and returns a list of three closures which query, add, and delete entries, respectively.

```lisp
(defun make-dbms (db)
  (list
    \#'(lambda (key)
        (cdr (assoc key db)))
    \#'(lambda (key val)
        (push (cons key val) db)
        key)
    \#'(lambda (key)
        (setf db (delete key db :key \#'car))
        key)))
```

Each call to make-dbms makes a new database—a new set of functions closed over their own shared copy of an assoc-list.

```
> (setq cities (make-dbms '((boston . us) (paris . france))))
(\#\<Interpreted-Function 8022E7\>
 \#\<Interpreted-Function 802317\>
 \#\<Interpreted-Function 802347\>)
```

The actual assoc-list within the database is invisible from the outside world—we can't even tell that it's an assoc-list—but it can be reached through the functions which are components of cities:

```
> (funcall (car cities) 'boston)
US
> (funcall (second cities) 'london 'england)
LONDON
> (funcall (car cities) 'london)
ENGLAND
```

Calling the car of a list is a bit ugly. In real programs, the access functions might instead be entries in a structure. Using them could also be cleaner—databases could be reached indirectly via functions like:

```lisp
(defun lookup (key db)
  (funcall (car db) key))
```

However, the basic behavior of closures is independent of such refinements. In real programs, the closures and data structures would also be more elaborate than those we see in make-adder or make-dbms. The single shared variable could be any number of variables, each bound to any sort of data structure.

Closures are one of the distinct, tangible benefits of Lisp. Some Lisp programs could, with effort, be translated into less powerful languages. But just try to translate a program which uses closures as above, and it will become evident how much work this abstraction is saving us. Later chapters will deal with closures in more detail. Chapter 5 shows how to use them to build compound functions, and Chapter 6 looks at their use as a substitute for traditional data structures.

#heading(level: 2)[Local Functions]

When we define functions with lambda-expressions, we face a restriction which doesn't arise with defun: a function defined in a lambda-expression doesn't have a name and therefore has no way of referring to itself. This means that in Common Lisp we can't use lambda to define a recursive function.

If we want to apply some function to all the elements of a list, we use the most familiar of Lisp idioms:

```lisp
> (mapcar #'(lambda (x) (+ 2 x))
          '(2 5 7 3))
(4 7 9 5)
```

What about cases where we want to give a recursive function as the first argument to mapcar? If the function has been defined with defun, we can simply refer to it by name:

```lisp
> (mapcar #'copy-tree '((a b) (c d e)))
((A B) (C D E))
```

But now suppose that the function has to be a closure, taking some bindings from the environment in which the mapcar occurs. In our example list+,

```lisp
(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))
```

the first argument to mapcar, `#'(lambda (x) (+ x n))`, must be defined within list+ because it needs to catch the binding of n. So far so good, but what if we want to give mapcar a function which both needs local bindings and is recursive? We can't use a function defined elsewhere with defun, because we need bindings from the local environment. And we can't use lambda to define a recursive function, because the function will have no way of referring to itself.

Common Lisp gives us labels as a way out of this dilemma. With one important reservation, labels could be described as a sort of let for functions. Each of the binding specifications in a labels expression should have the form

```
(name parameters . body)
```

Within the labels expression, name will refer to a function equivalent to:

```
#'(lambda parameters . body)
```

So for example:

```lisp
> (labels ((inc (x) (1+ x)))
    (inc 3))
4
```

However, there is an important difference between let and labels. In a let expression, the value of one variable can't depend on another variable made by the same let—that is, you can't say

```lisp
(let ((x 10) (y x))
  y)
```

and expect the value of the new y to reflect that of the new x. In contrast, the body of a function f defined in a labels expression may refer to any other function defined there, including f itself, which makes recursive function definitions possible.

Using labels we can write a function analogous to list+, but in which the first argument to mapcar is a recursive function:

```lisp
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))
```

This function takes an object and a list, and returns a list of the number of occurrences of the object in each element:

```lisp
> (count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
(1 2 1 2)
```

#heading(level: 2)[Tail-Recursion]

A recursive function is one that calls itself. Such a call is tail-recursive if no work remains to be done in the calling function afterwards. This function is not tail-recursive

```lisp
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))
```

because on returning from the recursive call we have to pass the result to 1+. The following function is tail-recursive, though

```lisp
(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
      (our-find-if fn (cdr lst))))
```

because the value of the recursive call is immediately returned.

Tail-recursion is desirable because many Common Lisp compilers can transform tail-recursive functions into loops. With such a compiler, you can have the elegance of recursion in your source code without the overhead of function calls at runtime. The gain in speed is usually great enough that programmers go out of their way to make functions tail-recursive.

A function which isn't tail-recursive can often be transformed into one that is by embedding in it a local function which uses an accumulator. In this context, an accumulator is a parameter representing the value computed so far. For example, our-length could be transformed into

```lisp
(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))
```

where the number of list elements seen so far is contained in a second parameter, acc. When the recursion reaches the end of the list, the value of acc will be the total length, which can just be returned. By accumulating the value as we go down the calling tree instead of constructing it on the way back up, we can make rec tail-recursive.

Many Common Lisp compilers can do tail-recursion optimization, but not all of them do it by default. So after writing your functions to be tail-recursive, you may also want to put

```lisp
(proclaim '(optimize speed))
```

at the top of the file, to ensure that the compiler can take advantage of your efforts.

Given tail-recursion and type declarations, existing Common Lisp compilers can generate code that runs as fast as, or faster than, C. Richard Gabriel gives as an example the following function, which returns the sum of the integers from 1 to n:

```lisp
(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))
```

This is what fast Common Lisp code looks like. At first it may not seem natural to write functions this way. It's often a good idea to begin by writing a function in whatever way seems most natural, and then, if necessary, transforming it into a tail-recursive equivalent.

#heading(level: 2)[Compilation]

Lisp functions can be compiled either individually or by the file. If you just type a defun expression into the toplevel,

```lisp
> (defun foo (x) (1+ x))
FOO
```

many implementations will create an interpreted function. You can check whether a given function is compiled by feeding it to compiled-function-p:

```lisp
> (compiled-function-p #'foo)
NIL
```

We can have foo compiled by giving its name to compile

```lisp
> (compile 'foo)
FOO
```

which will compile the definition of foo and replace the interpreted version with a compiled one.

```lisp
> (compiled-function-p #'foo)
T
```

Compiled and interpreted functions are both Lisp objects, and behave the same, except with respect to compiled-function-p. Literal functions can also be compiled: compile expects its first argument to be a name, but if you give nil as the first argument, it will compile the lambda-expression given as the second argument.

```lisp
> (compile nil '(lambda (x) (+ x 2)))
#\<Compiled-Function BF55BE\>
```

If you give both the name and function arguments, compile becomes a sort of compiling defun:

```lisp
> (progn (compile 'bar '(lambda (x) (* x 3)))
         (compiled-function-p #'bar))
T
```

Having compile in the language means that a program could build and compile new functions on the fly. However, calling compile explicitly is a drastic measure, comparable to calling eval, and should be viewed with the same suspicion.

When Section 2.1 said that creating new functions at runtime was a routinely used programming technique, it referred to new closures like those made by make-adder, not functions made by calling compile on raw lists. Calling compile is not a routinely used programming technique—it's an extremely rare one. So beware of doing it unnecessarily. Unless you're implementing another language on top of Lisp (and much of the time, even then), what you need to do may be possible with macros.

There are two sorts of functions which you can't give as an argument to compile. According to CLTL2 (p. 677), you can't compile a function "defined interpretively in a non-null lexical environment." That is, if at the toplevel you define foo within a let

```lisp
> (let ((y 2))
    (defun foo (x) (+ x y)))
```

then (compile 'foo) will not necessarily work. You also can't call compile on a function which is already compiled. In this situation, CLTL2 hints darkly that "the consequences...are unspecified."

The usual way to compile Lisp code is not to compile functions individually with compile, but to compile whole files with compile-file. This function takes a filename and creates a compiled version of the source file—typically with the same base name but a different extension. When the compiled file is loaded, compiled-function-p should return true for all the functions defined in the file.

Later chapters will depend on another effect of compilation: when one function occurs within another function, and the containing function is compiled, the inner function will also get compiled. CLTL2 does not seem to say explicitly that this will happen, but in a decent implementation you can count on it.

The compiling of inner functions becomes evident in functions which return functions. When make-adder (page 18) is compiled, it will return compiled functions:

```lisp
> (compile 'make-adder)
MAKE-ADDER
> (compiled-function-p (make-adder 2))
T
```

As later chapters will show, this fact is of great importance in the implementation of embedded languages. If a new language is implemented by transformation, and the transformation code is compiled, then it yields compiled output—and so becomes in effect a compiler for the new language. (A simple example is described on page 81.)

If we have a particularly small function, we may want to request that it be compiled inline. Otherwise, the machinery of calling it could entail more effort than the function itself. If we define a function:

```lisp
(defun 50th (lst) (nth 49 lst))
```

and make the declaration:

```lisp
(proclaim '(inline 50th))
```

then a reference to 50th within a compiled function should no longer require a real function call. If we define and compile a function which calls 50th,

```lisp
(defun foo (lst)
  (+ (50th lst) 1))
```

then when foo is compiled, the code for 50th should be compiled right into it, just as if we had written

```lisp
(defun foo (lst)
  (+ (nth 49 lst) 1))
```

in the first place. The drawback is that if we redefine 50th, we also have to recompile foo, or it will still reflect the old definition. The restrictions on inline functions are basically the same as those on macros (see Section 7.9).

#heading(level: 2)[Functions from Lists]

In some earlier dialects of Lisp, functions were represented as lists. This gave Lisp programs the remarkable ability to write and execute their own Lisp programs. In Common Lisp, functions are no longer made of lists—good implementations compile them into native machine code. But you can still write programs that write programs, because lists are the input to the compiler.

It cannot be overemphasized how important it is that Lisp programs can write Lisp programs, especially since this fact is so often overlooked. Even experienced Lisp users rarely realize the advantages they derive from this feature of the language. This is why Lisp macros are so powerful, for example. Most of the techniques described in this book depend on the ability to write programs which manipulate Lisp expressions.

#heading(level: 1)[Functional Programming]

The previous chapter explained how Lisp and Lisp programs are both built out of a single raw material: the function. Like any building material, its qualities influence both the kinds of things we build, and the way we build them.

This chapter describes the kind of construction methods which prevail in the Lisp world. The sophistication of these methods allows us to attempt more ambitious kinds of programs. The next chapter will describe one particularly important class of programs which become possible in Lisp: programs which evolve instead of being developed by the old plan-and-implement method.

#heading(level: 2)[Functional Design]

The character of an object is influenced by the elements from which it is made. A wooden building looks different from a stone one, for example. Even when you are too far away to see wood or stone, you can tell from the overall shape of the building what it's made of. The character of Lisp functions has a similar influence on the structure of Lisp programs.

Functional programming means writing programs which work by returning values instead of by performing side-effects. Side-effects include destructive changes to objects (e.g. by rplaca) and assignments to variables (e.g. by setq). If side-effects are few and localized, programs become easier to read, test, and debug. Lisp programs have not always been written in this style, but over time Lisp and functional programming have gradually become inseparable.

An example will show how functional programming differs from what you might do in another language. Suppose for some reason we want the elements of a list in the reverse order. Instead of writing a function to reverse lists, we write a function which takes a list, and returns a list with the same elements in the reverse order.

```lisp
(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))
```

Figure 3.1: A function to reverse lists.

Figure 3.1 contains a function to reverse lists. It treats the list as an array, reversing it in place; its return value is irrelevant:

```lisp
> (setq lst '(a b c))
(A B C)
> (bad-reverse lst)
NIL
> lst
(C B A)
```

As its name suggests, bad-reverse is far from good Lisp style. Moreover, its ugliness is contagious: because it works by side-effects, it will also draw its callers away from the functional ideal.

Though cast in the role of the villain, bad-reverse does have one merit: it shows the Common Lisp idiom for swapping two values. The rotatef macro rotates the values of any number of generalized variables—that is, expressions you could give as the first argument to setf. When applied to just two arguments, the effect is to swap them.

In contrast, Figure 3.2 shows a function which returns reversed lists. With good-reverse, we get the reversed list as the return value; the original list is not touched.

```lisp
> (setq lst '(a b c))
(A B C)
> (good-reverse lst)
(C B A)
> lst
(A B C)
```

```lisp
(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))
```

Figure 3.2: A function to return reversed lists.

It used to be thought that you could judge someone's character by looking at the shape of his head. Whether or not this is true of people, it is generally true of Lisp programs. Functional programs have a different shape from imperative ones. The structure in a functional program comes entirely from the composition of arguments within expressions, and since arguments are indented, functional code will show more variation in indentation. Functional code looks fluid on the page; imperative code looks solid and blockish, like Basic.

Even from a distance, the shapes of bad- and good-reverse suggest which is the better program. And despite being shorter, good-reverse is also more efficient: O(n) instead of O(n²).

We are spared the trouble of writing reverse because Common Lisp has it built-in. It is worth looking briefly at this function, because it is one that often brings to the surface misconceptions about functional programming. Like good-reverse, the built-in reverse works by returning a value—it doesn't touch its arguments. But people learning Lisp may assume that, like bad-reverse, it works by side-effects. If in some part of a program they want a list lst to be reversed, they may write

```lisp
(reverse lst)
```

and wonder why the call seems to have no effect. In fact, if we want effects from such a function, we have to see to it ourselves in the calling code. That is, we need to write

```lisp
(setq lst (reverse lst))
```

instead. Operators like reverse are intended to be called for return values, not side-effects. It is worth writing your own programs in this style too—not only for its inherent benefits, but because, if you don't, you will be working against the language.

One of the points we ignored in the comparison of bad- and good-reverse is that bad-reverse doesn't cons. Instead of building new list structure, it operates on the original list. This can be dangerous—the list could be needed elsewhere in the program—but for efficiency it is sometimes necessary. For such cases, Common Lisp provides an O(n) destructive reversing function called nreverse. A destructive function is one that can alter the arguments passed to it. However, even destructive functions usually work by returning values: you have to assume that nreverse will recycle lists you give to it as arguments, but you still can't assume that it will reverse them. As before, the reversed list has to be found in the return value. You still can't write

```lisp
(nreverse lst)
```

in the middle of a function and assume that afterwards lst will be reversed. This is what happens in most implementations:

```lisp
> (setq lst '(a b c))
(A B C)
> (nreverse lst)
(C B A)
> lst
(A)
```

To reverse lst, you have would have to set lst to the return value, as with plain reverse.

If a function is advertised as destructive, that doesn't mean that it's meant to be called for side-effects. The danger is, some destructive functions give the impression that they are. For example,

```lisp
(nconc x y)
```

almost always has the same effect as

```lisp
(setq x (nconc x y))
```

If you wrote code which relied on the former idiom, it might seem to work for some time. However, it wouldn't do what you expected when x was nil.

Only a few Lisp operators are intended to be called for side-effects. In general, the built-in operators are meant to be called for their return values. Don't be misled by names like sort, remove, or substitute. If you want side-effects, use setq on the return value.

This very rule suggests that some side-effects are inevitable. Having functional programming as an ideal doesn't imply that programs should never have side-effects. It just means that they should have no more than necessary.

It may take time to develop this habit. One way to start is to treat the following operators as if there were a tax on their use:

```
set setq setf psetf psetq incf decf push pop pushnew
rplaca rplacd rotatef shiftf remf remprop remhash
```

and also let\*, in which imperative programs often lie concealed. Treating these operators as taxable is only proposed as a help toward, not a criterion for, good Lisp style. However, this alone can get you surprisingly far.

In other languages, one of the most common causes of side-effects is the need for a function to return multiple values. If functions can only return one value, they have to "return" the rest by altering their parameters. Fortunately, this isn't necessary in Common Lisp, because any function can return multiple values.

The built-in function truncate returns two values, for example—the truncated integer, and what was cut off in order to create it. A typical implementation will print both when truncate is called at the toplevel:

```lisp
> (truncate 26.21875)
26
0.21875
```

When the calling code only wants one value, the first one is used:

```lisp
> (= (truncate 26.21875) 26)
T
```

The calling code can catch both return values by using a multiple-value-bind. This operator takes a list of variables, a call, and a body of code. The body is evaluated with the variables bound to the respective return values from the call:

```lisp
> (multiple-value-bind (int frac) (truncate 26.21875)
    (list int frac))
(26 0.21875)
```

Finally, to return multiple values, we use the values operator:

```lisp
> (defun powers (x)
    (values x (sqrt x) (expt x 2)))
POWERS
> (multiple-value-bind (base root square) (powers 4)
    (list base root square))
(4 2.0 16)
```

Functional programming is a good idea in general. It is a particularly good idea in Lisp, because Lisp has evolved to support it. Built-in operators like reverse and nreverse are meant to be used in this way. Other operators, like values and multiple-value-bind, have been provided specifically to make functional programming easier.

#heading(level: 2)[Imperative Outside-In]

The aims of functional programming may show more clearly when contrasted with those of the more common approach, imperative programming. A functional program tells you what it wants; an imperative program tells you what to do. A functional program says "Return a list of a and the square of the first element of x:"

```lisp
(defun fun (x)
  (list 'a (expt (car x) 2)))
```

An imperative programs says "Get the first element of x, then square it, then return a list of a and the square:"

```lisp
(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))
```

Lisp users are fortunate in being able to write this program both ways. Some languages are only suited to imperative programming—notably Basic, along with most machine languages. In fact, the definition of imp is similar in form to the machine language code that most Lisp compilers would generate for fun.

Why write such code when the compiler could do it for you? For many programmers, this question does not even arise. A language stamps its pattern on our thoughts: someone used to programming in an imperative language may have begun to conceive of programs in imperative terms, and may actually find it easier to write imperative programs than functional ones. This habit of mind is worth overcoming if you have a language that will let you.

For alumni of other languages, beginning to use Lisp may be like stepping onto a skating rink for the first time. It's actually much easier to get around on ice than it is on dry land—if you use skates. Till then you will be left wondering what people see in this sport.

What skates are to ice, functional programming is to Lisp. Together the two allow you to travel more gracefully, with less effort. But if you are accustomed to another mode of travel, this may not be your experience at first. One of the obstacles to learning Lisp as a second language is learning to program in a functional style.

Fortunately there is a trick for transforming imperative programs into functional ones. You can begin by applying this trick to finished code. Soon you will begin to anticipate yourself, and transform your code as you write it. Soon after that, you will begin to conceive of programs in functional terms from the start.

The trick is to realize that an imperative program is a functional program turned inside-out. To find the functional program implicit in our imperative one, we just turn it outside-in. Let's try this technique on imp.

The first thing we notice is the creation of y and sqr in the initial let. This is a sign that bad things are to follow. Like eval at runtime, uninitialized variables are so rarely needed that they should generally be treated as a symptom of some illness in the program. Such variables are often used like pins which hold the program down and keep it from coiling into its natural shape.

However, we ignore them for the time being, and go straight to the end of the function. What occurs last in an imperative program occurs outermost in a functional one. So our first step is to grab the final call to list and begin stuffing the rest of the program inside it—just like turning a shirt inside-out. We continue by applying the same transformation repeatedly, just as we would with the sleeves of the shirt, and in turn with their cuffs.

Starting at the end, we replace sqr with (expt y 2), yielding:
```lisp
(list 'a (expt y 2)))
```

Then we replace y by (car x):
```lisp
(list 'a (expt (car x) 2))
```

Now we can throw away the rest of the code, having stuffed it all into the last expression. In the process we removed the need for the variables y and sqr, so we can discard the let as well.

The final result is shorter than what we began with, and easier to understand. In the original code, we're faced with the final expression (list 'a sqr), and it's not immediately clear where the value of sqr comes from. Now the source of the return value is laid out for us like a road map.

The example in this section was a short one, but the technique scales up. Indeed, it becomes more valuable as it is applied to larger functions. Even functions which perform side-effects can be cleaned up in the portions which don't.

#heading(level: 2)[Functional Interfaces]

Some side-effects are worse than others. For example, though this function calls nconc

```lisp
(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))
```

it preserves referential transparency. If you call it with a given argument, it will always return the same (equal) value. From the caller's point of view, qualify might as well be purely functional code. We can't say the same for bad-reverse (page 29), which actually modifies its argument.

Instead of treating all side-effects as equally bad, it would be helpful if we had some way of distinguishing between such cases. Informally, we could say that it's harmless for a function to modify something that no one else owns. For example, the nconc in qualify is harmless because the list given as the first argument is freshly consed. No one else could own it.

In the general case, we have to talk about ownership not by functions, but by invocations of functions. Though no one else owns the variable x here,

```lisp
(let ((x 0))
  (defun total (y)
    (incf x y)))
```

the effects of one call will be visible in succeeding ones. So the rule should be: a given invocation can safely modify what it uniquely owns.

Who owns arguments and return values? The convention in Lisp seems to be that an invocation owns objects it receives as return values, but not objects passed to it as arguments. Functions that modify their arguments are distinguished by the label "destructive," but there is no special name for functions that modify objects returned to them.

This function adheres to the convention, for example:

```lisp
(defun ok (x)
  (nconc (list 'a x) (list 'c)))
```

It calls nconc, which doesn't, but since the list spliced by nconc will always be freshly made rather than, say, a list passed to ok as an argument, ok itself is ok.

If it were written slightly differently, however,

```lisp
(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))
```

then the call to nconc would be modifying an argument passed to not-ok.

Many Lisp programs violate this convention, at least locally. However, as we saw with ok, local violations need not disqualify the calling function. And functions which do meet the preceding conditions will retain many of the advantages of purely functional code.

To write programs that are really indistinguishable from functional code, we have to add one more condition. Functions can't share objects with other code that doesn't follow the rules. For example, though this function doesn't have side-effects,

```lisp
(defun anything (x)
  (+ x *anything*))
```

its return value depends on the global variable *anything*. So if any other function can alter the value of this variable, anything could return anything.

Code written so that each invocation only modifies what it owns is almost as good as purely functional code. A function that meets all the preceding conditions at least presents a functional interface to the world: if you call it twice with the same arguments, you should get the same results. And this, as the next section will show, is a crucial ingredient in bottom-up programming.

One problem with destructive operations is that, like global variables, they can destroy the locality of a program. When you're writing functional code, you can narrow your focus: you only need consider the functions that call, or are called by, the one you're writing. This benefit disappears when you want to modify something destructively. It could be used anywhere.

The conditions above do not guarantee the perfect locality you get with purely functional code, though they do improve things somewhat. For example, suppose that f calls g as below:

```lisp
(defun f (x)
  (let ((val (g x)))
    ; safe to modify val here?
    ))
```

Is it safe for f to nconc something onto val? Not if g is identity: then we would be modifying something originally passed as an argument to f itself.

So even in programs which do follow the convention, we may have to look beyond f if we want to modify something there. However, we don't have to look as far: instead of worrying about the whole program, we now only have to consider the subtree beginning with f.

A corollary of the convention above is that functions shouldn't return anything that isn't safe to modify. Thus one should avoid writing functions whose return values incorporate quoted objects. If we define exclaim so that its return value incorporates a quoted list,

```lisp
(defun exclaim (expression)
  (append expression '(oh my)))
```

Then any later destructive modification of the return value

```lisp
> (exclaim '(lions and tigers and bears))
(LIONS AND TIGERS AND BEARS OH MY)
> (nconc * '(goodness))
(LIONS AND TIGERS AND BEARS OH MY GOODNESS)
```

could alter the list within the function:

```lisp
> (exclaim '(fixnums and bignums and floats))
(FIXNUMS AND BIGNUMS AND FLOATS OH MY GOODNESS)
```

To make exclaim proof against such problems, it should be written:

```lisp
(defun exclaim (expression)
  (append expression (list 'oh 'my)))
```

There is one major exception to the rule that functions shouldn't return quoted lists: the functions which generate macro expansions. Macro expanders can safely incorporate quoted lists in the expansions they generate, if the expansions are going straight to the compiler.

Otherwise, one might as well be a suspicious of quoted lists generally. Many other uses of them are likely to be something which ought to be done with a macro like in (page 152).

#heading(level: 2)[Interactive Programming]

The previous sections presented the functional style as a good way of organizing programs. But it is more than this. Lisp programmers did not adopt the functional style purely for aesthetic reasons. They use it because it makes their work easier. In Lisp's dynamic environment, functional programs can be written with unusual speed, and at the same time, can be unusually reliable.

In Lisp it is comparatively easy to debug programs. A lot of information is available at runtime, which helps in tracing the causes of errors. But even more important is the ease with which you can test programs. You don't have to compile a program and test the whole thing at once. You can test functions individually by calling them from the toplevel loop.

Incremental testing is so valuable that Lisp style has evolved to take advantage of it. Programs written in the functional style can be understood one function at a time, and from the point of view of the reader this is its main advantage. However, the functional style is also perfectly adapted to incremental testing: programs written in this style can also be tested one function at a time. When a function neither examines nor alters external state, any bugs will appear immediately. Such a function can affect the outside world only through its return values. Insofar as these are what you expected, you can trust the code which produced them.

Experienced Lisp programmers actually design their programs to be easy to test:

1. They try to segregate side-effects in a few functions, allowing the greater part of the program to be written in a purely functional style.

2. If a function must perform side-effects, they try at least to give it a functional interface.

3. They give each function a single, well-defined purpose.

When a function is written, they can test it on a selection of representative cases, then move on to the next one. If each brick does what it's supposed to do, the wall will stand.

In Lisp, the wall can be better-designed as well. Imagine the kind of conversation you would have with someone so far away that there was a transmission delay of one minute. Now imagine speaking to someone in the next room. You wouldn't just have the same conversation faster, you would have a different kind of conversation. In Lisp, developing software is like speaking face-to-face. You can test code as you're writing it. And instant turnaround has just as dramatic an effect on development as it does on conversation. You don't just write the same program faster; you write a different kind of program.

How so? When testing is quicker you can do it more often. In Lisp, as in any language, development is a cycle of writing and testing. But in Lisp the cycle is very short: single functions, or even parts of functions. And if you test everything as you write it, you will know where to look when errors occur: in what you wrote last. Simple as it sounds, this principle is to a large extent what makes bottom-up programming feasible. It brings an extra degree of confidence which enables Lisp programmers to break free, at least part of the time, from the old plan-and-implement style of software development.

Section 1.1 stressed that bottom-up design is an evolutionary process. You build up a language as you write a program in it. This approach can work only if you trust the lower levels of code. If you really want to use this layer as a language, you have to be able to assume, as you would with any language, that any bugs you encounter are bugs in your application and not in the language itself.

So your new abstractions are supposed to bear this heavy burden of responsibility, and yet you're supposed to just spin them off as the need arises? Just so; in Lisp you can have both. When you write programs in a functional style and test them incrementally, you can have the flexibility of doing things on the spur of the moment, plus the kind of reliability one usually associates with careful planning.

#heading(level: 1)[Utility Functions]

Common Lisp operators come in three types: functions and macros, which you can write yourself, and special forms, which you can't. This chapter describes techniques for extending Lisp with new functions. But "techniques" here means something different from what it usually does. The important thing to know about such functions is not how they're written, but where they come from. An extension to Lisp will be written using mostly the same techniques you would use to write any other Lisp function. The hard part of writing these extensions is not deciding how to write them, but deciding which ones to write.

#heading(level: 2)[Birth of a Utility]

In its simplest form, bottom-up programming means second-guessing whoever designed your Lisp. At the same time as you write your program, you also add to Lisp new operators which make your program easy to write. These new operators are called utilities.

The term "utility" has no precise definition. A piece of code can be called a utility if it seems too small to be considered as a separate application, and too general-purpose to be considered as part of a particular program. A database program would not be a utility, for example, but a function which performed a single operation on a list could be. Most utilities resemble the functions and macros that Lisp has already. In fact, many of Common Lisp's built-in operators began life as utilities. The function remove-if-not, which collects all the elements of a list satisfying some predicate, was defined by individual programmers for years before it became a part of Common Lisp.

Learning to write utilities would be better described as learning the habit of writing them, rather than the technique of writing them. Bottom-up programming means simultaneously writing a program and a programming language. To do this well, you have to develop a fine sense of which operators a program is lacking. You have to be able to look at a program and say, "Ah, what you really mean to say is this."

For example, suppose that nicknames is a function which takes a name and builds a list of all the nicknames which could be derived from it. Given this function, how do we collect all the nicknames yielded by a list of names? Someone learning Lisp might write a function like:

```lisp
(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names))
             (all-nicknames (cdr names)))))
```

A more experienced Lisp programmer can look at such a function and say "Ah, what you really want is mapcan." Then instead of having to define and call a new function to find all the nicknames of a group of people, you can use a single expression:

```lisp
(mapcan #'nicknames people)
```

The definition of all-nicknames is reinventing the wheel. However, that's not all that's wrong with it: it is also burying in a specific function something that could be done by a general-purpose operator.

In this case the operator, mapcan, already exists. Anyone who knew about mapcan would feel a little uncomfortable looking at all-nicknames. To be good at bottom-up programming is to feel equally uncomfortable when the missing operator is one which hasn't been written yet. You must be able to say "what you really want is x," and at the same time, to know what x should be.

Lisp programming entails, among other things, spinning off new utilities as you need them. The aim of this section is to show how such utilities are born. Suppose that towns is a list of nearby towns, sorted from nearest to farthest, and that bookshops is a function which returns a list of all the bookshops in a city. If we want to find the nearest town which has any bookshops, and the bookshops in it, we could begin with:

```lisp
(let ((town (find-if #'bookshops towns)))
  (values town (bookshops town)))
```

But this is a bit inelegant: when find-if finds an element for which bookshops returns a non-nil value, the value is thrown away, only to be recomputed as soon as find-if returns. If bookshops were an expensive call, this idiom would be inefficient as well as ugly. To avoid unnecessary work, we could use the following function instead:

```lisp
(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
            (values (car towns) shops)
            (find-books (cdr towns))))))
```

Then calling (find-books towns) would at least get us what we wanted with no more computation than necessary. But wait—isn't it likely that at some time in the future we will want to do the same kind of search again? What we really want here is a utility which combines find-if and some, returning both the successful element, and the value returned by the test function. Such a utility could be defined as:

```lisp
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))
```

Notice the similarity between find-books and find2. Indeed, the latter could be described as the skeleton of the former. Now, using the new utility, we can achieve our original aim with a single expression:

```lisp
(find2 #'bookshops towns)
```

One of the unique characteristics of Lisp programming is the important role of functions as arguments. This is part of why Lisp is well-adapted to bottom-up programming. It's easier to abstract out the bones of a function when you can pass the flesh back as a functional argument.

Introductory programming courses teach early on that abstraction leads to less duplication of effort. One of the first lessons is: don't wire in behavior. For example, instead of defining two functions which do the same thing but for one or two constants, define a single function and pass the constants as arguments.

In Lisp we can carry this idea further, because we can pass whole functions as arguments. In both of the previous examples we went from a specific function to a more general function which took a functional argument. In the first case we used the predefined mapcan and in the second we wrote a new utility, find2, but the general principle is the same: instead of mixing the general and the specific, define the general and pass the specific as an argument.

When carefully applied, this principle yields noticeably more elegant programs. It is not the only force driving bottom-up design, but it is a major one. Of the 32 utilities defined in this chapter, 18 take functional arguments.

#heading(level: 2)[Invest in Abstraction]

If brevity is the soul of wit, it is also, along with efficiency, the essence of good software. The cost of writing or maintaining a program increases with its length. All other things being equal, the shorter program is the better.

From this point of view, the writing of utilities should be treated as a capital expenditure. By replacing find-books with the utility find2, we end up with just as many lines of code. But we have made the program shorter in one sense, because the length of the utility does not have to be charged against the current program.

It is not just an accounting trick to treat extensions to Lisp as capital expenditures. Utilities can go into a separate file; they will not clutter our view as we're working on the program, nor are they likely to be involved if we have to return later to change the program in some respect.

As capital expenditures, however, utilities demand extra attention. It is especially important that they be well-written. They are going to be used repeatedly, so any incorrectness or inefficiency will be multiplied. Extra care must also go into their design: a new utility must be written for the general case, not just for the problem at hand. Finally, like any capital expenditure, we need not be in a hurry about it. If you're thinking of spinning off some new operator, but aren't sure that you will want it elsewhere, write it anyway, but leave it with the particular program which uses it. Later if you use the new operator in other programs, you can promote it from a subroutine to a utility and make it generally accessible.

The utility find2 seems to be a good investment. By making a capital outlay of 7 lines, we get an immediate savings of 7. The utility has paid for itself in the first use. A programming language, Guy Steele wrote, should "cooperate with our natural tendency towards brevity:"

...we tend to believe that the expense of a programming construct is proportional to the amount of writer's cramp that it causes us (by "belief" I mean here an unconscious tendency rather than a fervent conviction). Indeed, this is not a bad psychological principle for language designers to keep in mind. We think of addition as cheap partly because we can notate it with a single character: "+". Even if we believe that a construct is expensive, we will often prefer it to a cheaper one if it will cut our writing effort in half.

In any language, the "tendency towards brevity" will cause trouble unless it is allowed to vent itself in new utilities. The shortest idioms are rarely the most efficient ones. If we want to know whether one list is longer than another, raw Lisp will tempt us to write

```lisp
(> (length x) (length y))
```

If we want to map a function over several lists, we will likewise be tempted to join them together first:

```lisp
(mapcar fn (append x y z))
```

Such examples show that it's especially important to write utilities for situations we might otherwise handle inefficiently. A language augmented with the right utilities will lead us to write more abstract programs. If these utilities are properly defined, it will also lead us to write more efficient ones.

A collection of utilities will certainly make programming easier. But they can do more than that: they can make you write better programs. The muses, like cooks, spring into action at the sight of ingredients. This is why artists like to have a lot of tools and materials in their studios. They know that they are more likely to start something new if they have what they need ready at hand. The same phenomenon appears with programs written bottom-up. Once you have written a new utility, you may find yourself using it more than you would have expected.

The following sections describe several classes of utility functions. They do not by any means represent all the different types of functions you might add to Lisp. However, all the utilities given as examples are ones that have proven their worth in practice.

#heading(level: 2)[Operations on Lists]

Lists were originally Lisp's main data structure. Indeed, the name "Lisp" comes from "LISt Processing." It is as well not to be misled by this historical fact, however. Lisp is not inherently about processing lists any more than Polo shirts are for Polo. A highly optimized Common Lisp program might never see a list. It would still be a list, though, at least at compile-time. The most sophisticated programs, which use lists less at runtime, use them proportionately more at compile-time, when generating macro expansions. So although the role of lists is decreased in modern dialects, operations on lists can still make up the greater part of a Lisp program.

```lisp
(proclaim '(inline last1 single append1 conc1 mklist))
(defun last1 (lst)
  (car (last lst)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj)
  (append lst (list obj)))
(defun conc1 (lst obj)
  (nconc lst (list obj)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))
```

Figure 4.1: Small functions which operate on lists.

Figures 4.1 and 4.2 contain a selection of functions which build or examine lists. Those given in Figure 4.1 are among the smallest utilities worth defining. For efficiency, they should all be declared inline (page 26).

The first, last1, returns the last element in a list. The built-in function last returns the last cons in a list, not the last element. Most of the time one uses it to get the last element, by saying (car (last ...)). Is it worth writing a new utility for such a case? Yes, when it effectively replaces one of the built-in operators.

Notice that last1 does no error-checking. In general, none of the code defined in this book will do error-checking. Partly this is just to make the examples clearer. But in shorter utilities it is reasonable not to do any error-checking anyway. If we try:

```lisp
> (last1 "blub")
>>Error: "blub" is not a list.
Broken at LAST...
```

the error will be caught by last itself. When utilities are small, they form a layer of abstraction so thin that it starts to be transparent. As one can see through a thin layer of ice, one can see through utilities like last1 to interpret errors which arise in the underlying functions.

The function single tests whether something is a list of one element. Lisp programs need to make this test rather often. At first one might be tempted to use the natural translation from English:

```lisp
(= (length lst) 1)
```

Written this way, the test would be very inefficient. We know all we need to know as soon as we've looked past the first element.

Next come append1 and conc1. Both attach a new element to the end of a list, the latter destructively. These functions are small, but so frequently needed that they are worth defining. Indeed, append1 has been predefined in previous Lisp dialects.

So has mklist, which was predefined in (at least) Interlisp. Its purpose is to ensure that something is a list. Many Lisp functions are written to return either a single value or a list of values. Suppose that lookup is such a function, and that we want to collect the results of calling it on all the elements of a list called data. We can do so by writing:

```lisp
(mapcan #'(lambda (d) (mklist (lookup d)))
        data)
```

```lisp
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
```

Figure 4.2: Larger functions that operate on lists.

Figure 4.2 contains some larger examples of list utilities. The first, longer, is useful from the point of view of efficiency as well as abstraction. It compares two sequences and returns true only if the first is longer. When comparing the lengths of two lists, it is tempting to do just that:

```lisp
(> (length x) (length y))
```

This idiom is inefficient because it requires the program to traverse the entire length of both lists. If one list is much longer than the other, all the effort of traversing the difference in their lengths will be wasted. It is faster to do as longer does and traverse the two lists in parallel.

Embedded within longer is a recursive function to compare the lengths of two lists. Since longer is for comparing lengths, it should work for anything that you could give as an argument to length. But the possibility of comparing lengths in parallel only applies to lists, so the internal function is only called if both arguments are lists.

The next function, filter, is to some what remove-if-not is to find-if. The built-in remove-if-not returns all the values that might have been returned if you called find-if with the same function on successive cdrs of a list. Analogously, filter returns what some would have returned for successive cdrs of the list:

```lisp
> (filter #'(lambda (x) (if (numberp x) (1+ x)))
          '(a 1 2 b 3 c d 4))
(2 3 4 5)
```

You give filter a function and a list, and get back a list of whatever non-nil values are returned by the function as it is applied to the elements of the list.

Notice that filter uses an accumulator in the same way as the tail-recursive functions described in Section 2.8. Indeed, the aim in writing a tail-recursive function is to have the compiler generate code in the shape of filter. For filter, the straightforward iterative definition is simpler than the tail-recursive one. The combination of push and nreverse in the definition of filter is the standard Lisp idiom for accumulating a list.

The last function in Figure 4.2 is for grouping lists into sublists. You give group a list l and a number n, and it will return a new list in which the elements of l are grouped into sublists of length n. The remainder is put in a final sublist. Thus if we give 2 as the second argument, we get an assoc-list:

```lisp
> (group '(a b c d e f g) 2)
((A B) (C D) (E F) (G))
```

This function is written in a rather convoluted way in order to make it tail-recursive (Section 2.8). The principle of rapid prototyping applies to individual functions as well as to whole programs. When writing a function like flatten, it can be a good idea to begin with the simplest possible implementation. Then, once the simpler version works, you can replace it if necessary with a more efficient tail-recursive or iterative version. If it's short enough, the initial version could be left as a comment to describe the behavior of its replacement. (Simpler versions of group and several other functions in Figures 4.2 and 4.3 are included in the note on page 389.)

The definition of group is unusual in that it checks for at least one error: a second argument of 0, which would otherwise send the function into an infinite recursion.

In one respect, the examples in this book deviate from usual Lisp practice: to make the chapters independent of one another, the code examples are as much as possible written in raw Lisp. Because it is so useful in defining macros, group is an exception, and will reappear at several points in later chapters.

The functions in Figure 4.2 all work their way along the top-level structure of a list. Figure 4.3 shows two examples of functions that descend into nested lists. The first, flatten, was also predefined in Interlisp. It returns a list of all the atoms that are elements of a list, or elements of its elements, and so on:

```lisp
> (flatten '(a (b c) ((d e) f)))
(A B C D E F)
```

The other function in Figure 4.3, prune, is to remove-if as copy-tree is to copy-list. That is, it recurses down into sublists:

```lisp
> (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))
(1 (3 (5)) 7 (9))
```

Every leaf for which the function returns true is removed.

```lisp
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))
```

Figure 4.3: Doubly-recursive list utilities.

#heading(level: 2)[Search]

This section gives some examples of functions for searching lists. Common Lisp provides a rich set of built-in operators for this purpose, but some tasks are still difficult—or at least difficult to perform efficiently. We saw this in the hypothetical case described on page 41. The first utility in Figure 4.4, find2, is the one we defined in response to it.

The next utility, before, is written with similar intentions. It tells you if one object is found before another in a list:

```lisp
> (before 'b 'd '(a b c d))
(B C D)
```

It is easy enough to do this sloppily in raw Lisp:

```lisp
(< (position 'b '(a b c d)) (position 'd '(a b c d)))
```

But the latter idiom is inefficient and error-prone: inefficient because we don't need to find both objects, only the one that occurs first; and error-prone because if either object isn't in the list, nil will be passed as an argument to <. Using before fixes both problems.

Since before is similar in spirit to a test for membership, it is written to resemble the built-in member function. Like member it takes an optional test argument, which defaults to eql. Also, instead of simply returning t, it tries to return potentially useful information: the cdr beginning with the object given as the first argument.

```lisp
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))
```

Figure 4.4: Functions which search lists.

Note that before returns true if we encounter the first argument before encountering the second. Thus it will return true if the second argument doesn't occur in the list at all:

```lisp
> (before 'a 'b '(a))
(A)
```

We can peform a more exacting test by calling after, which requires that both its arguments occur in the list:

```lisp
> (after 'a 'b '(b a d))
(A D)
> (after 'a 'b '(a))
NIL
```

If (member o l) finds o in the list l, it also returns the cdr of l beginning with o. This return value can be used, for example, to test for duplication. If o is duplicated in l, then it will also be found in the cdr of the list returned by member. This idiom is embodied in the next utility, duplicate:

```lisp
> (duplicate 'a '(a b c a d))
(A D)
```

Other utilities to test for duplication could be written on the same principle.

More fastidious language designers are shocked that Common Lisp uses nil to represent both falsity and the empty list. It does cause trouble sometimes (see Section 14.2), but it is convenient in functions like duplicate. In questions of sequence membership, it seems natural to represent falsity as the empty sequence.

The last function in Figure 4.4 is also a kind of generalization of member. While member returns the cdr of the list beginning with the element it finds, split-if returns both halves. This utility is mainly used with lists that are ordered in some respect:

```lisp
> (split-if #'(lambda (x) (> x 4))
            '(1 2 3 4 5 6 7 8 9 10))
(1 2 3 4)
(5 6 7 8 9 10)
```

```lisp
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))
```

Figure 4.5: Search functions which compare elements.

Figure 4.5 contains search functions of another kind: those which compare elements against one another. The first, most, looks at one element at a time. It takes a list and a scoring function, and returns the element with the highest score. In case of ties, the element occurring first wins.

```lisp
> (most #'length '((a b) (a b c) (a) (e f g)))
(A B C)
3
```

For convenience, most also returns the score of the winner.

A more general kind of search is provided by best. This utility also takes a function and a list, but here the function must be a predicate of two arguments. It returns the element which, according to the predicate, beats all the others.

```lisp
> (best #'> '(1 2 3 4 5))
5
```

We can think of best as being equivalent to car of sort, but much more efficient. It is up to the caller to provide a predicate which defines a total order on the elements of the list. Otherwise the order of the elements will influence the result; as before, in case of ties, the first element wins.

Finally, mostn takes a function and a list and returns a list of all the elements for which the function yields the highest score (along with the score itself):

```lisp
> (mostn #'length '((a b) (a b c) (a) (e f g)))
((A B C) (E F G))
3
```

#heading(level: 2)[Mapping]

Another widely used class of Lisp functions are the mapping functions, which apply a function to a sequence of arguments. Figure 4.6 shows some examples of new mapping functions. The first three are for applying a function to a range of numbers without having to cons up a list to contain them. The first two, map0-n and map1-n, work for ranges of positive integers:

```lisp
> (map0-n #'1+ 5)
(1 2 3 4 5 6)
```

Both are written using the more general mapa-b, which works for any range of numbers:

```lisp
> (mapa-b #'1+ -2 0 .5)
(-1 -0.5 0.0 0.5 1.0)
```

Following mapa-b is the still more general map->, which works for sequences of objects of any kind. The sequence begins with the object given as the second argument, the end of the sequence is defined by the function given as the third argument, and successors are generated by the function given as the fourth argument. With map-> it is possible to navigate arbitrary data structures, as well as operate on sequences of numbers. We could define mapa-b in terms of map-> as follows:

```lisp
(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
         a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))
```

```lisp
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
```

Figure 4.6: Mapping functions.

For efficiency, the built-in mapcan is destructive. It could be duplicated by:

```lisp
(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))
```

Because mapcan splices together lists with nconc, the lists returned by the first argument had better be newly created, or the next time we look at them they might be altered. That's why nicknames (page 41) was defined as a function which "builds a list" of nicknames. If it simply returned a list stored elsewhere, it wouldn't have been safe to use mapcan. Instead we would have had to splice the returned lists with append. For such cases, mappend offers a nondestructive alternative to mapcan.

The next utility, mapcars, is for cases where we want to mapcar a function over several lists. If we have two lists of numbers and we want to get a single list of the square roots of both, using raw Lisp we could say

```lisp
(mapcar #'sqrt (append list1 list2))
```

but this conses unnecessarily. We append together list1 and list2 only to discard the result immediately. With mapcars we can get the same result from:

```lisp
(mapcars #'sqrt list1 list2)
```

and do no unnecessary consing.

The final function in Figure 4.6 is a version of mapcar for trees. Its name, rmapcar, is short for "recursive mapcar," and what mapcar does on flat lists, it does on trees:

```lisp
> (rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))
123456789
(1 2 (3 4 (5) 6) 7 (8 9))
```

Like mapcar, it can take more than one list argument.

```lisp
> (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))
(11 (22 (33) 44))
```

Several of the functions which appear later on ought really to call rmapcar, including rep on page 324.

To some extent, traditional list mapping functions may be rendered obsolete by the new series macros introduced in CLTL2. For example,

```lisp
(mapa-b #'fn a b c)
```

could be rendered

```lisp
(collect (#Mfn (scan-range :from a :upto b :by c)))
```

However, there is still some call for mapping functions. A mapping function may in some cases be clearer or more elegant. Some things we could express with map-> might be difficult to express using series. Finally, mapping functions, as functions, can be passed as arguments.

#heading(level: 2)[I/O]

```lisp
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))
```

Figure 4.7: I/O functions.

Figure 4.7 contains three examples of I/O utilities. The need for this kind of utility varies from program to program. Those in Figure 4.7 are just a representative sample. The first is for the case where you want users to be able to type in expressions without parentheses; it reads a line of input and returns it as a list:

```lisp
> (readlist)
Call me "Ed"
(CALL ME "Ed")
```

The call to values ensures that we get only one value back (read-from-string itself returns a second value that is irrelevant in this case).

The function prompt combines printing a question and reading the answer. It takes the arguments of format, except the initial stream argument.

```lisp
> (prompt "Enter a number between ~A and ~A.~%>> " 1 10)
Enter a number between 1 and 10.
>> 3
3
```

Finally, break-loop is for situations where you want to imitate the Lisp toplevel. It takes two functions and an &rest argument, which is repeatedly given to prompt. As long as the second function returns false for the input, the first function is applied to it. So for example we could simulate the actual Lisp toplevel with:

```lisp
> (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
Entering break-loop.
>> (+ 2 3)
5
>> :q
:Q
```

This, by the way, is the reason Common Lisp vendors generally insist on runtime licenses. If you can call eval at runtime, then any Lisp program can include Lisp.

#heading(level: 2)[Symbols and Strings]

Symbols and strings are closely related. By means of printing and reading functions we can go back and forth between the two representations. Figure 4.8 contains examples of utilities which operate on this border. The first, mkstr, takes any number of arguments and concatenates their printed representations into a string:

```lisp
> (mkstr pi " pieces of " 'pi)
"3.141592653589793 pieces of PI"
```

Built upon it is symb, which is mostly used for building symbols. It takes one or more arguments and returns the symbol (creating one if necessary) whose printname is their concatenation. It can take as an argument any object which has a printable representation: symbols, strings, numbers, even lists.

```lisp
> (symb 'ar "Madi" #\L #\L 0)
|ARMadiLL0|
```

```lisp
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))
```

Figure 4.8: Functions which operate on symbols and strings.

After calling mkstr to concatenate all its arguments into a single string, symb sends the string to intern. This function is Lisp's traditional symbol-builder: it takes a string and either finds the symbol which prints as the string, or makes a new one which does.

Any string can be the print-name of a symbol, even a string containing lowercase letters or macro characters like parentheses. When a symbol's name contains such oddities, it is printed within vertical bars, as above. In source code, such symbols should either be enclosed in vertical bars, or the offending characters preceded by backslashes:

```lisp
> (let ((s (symb '(a b))))
    (and (eq s '|(A B)|) (eq s '\(A\ B\))))
T
```

The next function, reread, is a generalization of symb. It takes a series of objects, and prints and rereads them. It can return symbols like symb, but it can also return anything else that read can. Read-macros will be invoked instead of being treated as part of a symbol's name, and a:b will be read as the symbol b in package a, instead of the symbol |a:b| in the current package. The more general function is also pickier: reread will generate an error if its arguments are not proper Lisp syntax.

The last function in Figure 4.8 was predefined in several earlier dialects: explode takes a symbol and returns a list of symbols made from the characters in its name.

```lisp
> (explode 'bomb)
(B O M B)
```

It is no accident that this function wasn't included in Common Lisp. If you find yourself wanting to take apart symbols, you're probably doing something inefficient. However, there is a place for this kind of utility in prototypes, if not in production software.

#heading(level: 2)[Density]

If your code uses a lot of new utilities, some readers may complain that it is hard to understand. People who are not yet very fluent in Lisp will only be used to reading raw Lisp. In fact, they may not be used to the idea of an extensible language at all. When they look at a program which depends heavily on utilities, it may seem to them that the author has, out of pure eccentricity, decided to write the program in some sort of private language.

All these new operators, it might be argued, make the program harder to read. One has to understand them all before being able to read the program. To see why this kind of statement is mistaken, consider the case described on page 41, in which we want to find the nearest bookshops. If you wrote the program using find2, someone could complain that they had to understand the definition of this new utility before they could read your program. Well, suppose you hadn't used find2. Then, instead of having to understand the definition of find2, the reader would have had to understand the definition of find-books, in which the function of find2 is mixed up with the specific task of finding bookshops. It is no more difficult to understand find2 than find-books. And here we have only used the new utility once. Utilities are meant to be used repeatedly. In a real program, it might be a choice between having to understand find2, and having to understand three or four specialized search routines. Surely the former is easier.

So yes, reading a bottom-up program requires one to understand all the new operators defined by the author. But this will nearly always be less work than having to understand all the code that would have been required without them.

If people complain that using utilities makes your code hard to read, they probably don't realize what the code would look like if you hadn't used them. Bottom-up programming makes what would otherwise be a large program look like a small, simple one. This can give the impression that the program doesn't do much, and should therefore be easy to read. When inexperienced readers look closer and find that this isn't so, they react with dismay.

We find the same phenomenon in other fields: a well-designed machine may have fewer parts, and yet look more complicated, because it is packed into a smaller space. Bottom-up programs are conceptually denser. It may take an effort to read them, but not as much as it would take if they hadn't been written that way.

There is one case in which you might deliberately avoid using utilities: if you had to write a small program to be distributed independently of the rest of your code. A utility usually pays for itself after two or three uses, but in a small program, a utility might not be used enough to justify including it.

#heading(level: 1)[Returning Functions]

The previous chapter showed how the ability to pass functions as arguments leads to greater possibilities for abstraction. The more we can do to functions, the more we can take advantage of these possibilities. By defining functions to build and return new functions, we can magnify the effect of utilities which take functions as arguments.

The utilities in this chapter operate on functions. It would be more natural, at least in Common Lisp, to write many of them to operate on expressions—that is, as macros. A layer of macros will be superimposed on some of these operators in Chapter 15. However, it is important to know what part of the task can be done with functions, even if we will eventually call these functions only through macros.

#heading(level: 2)[Common Lisp Evolves]

Common Lisp originally provided several pairs of complementary functions. The functions remove-if and remove-if-not make one such pair. If pred is a predicate of one argument, then

```lisp
(remove-if-not #'pred lst)
```

is equivalent to

```lisp
(remove-if #'(lambda (x) (not (pred x))) lst)
```

By varying the function given as an argument to one, we can duplicate the effect of the other. In that case, why have both? CLTL2 includes a new function intended for cases like this: complement takes a predicate p and returns a function which always returns the opposite value. When p returns true, the complement returns false, and vice versa. Now we can replace

```lisp
(remove-if-not #'pred lst)
```

with the equivalent

```lisp
(remove-if (complement #'pred) lst)
```

With complement, there is little justification for continuing to use the -if-not functions. Indeed, CLTL2 (p. 391) says that their use is now deprecated. If they remain in Common Lisp, it will only be for the sake of compatibility.

The new complement operator is the tip of an important iceberg: functions which return functions. This has long been an important part of the idiom of Scheme. Scheme was the first Lisp to make functions lexical closures, and it is this which makes it interesting to have functions as return values.

It's not that we couldn't return functions in a dynamically scoped Lisp. The following function would work the same under dynamic or lexical scope:

```lisp
(defun joiner (obj)
  (typecase obj
    (cons   #'append)
    (number #'+)))
```

It takes an object and, depending on its type, returns a function to add such objects together. We could use it to define a polymorphic join function that worked for numbers or lists:

```lisp
(defun join (&rest args)
  (apply (joiner (car args)) args))
```

However, returning constant functions is the limit of what we can do with dynamic scope. What we can't do (well) is build functions at runtime; joiner can return one of two functions, but the two choices are fixed.

On page 18 we saw another function for returning functions, which relied on lexical scope:

```lisp
(defun make-adder (n)
  #'(lambda (x) (+ x n)))
```

Calling make-adder will yield a closure whose behavior depends on the value originally given as an argument:

```lisp
> (setq add3 (make-adder 3))
#\<Interpreted-Function BF1356\>
> (funcall add3 2)
5
```

Under lexical scope, instead of merely choosing among a group of constant functions, we can build new closures at runtime. With dynamic scope this technique is impossible. If we consider how complement would be written, we see that it too must return a closure:

```lisp
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))
```

The function returned by complement uses the value of the parameter fn when complement was called. So instead of just choosing from a group of constant functions, complement can custom-build the inverse of any function:

```lisp
> (remove-if (complement #'oddp) '(1 2 3 4 5 6))
(1 3 5)
```

Being able to pass functions as arguments is a powerful tool for abstraction. The ability to write functions which return functions allows us to make the most of it. The remaining sections present several examples of utilities which return functions.

#heading(level: 2)[Orthogonality]

An orthogonal language is one in which you can express a lot by combining a small number of operators in a lot of different ways. Toy blocks are very orthogonal; a plastic model kit is hardly orthogonal at all. The main advantage of complement is that it makes a language more orthogonal. Before complement, Common Lisp had pairs of functions like remove-if and remove-if-not, subst-if and subst-if-not, and so on. With complement we can do without half of them.

The setf macro also improves Lisp's orthogonality. Earlier dialects of Lisp would often have pairs of functions for reading and writing data. With property-lists, for example, there would be one function to establish properties and another function to ask about them. In Common Lisp, we have only the latter, get. To establish a property, we use get in combination with setf:

```lisp
(setf (get 'ball 'color) 'red)
```

We may not be able to make Common Lisp smaller, but we can do something almost as good: use a smaller subset of it. Can we define any new operators which would, like complement and setf, help us toward this goal? There is at least one other way in which functions are grouped in pairs. Many functions also come in a destructive version: remove-if and delete-if, reverse and nreverse, append and nconc. By defining an operator to return the destructive counterpart of a function, we would not have to refer to the destructive functions directly.

```lisp
(defvar *!equivs* (make-hash-table))
(defun ! (fn)
  (or (gethash fn *!equivs*) fn))
(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))
```

Figure 5.1: Returning destructive equivalents.

Figure 5.1 contains code to support the notion of destructive counterparts. The global hash-table *!equivs* maps functions to their destructive equivalents; ! returns destructive equivalents; and def! sets them. The name of the ! (bang) operator comes from the Scheme convention of appending ! to the names of functions with side-effects. Now once we have defined

```lisp
(def! #'remove-if #'delete-if)
```

then instead of

```lisp
(delete-if #'oddp lst)
```

we would say

```lisp
(funcall (! #'remove-if) #'oddp lst)
```

Here the awkwardness of Common Lisp masks the basic elegance of the idea, which would be more visible in Scheme:

```lisp
((! remove-if) oddp lst)
```

As well as greater orthogonality, the ! operator brings a couple of other benefits. It makes programs clearer, because we can see immediately that (! \#'foo) is the destructive equivalent of foo. Also, it gives destructive operations a distinct, recognizable form in source code, which is good because they should receive special attention when we are searching for a bug.

Since the relation between a function and its destructive counterpart will usually be known before runtime, it would be most efficient to define ! as a macro, or even provide a read macro for it.

#heading(level: 2)[Memoizing]

If some function is expensive to compute, and we expect sometimes to make the same call more than once, then it pays to memoize: to cache the return values of all the previous calls, and each time the function is about to be called, to look first in the cache to see if the value is already known.

```lisp
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))
```

Figure 5.2: Memoizing utility.

Figure 5.2 contains a generalized memoizing utility. We give a function to memoize, and it returns an equivalent memoized version—a closure containing a hash-table in which to store the results of previous calls.

```lisp
> (setq slowid (memoize #'(lambda (x) (sleep 5) x)))
#\<Interpreted-Function C38346\>
> (time (funcall slowid 1))
Elapsed Time = 5.15 seconds
1
> (time (funcall slowid 1))
Elapsed Time = 0.00 seconds
1
```

With a memoized function, repeated calls are just hash-table lookups. There is of course the additional expense of a lookup on each initial call, but since we would only memoize a function that was sufficiently expensive to compute, it's reasonable to assume that this cost is insignificant in comparison.

Though adequate for most uses, this implementation of memoize has several limitations. It treats calls as identical if they have equal argument lists; this could be too strict if the function had keyword parameters. Also, it is intended only for single-valued functions, and cannot store or return multiple values.

#heading(level: 2)[Composing Functions]

The complement of a function f is denoted ~f. Section 5.1 showed that closures make it possible to define ~ as a Lisp function. Another common operation on functions is composition, denoted by the operator ∘. If f and g are functions, then f∘g is also a function, and f∘g(x) = f(g(x)). Closures also make it possible to define ∘ as a Lisp function.

```lisp
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))
```

Figure 5.3: An operator for functional composition.

Figure 5.3 defines a compose function which takes any number of functions and returns their composition. For example

```lisp
(compose #'list #'1+)
```

returns a function equivalent to

```lisp
#'(lambda (x) (list (1+ x)))
```

All the functions given as arguments to compose must be functions of one argument, except the last. On the last function there are no restrictions, and whatever arguments it takes, so will the function returned by compose:

```lisp
> (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
4
```

Since not is a Lisp function, complement is a special case of compose. It could be defined as:

```lisp
(defun complement (pred)
  (compose #'not pred))
```

We can combine functions in other ways than by composing them. For example, we often see expressions like

```lisp
(mapcar #'(lambda (x)
            (if (slave x)
                (owner x)
                (employer x)))
        people)
```

We could define an operator to build functions like this one automatically. Using fif from Figure 5.4, we could get the same effect with:

```lisp
(mapcar (fif #'slave #'owner #'employer)
        people)
```

```lisp
(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))
```

Figure 5.4: More function builders.

Figure 5.4 contains several other constructors for commonly occurring types of functions. The second, fint, is for cases like this:

```lisp
(find-if #'(lambda (x)
             (and (signed x) (sealed x) (delivered x)))
         docs)
```

The predicate given as the second argument to find-if defines the intersection of the three predicates called within it. With fint, whose name stands for "function intersection," we can say:

```lisp
(find-if (fint #'signed #'sealed #'delivered) docs)
```

We can define a similar operator to return the union of a set of predicates. The function fun is like fint but uses or instead of and.

#heading(level: 2)[Recursion on Cdrs]

Recursive functions are so important in Lisp programs that it would be worth having utilities to build them. This section and the next describe functions which build the two most common types. In Common Lisp, these functions are a little awkward to use. Once we get into the subject of macros, we will see how to put a more elegant facade on this machinery. Macros for building recursers are discussed in Sections 15.2 and 15.3.

Repeated patterns in a program are a sign that it could have been written at a higher level of abstraction. What pattern is more commonly seen in Lisp programs than a function like this:

```lisp
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))
```

or this:

```lisp
(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))
```

Structurally these two functions have a lot in common. They both operate recursively on successive cdrs of a list, evaluating the same expression on each step, except in the base case, where they return a distinct value. This pattern appears so frequently in Lisp programs that experienced programmers can read and reproduce it without stopping to think. Indeed, the lesson is so quickly learned, that the question of how to package the pattern in a new abstraction does not arise.

However, a pattern it is, all the same. Instead of writing these functions out by hand, we should be able to write a function which will generate them for us. Figure 5.5 contains a function-builder called lrec ("list recurser") which should be able to generate most functions that recurse on successive cdrs of a list.

```lisp
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))
```

Figure 5.5: Function to define flat list recursers.

The first argument to lrec must be a function of two arguments: the current car of the list, and a function which can be called to continue the recursion. Using lrec we could express our-length as:

```lisp
(lrec #'(lambda (x f) (1+ (funcall f))) 0)
```

To find the length of the list, we don't need to look at the elements, or stop partway, so the object x is always ignored, and the function f always called. However, we need to take advantage of both possibilities to express our-every, for e.g. oddp:

```lisp
(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
```

The definition of lrec uses labels to build a local recursive function called self. In the recursive case the function rec is passed two arguments, the current car of the list, and a function embodying the recursive call. In functions like our-every, where the recursive case is an and, if the first argument returns false we want to stop right there. Which means that the argument passed in the recursive case must not be a value but a function, which we can call (if we want) in order to get a value.

```lisp
; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))
```

Figure 5.6: Functions expressed with lrec.

Figure 5.6 shows some existing Common Lisp functions defined with lrec. Calling lrec will not always yield the most efficient implementation of a given function. Indeed, lrec and the other recurser generators to be defined in this chapter tend to lead one away from tail-recursive solutions. For this reason they are best suited for use in initial versions of a program, or in parts where speed is not critical.

#heading(level: 2)[Recursion on Subtrees]

There is another recursive pattern commonly found in Lisp programs: recursion on subtrees. This pattern is seen in cases where you begin with a possibly nested list, and want to recurse down both its car and its cdr.

The Lisp list is a versatile structure. Lists can represent, among other things, sequences, sets, mappings, arrays, and trees. There are several different ways to interpret a list as a tree. The most common is to regard the list as a binary tree whose left branch is the car and whose right branch is the cdr. (In fact, this is usually the internal representation of lists.) Figure 5.7 shows three examples of lists and the trees they represent. Each internal node in such a tree corresponds to a dot in the dotted-pair representation of the list, so the tree structure may be easier to interpret if the lists are considered in that form:

```
(a b c)     =  (a . (b . (c . nil)))
(a b (c d)) =  (a . (b . ((c . (d . nil)) . nil)))
```

[THIS IS FIGURE: A diagram showing three tree structures for lists (a . b), (a b c), and (a b (c d))]

Figure 5.7: Lists as trees.

Any list can be interpreted as a binary tree. Hence the distinction between pairs of Common Lisp functions like copy-list and copy-tree. The former copies a list as a sequence—if the list contains sublists, the sublists, being mere elements in the sequence, are not copied:

```lisp
> (setq x '(a b)
        listx (list x 1))
((A B) 1)
> (eq x (car (copy-list listx)))
T
```

In contrast, copy-tree copies a list as a tree—sublists are subtrees, and so must also be copied:

```lisp
> (eq x (car (copy-tree listx)))
NIL
```

We could define a version of copy-tree as follows:

```lisp
(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (if (cdr tree) (our-copy-tree (cdr tree))))))
```

This definition turns out to be one instance of a common pattern. (Some of the following functions are written a little oddly in order to make the pattern obvious.) Consider for example a utility to count the number of leaves in a tree:

```lisp
(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (or (if (cdr tree) (count-leaves (cdr tree)))
             1))))
```

A tree has more leaves than the atoms you can see when it is represented as a list:

```lisp
> (count-leaves '((a b (c d)) (e) f))
10
```

The leaves of a tree are all the atoms you can see when you look at the tree in its dotted-pair representation. In dotted-pair notation, ((a b (c d)) (e) f) would have four nils that aren't visible in the list representation (one for each pair of parentheses) so count-leaves returns 10.

In the last chapter we defined several utilities which operate on trees. For example, flatten (page 47) takes a tree and returns a list of all the atoms in it. That is, if you give flatten a nested list, you'll get back a list that looks the same except that it's missing all but the outermost pair of parentheses:

```lisp
> (flatten '((a b (c d)) (e) f ()))
(A B C D E F)
```

This function could also be defined (somewhat inefficiently) as follows:

```lisp
(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (flatten (cdr tree))))))
```

Finally, consider rfind-if, a recursive version of find-if which works on trees as well as flat lists:

```lisp
(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))
```

To generalize find-if for trees, we have to decide whether we want to search for just leaves, or for whole subtrees. Our rfind-if takes the former approach, so the caller can assume that the function given as the first argument will only be called on atoms:

```lisp
> (rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5))
3
```

How similar in form are these four functions, copy-tree, count-leaves, flatten, and rfind-if. Indeed, they're all instances of an archetypal function for recursion on subtrees. As with recursion on cdrs, we need not leave this archetype to float vaguely in the background—we can write a function to generate instances of it.

To get at the archetype itself, let's look at these functions and see what's not pattern. Essentially our-copy-tree is two facts:

1. In the base case it returns its argument.
2. In the recursive case, it applies cons to the recursions down the left (car) and right (cdr) subtrees.

We should thus be able to express it as a call to a builder with two arguments:

```lisp
(ttrav #'cons #'identity)
```

A definition of ttrav ("tree traverser") is shown in Figure 5.8. Instead of passing one value in the recursive case, we pass two, one for the left subtree and one for the right. If the base argument is a function it will be called on the current leaf. In flat list recursion, the base case is always nil, but in tree recursion the base case could be an interesting value, and we might want to use it.

```lisp
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))
```

Figure 5.8: Function for recursion on trees.

With ttrav we could express all the preceding functions except rfind-if. (They are shown in Figure 5.9.) To define rfind-if we need a more general tree recursion builder which gives us control over when, and if, the recursive calls are made. As the first argument to ttrav we gave a function which took the results of the recursive calls. For the general case, we want to use instead a function which takes two closures representing the calls themselves. Then we can write recursers which only traverse as much of the tree as they want to.

```lisp
; our-copy-tree
(ttrav #'cons)

; count-leaves
(ttrav #'(lambda (l r) (+ l (or r 1))) 1)

; flatten
(ttrav #'nconc #'mklist)
```

Figure 5.9: Functions expressed with ttrav.

Functions built by ttrav always traverse a whole tree. That's fine for functions like count-leaves or flatten, which have to traverse the whole tree anyway. But we want rfind-if to stop searching as soon as it finds what it's looking for.

It must be built by the more general trec, shown in Figure 5.10. The second arg to trec should be a function of three arguments: the current object and the two recursers. The latter two will be closures representing the recursions down the left and right subtrees. With trec we would define flatten as:

```lisp
(trec #'(lambda (o l r) (nconc (funcall l) (funcall r)))
      #'mklist)
```

Now we can also express rfind-if for e.g. oddp as:

```lisp
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree)))
```

```lisp
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
                 base)
             (funcall rec tree
                      #'(lambda ()
                          (self (car tree)))
                      #'(lambda ()
                          (if (cdr tree)
                              (self (cdr tree))))))))
    #'self))
```

Figure 5.10: Function for recursion on trees.

#heading(level: 2)[When to Build Functions]

Expressing functions by calls to constructors instead of sharp-quoted lambda-expressions could, unfortunately, entail unnecessary work at runtime. A sharp-quoted lambda-expression is a constant, but a call to a constructor function will be evaluated at runtime. If we really have to make this call at runtime, it might not be worth using constructor functions. However, at least some of the time we can call the constructor beforehand. By using \#., the sharp-dot read macro, we can have the new functions built at read-time. So long as compose and its arguments are defined when this expression is read, we could say, for example,

```lisp
(find-if #.(compose #'oddp #'truncate) lst)
```

Then the call to compose would be evaluated by the reader, and the resulting function inserted as a constant into our code. Since both oddp and truncate are built-in, it would safe to assume that we can evaluate the compose at read-time, so long as compose itself were already loaded.

In general, composing and combining functions is more easily and efficiently done with macros. This is particularly true in Common Lisp, with its separate name-space for functions. After introducing macros, we will in Chapter 15 cover much of the ground we covered here, but in a more luxurious vehicle.

#heading(level: 1)[Functions as Representation]

Generally, data structures are used to represent. An array could represent a geometric transformation; a tree could represent a hierarchy of command; a graph could represent a rail network. In Lisp we can sometimes use closures as a representation. Within a closure, variable bindings can store information, and can also play the role that pointers play in constructing complex data structures. By making a group of closures which share bindings, or can refer to one another, we can create hybrid objects which combine the advantages of data structures and programs.

Beneath the surface, shared bindings are pointers. Closures just bring us the convenience of dealing with them at a higher level of abstraction. By using closures to represent something we would otherwise represent with static data structures, we can often expect substantial improvements in elegance and efficiency.

#heading(level: 2)[Networks]

Closures have three useful properties: they are active, they have local state, and we can make multiple instances of them. Where could we use multiple copies of active objects with local state? In applications involving networks, among others. In many cases we can represent nodes in a network as closures. As well as having its own local state, a closure can refer to another closure. Thus a closure representing a node in a network can know of several other nodes (closures) to which it must send its output. This means that we may be able to translate some networks straight into code.

```
> (run-node 'people)
Is the person a man?
>> yes
Is he living?
>> no
Was he American?
>> yes
Is he on a coin?
>> yes
Is the coin a penny?
>> yes
LINCOLN
```

Figure 6.1: Session of twenty questions.

In this section and the next we will look at two ways to traverse a network. First we will follow the traditional approach, with nodes defined as structures, and separate code to traverse the network. Then in the next section we'll show how to build the same program from a single abstraction.

As an example, we will use about the simplest application possible: one of those programs that play twenty questions. Our network will be a binary tree. Each non-leaf node will contain a yes/no question, and depending on the answer to the question, the traversal will continue down the left or right subtree. Leaf nodes will contain return values. When the traversal reaches a leaf node, its value will be returned as the value of the traversal. A session with this program might look as in Figure 6.1.

The traditional way to begin would be to define some sort of data structure to represent nodes. A node is going to have to know several things: whether it is a leaf; if so, which value to return, and if not, which question to ask; and where to go depending on the answer. A sufficient data structure is defined in Figure 6.2. It is designed for minimal size. The contents field will contain either a question or a return value. If the node is not a leaf, the yes and no fields will tell where to go depending on the answer to the question; if the node is a leaf, we will know it because these fields are empty. The global *nodes* will be a hash-table in which nodes are indexed by name. Finally, defnode makes a new node (of either type) and stores it in *nodes*. Using these materials we could define the first node of our tree:

```lisp
(defnode 'people "Is the person a man?"
         'male 'female)
```

```lisp
(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes     yes
                   :no      no)))
```

Figure 6.2: Representation and definition of nodes.

```lisp
(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)
```

Figure 6.3: Sample network.

Figure 6.3 shows as much of the network as we need to produce the transcript in Figure 6.1.

Now all we need to do is write a function to traverse this network, printing out the questions and following the indicated path. This function, run-node, is shown in Figure 6.4. Given a name, we look up the corresponding node. If it is not a leaf, the contents are asked as a question, and depending on the answer, we continue traversing at one of two possible destinations. If the node is a leaf, run-node just returns its contents. With the network defined in Figure 6.3, this function produces the output shown in Figure 6.1.

```lisp
(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t   (run-node (node-no n)))))
          (t (node-contents n)))))
```

Figure 6.4: Function for traversing networks.

#heading(level: 2)[Compiling Networks]

In the preceding section we wrote a network program as it might have been written in any language. Indeed, the program is so simple that it seems odd to think that we could write it any other way. But we can—in fact, we can write it much more simply.

The code in Figure 6.5 illustrates this point. It's all we really need to run our network. Instead of having nodes as data structures and a separate function to traverse them, we represent the nodes as closures. The data formerly contained in the structures gets stored in variable bindings within the closures. Now there is no need for run-node; it is implicit in the nodes themselves. To start the traversal, we just funcall the node at which we want to begin:

```lisp
(defvar *nodes* (make-hash-table))
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (format t "~A~%>> " conts)
                (case (read)
                  (yes (funcall (gethash yes *nodes*)))
                  (t   (funcall (gethash no *nodes*)))))
            #'(lambda () conts))))
```

Figure 6.5: A network compiled into closures.

```lisp
(funcall (gethash 'people *nodes*))
Is the person a man?
>>
```

From then on, the transcript will be just as it was with the previous implementation.

By representing the nodes as closures, we are able to transform our twenty-questions network entirely into code. As it is, the code will have to look up the node functions by name at runtime. However, if we know that the network is not going to be redefined on the fly, we can add a further enhancement: we can have node functions call their destinations directly, without having to go through a hash-table.

```lisp
(defvar *nodes* nil)
(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
        (let ((conts (second node))
              (yes (third node))
              (no (fourth node)))
          (if yes
              (let ((yes-fn (compile-net yes))
                    (no-fn (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " conts)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () conts))))))
```

Figure 6.6: Compilation with static references.

Figure 6.6 contains a new version of the program. Now *nodes* is a disposable list instead of a hash-table. All the nodes are defined with defnode as before, but no closures are generated at this point. After all the nodes have been defined, we call compile-net to compile a whole network at once. This function recursively works its way right down to the leaves of the tree, and on the way back up, returns at each step the node/function for each of the two subtrees. So now each node will have a direct handle on its two destinations, instead of having only their names. When the original call to compile-net returns, it will yield a function representing the portion of the network we asked to have compiled.

```lisp
> (setq n (compile-net 'people))
#\<Compiled-Function BF3C06\>
> (funcall n)
Is the person a man?
>>
```

Notice that compile-net compiles in both senses. It compiles in the general sense, by translating the abstract representation of the network into code. Moreover, if compile-net itself is compiled, it will return compiled functions. (See page 25.)

After compiling the network, we will no longer need the list made by defnode. It can be cut loose (e.g. by setting *nodes* to nil) and reclaimed by the garbage collector.

#heading(level: 2)[Looking Forward]

Many programs involving networks can be implemented by compiling the nodes into closures. Closures are data objects, and they can be used to represent things just as structures can. Doing so requires some unconventional thinking, but the rewards are faster and more elegant programs.

Macros help substantially when we use closures as a representation. "To represent with closures" is another way of saying "to compile," and since macros do their work at compile-time, they are a natural vehicle for this technique. After macros have been introduced, Chapters 23 and 24 will present much larger programs based on the strategy used here.

#heading(level: 1)[Macros]

Lisp's macro facility allows you to define operators that are implemented by transformation. The definition of a macro is essentially a function that generates Lisp code—a program that writes programs. From these small beginnings arise great possibilities, and also unexpected hazards. Chapters 7–10 form a tutorial on macros. This chapter explains how macros work, gives techniques for writing and testing them, and looks at the issue of macro style.

#heading(level: 2)[How Macros Work]

Since macros can be called and return values, they tend to be associated with functions. Macro definitions sometimes resemble function definitions, and speaking informally, people call do, which is actually a macro, a "built-in function." But pushing the analogy too far can be a source of confusion. Macros work differently from normal functions, and knowing how and why macros are different is the key to using them correctly. A function produces results, but a macro produces expressions—which, when evaluated, produce results.

The best way to begin is to move straight into an example. Suppose we want to write a macro nil!, which sets its argument to nil. We want (nil! x) to have the same effect as (setq x nil). We do it by defining nil! as a macro which turns instances of the first form into instances of the second.

```lisp
> (defmacro nil! (var)
    (list 'setq var nil))
NIL!
```

Paraphrased in English, this definition tells Lisp: "Whenever you see an expression of the form (nil! var), turn it into one of the form (setq var nil) before evaluating it."

The expression generated by the macro will be evaluated in place of the original macro call. A macro call is a list whose first element is the name of a macro. What happens when we type the macro call (nil! x) into the toplevel? Lisp notices that nil! is the name of a macro, and

1. builds the expression specified by the definition above, then
2. evaluates that expression in place of the original macro call.

The step of building the new expression is called macroexpansion. Lisp looks up the definition of nil!, which shows how to construct a replacement for the macro call. The definition of nil! is applied like a function to the expressions given as arguments in the macro call. It returns a list of three elements: setq, the expression given as the argument to the macro, and nil. In this case, the argument to nil! is x, and the macroexpansion is (setq x nil).

After macroexpansion comes a second step, evaluation. Lisp evaluates the macroexpansion (setq x nil) as if you had typed that in the first place. Evaluation does not always come immediately after expansion, as it does at the toplevel. A macro call occurring in the definition of a function will be expanded when the function is compiled, but the expansion—or the object code which results from it—won't be evaluated until the function is called.

Many of the difficulties you might encounter with macros can be avoided by maintaining a sharp distinction between macroexpansion and evaluation. When writing macros, know which computations are performed during macroexpansion, and which during evaluation, for the two steps generally operate on objects of two different sorts. The macroexpansion step deals with expressions, and the evaluation step deals with their values.

Sometimes macroexpansion can be more complicated than it was in the case of nil!. The expansion of nil! was a call to a built-in special form, but sometimes the expansion of a macro will be yet another macro call, like a Russian doll which contains another doll inside it. In such cases, macroexpansion simply continues until it arrives at an expression which is no longer a macro call. The process can take arbitrarily many steps, so long as it terminates eventually.

Many languages offer some form of macro, but Lisp macros are singularly powerful. When a file of Lisp is compiled, a parser reads the source code and sends its output to the compiler. Here's the stroke of genius: the output of the parser consists of lists of Lisp objects. With macros, we can manipulate the program while it's in this intermediate form between parser and compiler. If necessary, these manipulations can be very extensive. A macro generating its expansion has at its disposition the full power of Lisp. Indeed, a macro is really a Lisp function—one which happens to return expressions. The definition of nil! contains a single call to list, but another macro might invoke a whole subprogram to generate its expansion.

Being able to change what the compiler sees is almost like being able to rewrite it. We can add any construct to the language that we can define by transformation into existing constructs.

#heading(level: 2)[Backquote]

Backquote is a special version of quote which can be used to create templates for Lisp expressions. One of the most common uses of backquote is in macro definitions.

The backquote character, \`, is so named because it resembles a regular quote, ', reversed. When backquote alone is affixed to an expression, it behaves just like quote:

`` `(a b c) is equal to '(a b c).``

Backquote becomes useful only when it appears in combination with comma, `,`, and comma-at, `,@`. If backquote makes a template, comma makes a slot within a template. A backquoted list is equivalent to a call to list with the elements quoted. That is,

`` `(a b c) is equal to (list 'a 'b 'c).``

Within the scope of a backquote, a comma tells Lisp: "turn off the quoting." When a comma appears before one of the elements of the list, it has the effect of cancelling out the quote that would have been put there. So

`` `(a ,b c ,d) is equal to (list 'a b 'c d).``

Instead of the symbol b, its value is inserted into the resulting list. Commas work no matter how deeply they appear within a nested list,

```lisp
> (setq a 1 b 2 c 3)
3
> `(a ,b c)
(A 2 C)
> `(a (,b c))
(A (2 C))
```

and they may even appear within quotes, or within quoted sublists:

```lisp
> `(a b ,c (',(+ a b c)) (+ a b) 'c '((,a ,b)))
(A B 3 ('6) (+ A B) 'C '((1 2)))
```

One comma counteracts the effect of one backquote, so commas must match backquotes. Say that a comma is surrounded by a particular operator if the operator is prepended to the comma, or prepended to an expression which contains it. In `` `(,a ,(b ',c))), `` for example, the last comma is surrounded by one comma and two backquotes. The general rule is: a comma surrounded by n commas must be surrounded by at least n+1 backquotes. An obvious corollary is that commas may not appear outside of a backquoted expression. Backquotes and commas can be nested, so long as they obey the rule above. Any of the following expressions would generate an error if typed into the toplevel:

```
,x    `(a ,,b c)    `(a ,(b ,c) d)    `(,,'a)
```

Nested backquotes are only likely to be needed in macro-defining macros. Both topics are discussed in Chapter 16.

Backquote is usually used for making lists. Any list generated by backquote can also be generated by using list and regular quotes. The advantage of backquote is just that it makes expressions easier to read, because a backquoted expression resembles the expression it will produce. In the previous section we defined nil! as:

```lisp
(defmacro nil! (var)
  (list 'setq var nil))
```

With backquote the same macro can be defined as:

```lisp
(defmacro nil! (var)
  `(setq ,var nil))
```

which in this case is not all that different. The longer the macro definition, however, the more important it is to use backquote. Figure 7.1 contains two possible definitions of nif, a macro which does a three-way numeric if.

The first argument should evaluate to a number. Then the second, third, or fourth argument is evaluated, depending on whether the first was positive, zero, or negative:

```lisp
> (mapcar #'(lambda (x)
              (nif x 'p 'z 'n))
          '(0 2.5 -8))
(Z P N)
```

With backquote:
```lisp
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))
```

Without backquote:
```lisp
(defmacro nif (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expr))
        (list 1 pos)
        (list 0 zero)
        (list -1 neg)))
```

Figure 7.1: A macro defined with and without backquote.

The two definitions in Figure 7.1 define the same macro, but the first uses backquote, while the second builds its expansion by explicit calls to list. From the first definition it's easy to see that (nif x 'p 'z 'n), for example, expands into

```lisp
(case (truncate (signum x))
  (1 'p)
  (0 'z)
  (-1 'n))
```

because the body of the macro definition looks just like the expansion it generates. To understand the second version, without backquote, you have to trace in your head the building of the expansion.

Comma-at, `,@`, is a variant of comma. It behaves like comma, with one difference: instead of merely inserting the value of the expression to which it is affixed, as comma does, comma-at splices it. Splicing can be thought of as inserting while removing the outermost level of parentheses:

```lisp
> (setq b '(1 2 3))
(1 2 3)
> `(a ,b c)
(A (1 2 3) C)
> `(a ,@b c)
(A 1 2 3 C)
```

The comma causes the list (1 2 3) to be inserted in place of b, while the comma-at causes the elements of the list to be inserted there. There are some additional restrictions on the use of comma-at:

1. In order for its argument to be spliced, comma-at must occur within a sequence. It's an error to say something like ``,\@b`` because there is nowhere to splice the value of b.

2. The object to be spliced must be a list, unless it occurs last. The expression `` `(a ,@1) `` will evaluate to (a . 1), but attempting to splice an atom into the middle of a list, as in `` `(a ,\@1 b) ``, will cause an error.

Comma-at tends to be used in macros which take an indeterminate number of arguments and pass them on to functions or macros which also take an indeterminate number of arguments. This situation commonly arises when implementing implicit blocks. Common Lisp has several operators for grouping code into blocks, including block, tagbody, and progn. These operators rarely appear directly in source code; they are more often implicit—that is, hidden by macros.

An implicit block occurs in any built-in macro which can have a body of expressions. Both let and cond provide implicit progn, for example. The simplest built-in macro to do so is probably when:

```lisp
(when (eligible obj)
  (do-this)
  (do-that)
  obj)
```

If (eligible obj) returns true, the remaining expressions will be evaluated, and the when expression as a whole will return the value of the last. As an example of the use of comma-at, here is one possible definition for when:

```lisp
(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))
```

This definition uses an &body parameter (identical to &rest except for its effect on pretty-printing) to take in an arbitrary number of arguments, and a comma-at to splice them into a progn expression. In the macroexpansion of the call above, the three expressions in the body will appear within a single progn:

```lisp
(if (eligible obj)
    (progn (do-this)
           (do-that)
           obj))
```

Most macros for iteration splice their arguments in a similar way.

The effect of comma-at can be achieved without using backquote. The expression `` `(a ,@b c) `` is equal to (cons 'a (append b (list 'c))), for example. Comma-at exists only to make such expression-generating expressions more readable.

Macro definitions (usually) generate lists. Although macro expansions could be built with the function list, backquote list-templates make the task much easier. A macro defined with defmacro and backquote will superficially resemble a function defined with defun. So long as you are not misled by the similarity, backquote makes macro definitions both easier to write and easier to read.

Backquote is so often used in macro definitions that people sometimes think of backquote as part of defmacro. The last thing to remember about backquote is that it has a life of its own, separate from its role in macros. You can use backquote anywhere sequences need to be built:

```lisp
(defun greet (name)
  `(hello ,name))
```

#heading(level: 2)[Defining Simple Macros]

In programming, the best way to learn is often to begin experimenting as soon as possible. A full theoretical understanding can come later. Accordingly, this section presents a way to start writing macros immediately. It works only for a narrow range of cases, but where applicable it can be applied quite mechanically. (If you've written macros before, you may want to skip this section.)

As an example, we consider how to write a variant of the the built-in Common Lisp function member. By default member uses eql to test for equality. If you want to test for membership using eq, you have to say so explicitly:

```lisp
(member x choices :test #'eq)
```

If we did this a lot, we might want to write a variant of member which always used eq. Some earlier dialects of Lisp had such a function, called memq:

```lisp
(memq x choices)
```

Ordinarily one would define memq as an inline function, but for the sake of example we will reincarnate it as a macro.

call:      (memq x choices)
expansion: (member x choices :test \#'eq)

Figure 7.2: Diagram used in writing memq.

The method: Begin with a typical call to the macro you want to define. Write it down on a piece of paper, and below it write down the expression into which it ought to expand. Figure 7.2 shows two such expressions. From the macro call, construct the parameter list for your macro, making up some parameter name for each of the arguments. In this case there are two arguments, so we'll have two parameters, and call them obj and lst:

```lisp
(defmacro memq (obj lst)
```

Now go back to the two expressions you wrote down. For each argument in the macro call, draw a line connecting it with the place it appears in the expansion below. In Figure 7.2 there are two parallel lines. To write the body of the macro, turn your attention to the expansion. Start the body with a backquote. Now, begin reading the expansion expression by expression. Wherever you find a parenthesis that isn't part of an argument in the macro call, put one in the macro definition. So following the backquote will be a left parenthesis. For each expression in the expansion

1. If there is no line connecting it with the macro call, then write down the expression itself.

2. If there is a connection to one of the arguments in the macro call, write down the symbol which occurs in the corresponding position in the macro parameter list, preceded by a comma.

There is no connection to the first element, member, so we use member itself:

```lisp
(defmacro memq (obj lst)
  `(member
```

However, x has a line leading to the first argument in the source expression, so we use in the macro body the first parameter, with a comma:

```lisp
(defmacro memq (obj lst)
  `(member ,obj
```

Continuing in this way, the completed macro definition is:

```lisp
(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))
```

So far, we can only write macros which take a fixed number of arguments. Now suppose we want to write a macro while, which will take a test expression and some body of code, and loop through the code as long as the test expression returns true. Figure 7.3 contains an example of a while loop describing the behavior of a cat.

```
(while hungry
  (stare-intently)
  (meow)
  (rub-against-legs))

(do ()
    ((not hungry))
  (stare-intently)
  (meow)
  (rub-against-legs))
```

Figure 7.3: Diagram used in writing while.

To write such a macro, we have to modify our technique slightly. As before, begin by writing down a sample macro call. From that, build the parameter list of the macro, but where you want to take an indefinite number of arguments, conclude with an &rest or &body parameter:

```lisp
(defmacro while (test &body body)
```

Now write the desired expansion below the macro call, and as before draw lines connecting the arguments in the macro call to their position in the expansion. However, when you have a sequence of arguments which are going to be sucked into a single &rest or &body parameter, treat them as a group, drawing a single line for the whole sequence. Figure 7.3 shows the resulting diagram.

To write the body of the macro definition, proceed as before along the expansion. As well as the two previous rules, we need one more:

3. If there is a connection from a series of expressions in the expansion to a series of the arguments in the macro call, write down the corresponding &rest or &body parameter, preceded by a comma-at.

So the resulting macro definition will be:

```lisp
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))
```

To build a macro which can have a body of expressions, some parameter has to act as a funnel. Here multiple arguments in the macro call are joined together into body, and then broken up again when body is spliced into the expansion.

The approach described in this section enables us to write the simplest macros—those which merely shuffle their parameters. Macros can do a lot more than that. Section 7.7 will present examples where expansions can't be represented as simple backquoted lists, and to generate them, macros become programs in their own right.

#heading(level: 2)[Testing Macroexpansion]

Having written a macro, how do we test it? A macro like memq is simple enough that one can tell just by looking at it what it will do. When writing more complicated macros, we have to be able to check that they are being expanded correctly.

```lisp
> (defmacro while (test &body body)
    `(do ()
         ((not ,test))
       ,@body))
WHILE
> (pprint (macroexpand '(while (able) (laugh))))
(BLOCK NIL
  (LET NIL
    (TAGBODY
     #:G61
       (IF (NOT (ABLE)) (RETURN NIL))
       (LAUGH)
       (GO #:G61))))
T
> (pprint (macroexpand-1 '(while (able) (laugh))))
(DO NIL
    ((NOT (ABLE)))
  (LAUGH))
T
```

Figure 7.4: A macro and two depths of expansion.

Figure 7.4 shows a macro definition and two ways of looking at its expansion. The built-in function macroexpand takes an expression and returns its macroexpansion. Sending a macro call to macroexpand shows how the macro call will finally be expanded before being evaluated, but a complete expansion is not always what you want in order to test a macro. When the macro in question relies on other macros, they too will be expanded, so a complete macroexpansion can sometimes be difficult to read.

From the first expression shown in Figure 7.4, it's hard to tell whether or not while is expanding as intended, because the built-in do macro gets expanded, as well as the prog macro into which it expands. What we need is a way of seeing the result after only one step of expansion. This is the purpose of the built-in function macroexpand-1, shown in the second example; macroexpand-1 stops after just one step, even if the expansion is still a macro call.

When we want to look at the expansion of a macro call, it will be a nuisance always to have to type

```lisp
(pprint (macroexpand-1 '(or x y)))
```

```lisp
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))
```

Figure 7.5: A macro for testing macroexpansion.

Figure 7.5 defines a new macro which allows us to say instead:

```lisp
(mac (or x y))
```

Typically you debug functions by calling them, and macros by expanding them. But since a macro call involves two layers of computation, there are two points where things can go wrong. If a macro is misbehaving, most of the time you will be able to tell what's wrong just by looking at the expansion. Sometimes, though, the expansion will look fine and you'll want to evaluate it to see where the problems arise. If the expansion contains free variables, you may want to set some variables first. In some systems, you will be able to copy the expansion and paste it into the toplevel, or select it and choose eval from a menu. In the worst case you can set a variable to the list returned by macroexpand-1, then call eval on it:

```lisp
> (setq exp (macroexpand-1 '(memq 'a '(a b c))))
(MEMBER (QUOTE A) (QUOTE (A B C)) :TEST (FUNCTION EQ))
> (eval exp)
(A B C)
```

Finally, macroexpansion is more than an aid in debugging, it's also a way of learning how to write macros. Common Lisp has over a hundred macros built-in, some of them quite complex. By looking at the expansions of these macros you will often be able to see how they were written.

#heading(level: 2)[Destructuring in Parameter Lists]

Destructuring is a generalization of the sort of assignment done by function calls. If you define a function of several arguments

```lisp
(defun foo (x y z)
  (+ x y z))
```

then when the function is called

```lisp
(foo 1 2 3)
```

the parameters of the function are assigned arguments in the call according to their position: x to 1, y to 2, and z to 3. Destructuring describes the situation where this sort of positional assignment is done for arbitrary list structures, as well as flat lists like (x y z).

The Common Lisp destructuring-bind macro (new in CLTL2) takes a pattern, an argument evaluating to a list, and a body of expressions, and evaluates the expressions with the parameters in the pattern bound to the corresponding elements of the list:

```lisp
> (destructuring-bind (x (y) . z) '(a (b) c d)
    (list x y z))
(A B (C D))
```

This new operator and others like it form the subject of Chapter 18.

Destructuring is also possible in macro parameter lists. The Common Lisp defmacro allows parameter lists to be arbitrary list structures. When a macro call is expanded, components of the call will be assigned to the parameters as if by destructuring-bind. The built-in dolist macro takes advantage of such parameter list destructuring. In a call like:

```lisp
(dolist (x '(a b c))
  (print x))
```

the expansion function must pluck x and '(a b c) from within the list given as the first argument. That can be done implicitly by giving dolist the appropriate parameter list:

```lisp
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))
```

In Common Lisp, macros like dolist usually enclose within a list the arguments not part of the body. Because it takes an optional result argument, dolist must enclose its first arguments in a distinct list anyway. But even if the extra list structure were not necessary, it would make calls to dolist easier to read.

Suppose we want to define a macro when-bind, like when except that it binds some variable to the value returned by the test expression. This macro may be best implemented with a nested parameter list:

```lisp
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))
```

and called as follows:

```lisp
(when-bind (input (get-user-input))
  (process input))
```

instead of:

```lisp
(let ((input (get-user-input)))
  (when input
    (process input)))
```

Used sparingly, parameter list destructuring can result in clearer code. At a minimum, it can be used in macros like when-bind and dolist, which take two or more arguments followed by a body of expressions.

#heading(level: 2)[A Model of Macros]

A formal description of what macros do would be long and confusing. Experienced programmers do not carry such a description in their heads anyway. It's more convenient to remember what defmacro does by imagining how it would be defined.

There is a long tradition of such explanations in Lisp. The Lisp 1.5 Programmer's Manual, first published in 1962, gives for reference a definition of eval written in Lisp. Since defmacro is itself a macro, we can give it the same treatment, as in Figure 7.6. This definition uses several techniques which haven't been covered yet, so some readers may want to refer to it later.

```lisp
(defmacro our-expander (name) `(get ,name 'expander))

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,parms (cdr ,g)
                     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))
```

Figure 7.6: A sketch of defmacro.

The definition in Figure 7.6 gives a fairly accurate impression of what macros do, but like any sketch it is incomplete. It wouldn't handle the &whole keyword properly. And what defmacro really stores as the macro-function of its first argument is a function of two arguments: the macro call, and the lexical environment in which it occurs. However, these features are used only by the most esoteric macros. If you worked on the assumption that macros were implemented as in Figure 7.6, you would hardly ever go wrong. Every macro defined in this book would work, for example.

The definition in Figure 7.6 yields an expansion function which is a sharp-quoted lambda-expression. That should make it a closure: any free symbols in the macro definition should refer to variables in the environment where the defmacro occurred. So it should be possible to say this:

```lisp
(let ((op 'setq))
  (defmacro our-setq (var val)
    (list op var val)))
```

As of CLTL2, it is. But in CLTL1, macro expanders were defined in the null lexical environment, so in some old implementations this definition of our-setq will not work.

#heading(level: 2)[Macros as Programs]

A macro definition need not be just a backquoted list. A macro is a function which transforms one sort of expression into another. This function can call list to generate its result, but can just as well invoke a whole subprogram consisting of hundreds of lines of code.

Section 7.3 gave an easy way of writing macros. Using this technique we can write macros whose expansions contain the same subexpressions as appear in the macro call. Unfortunately, only the simplest macros meet this condition. As a more complicated example, consider the built-in macro do. It isn't possible to write do as a macro which simply shuffles its parameters. The expansion has to build complex expressions which never appear in the macro call.

The more general approach to writing macros is to think about the sort of expression you want to be able to use, what you want it to expand into, and then write the program that will transform the first form into the second. Try expanding an example by hand, then look at what happens when one form is transformed into another. By working from examples you can get an idea of what will be required of your proposed macro.
