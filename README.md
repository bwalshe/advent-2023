# Advent of Code 2023

This is my attempt at this year's [Advent of Code](https://adventofcode.com). 
I'm using Haskell this year, which is not my strongest language, to put it
lightly.

If you are not familiar with AoC, it's a yearly event where they put out 
two coding puzzles each day and people compete to be the first to complete
them. I started late and I'm not a particularly fast coder, so I'm not 
going to win any points there. Instead I have decided to record some notes
about my experience using Haskell - I want to see how it stacks up against
the experience of using something more mainstream like Java.

The way this project is structured is that I have my solutions in the 
`DayN.hs` files in the [lib/](lib/) folder and the 
[app/Main.hs](app/Main.hs) just runs whichever puzzle I have solved most 
recently. If all you want is to see how I solved the puzzles, them you 
are best off just looking at the daily `.hs` files. If you want to know
about how I _feel_ about those solutions, then read on. 


## Daily Log

I didn't actually start recording this log until day 3, so the first two
days might not be 100% accurate recounts. 

### Day 1
This was a fairly simple task at first. Just strip out the non-digit chars
and then pick the first and last (which may be the same position, if there
is only one digit). Things got more difficult though in the second task of 
the day where I wanted to use the `replace` function, which uses `Text` 
instead of `String`. 

`String` vs `Text` is something I have struggled to understand in the past,
but by the end of the day I realised it is actually pretty simple. By 
default, Haskell stores strings as linked lists of characters. This is a
really bad idea, and most languages have highly optimized string storage. 

The `Text` datatype _is_ a nice, optimised form of string, and you can 
make GHC use `Text` instead of `String`, by including the following directive 
at the top of your file

```
{-# LANGUAGE OverloadedStrings #-}
```

The bit that causes confusion - for me at least - is that this just changes
how GHC handles string literals. It does not remove the bad, linked-list
strings from your code or any of the libraries you are using. So you have to 
be careful not to mix them up. That should be straightforward, but I lost
loads of time to this today. 

I initially just wrote a single `.hs` file and compiled it with `ghc`, without
using any kind of build system. After I was finished up, I realised that things 
would have been much easier if I had some kind of build & unit testing system
set up, so I went back and created a Cabal project & 1 or 2 tests using HUnit, 
so that I would be ready for tomorrow. 

### Day 2
Today's challenge revolved around parsing out some information and then doing
a calculation based on this. I was actually super-interested in this kind of
problem, as I have worked a bit with parsing before using grammars, but hadn't 
done things the Haskell way with combinators. Today's task gave me an opportunity
to finally give [Megaparsec](https://hackage.haskell.org/package/megaparsec) a go.

I'm going to be honest, I really struggled getting started with this one. I think
my biggest problem is that the docs assume you already have a good handle of 
the Haskell way of doing things. The examples use operators like `<*` without
explaining what they do, and assume you will infer that `*>` is also available. 

I also got bitten quite hard by the `String` vs `Text` issue again today. 
Megaparsec is flexible enough that it can parse both `String` and `Text`
input, but some of the functions have inputs that have to be stings. For
example the [`runParser`](https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec.html#v:runParser)
function has a `fileName` parameter which is always `String` even if your
input data is `Text`. This is pretty simple once you understand what is 
going on, but I found it caused very confusing error messages for me today. 

On a more positive note, it was nice to see how once I identified that 
one of the pieces I was working with was actually a Monoid, the compiler/LSP
started making lots of really good suggestions on how to streamline my code. 

Thankfully, once I had the parsing down, both tasks for day were fairly simple.

Side-note: After setting everything up so I could do a more test-driven approach
to solving these tasks, I didn't write a single unit test today. I just tried
things out in the repel and then copied my progress into my `.hs` file. I 
definitely would not have been able to do that using Java, and even in Python
it would have been tricky.


### Day 3
Another day where it looked like parsing was going to be the main hurdle. 
After all the time I had put into learning Megaparsec yesterday, I was 
chomping at the bit to put this knowledge into practice. 

Unfortunately, I really couldn't see how to do today's task using Megaparsec, 
and wasted a lot of time reading the docs for info that I could not find. In 
the end, I realised that it would be fairly straightforward to do without 
using a parsing library. 

Today's biggest problem was that I could not get my LSP to work correctly
while writing unit tests. These live in a separate cabal package to the
daily code, and when editing these files, the LSP was failing to pick up
on mistakes I was making and wasn't making good auto-complete suggestions.
The weird thing was that it was still kind of working, but it just wasn't 
as complete as when I was writing code in the `lib` folder. I'm sure it's
just some setting in my `.cabal` file, but it's frustrating when you decide
to take 20 mins to solve a puzzle and instead spend an hour trying to read
the docs for cabal. 

### Day 4
Today was much more straightforward than the previous ones. Task 1 was simple
and I have my setup more in order. I do still get irrationally frustrated
at the `String` vs. `Text` thing, and I wasted a bit of time when writing
unit tests as I was trying to use `parseTest` instead of `runParser` in the
test code. This is a me problem, not a Haskell problem, as the error messages
were actually pretty clear.

Task 2 was a bit more of a challenge, as it required indexing into a list and
this is not something that the standard Haskell lists are good at. Like an 
idiot, I tried to work around this instead of just finding a data structure
that allowed efficient indexing. In the end I saw sense and used the
[Vector](https://hackage.haskell.org/package/vector) package. It's a bit 
annoying that something like this is not part of the standard set of 
Haskell packages, but at least it is less confusing than the `String` 
situation. ¯\\\_(ツ)_/¯

### Day 5
This is the day that broke me. The initial task was quite straightforward - 
compose a list of mapping functions together, apply the composed function
to a list of numbers and find the lowest result. The second task was the 
same, except now there were billions of input numbers, so mapping and then 
sorting the results was not feasible. I actually had a good idea of how to 
solve the problem, but my lack of Haskell experience got the better of me here.

The basic idea is that a `Mapping` consists of a set of intervals and translation 
amounts. If you had a single mapping and a very long list of numbers, and you 
wanted to find which number in that list mapped to the lowest result, you should
only look in the interval that maps down to the lowest values. On top of this,
while mappings are not monotonic _over all_, within each interval they *are* 
monotonic - meaning that you only need to find the lowest input value in the lowest
interval. As the inputs are already sorted, this means that no sorting is needed
at all.

If you have two `Mapping`s, `m1` and `m2`, then you could combine them to form `m3`
with a new set of intervals and translation amounts where `m3(x) == m2(m1(x))`.
The `Mapping`s form a Monoid, and a list of mappings can be squashed down using
`mconcat`. This means that the solution to task 2 is to first combine all the mappings, 
find the interval that maps to the lowest output interval, and then scan through 
for the first input that is inside that interval.

Where I went wrong is that I didn't know about `mconcat`, or at least I didn't 
know exactly which of `mconcat`, `concat`, `sequence`, or any number of other
higher order functions I should be using. This meant I was writing a lot more
code than I needed, doing things at a lower level of abstraction. The method of
combining two mappings is a bit tricky, and more code means more chances to make
mistakes.

Long story short, I gave up in frustration here. About a week later (I think),
I came back and fixed things up, but this "day" was a slog.


### Day 6
This was more of a maths puzzle than a coding task. The only issue worth mentioning
here was that I did spend a bit of time scratching my head trying to understand
how Haskell deals with converting floating point values to/from integer values.


### Day 7
This day's tasks involved scoring hands of a card game. This is one of those tasks
that are very straightforward when your language has build-in pattern matching.


### Day 8
After two fairly easy days, here was another one that damn near killed me. Not 
really because the task itself was difficult, but because of how I approached 
it. 

Today's task was to navigate from one point in a graph to another, and count 
the number of steps. I decided that I really, really, really wanted to solve
this using the `State` monad, as this is a thing I had been trying to understand
for a while, but could never quite grasp, and this looked like a problem that 
could be solved using state. 

Long, long story short: I went off and read a book chapter about how the 
`State` monad works. I did some practice exercises to make sure I really 
understood what was going on. Then I came back to the AoC problem and tried 
applying what I had learned. I spend quite a bit of time trying to force
the State monad (actually an [RWST](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-RWS-Strict.html)
to be precise) into my solution, but in the end I gave up and just used 
a regular function with a very ugly signature to hold all the state as 
normal arguments. Oh, well.
