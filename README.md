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
`DayN.hs` files in the [libs/](libs/) folder and the 
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
to finally give [Megaparsec]() a go.

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

