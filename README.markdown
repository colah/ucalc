ucalc: A Unicode Calculator!
============================

Quick Tour
----------

Can work with unicode:

    ==>log₂(2¹⁰)+(1,0)⋅(1,0)+sin(π)
    11.0

Tries to give precise answers:

    ==>1/6+3/4+sin(e)
    11/12 + sin(e) ≅ 1.327448

And simplify things:

    ==>π
    ½τ ≅ 3.1415927

Why?
----

I had free time on a train ride to Berlin :)

I think that we should use unicode in programming more. If you're a *nix user, the idea that typing unicode is difficult is a myth -- look into XCompose and something like [Kragen's .XCompose](https://github.com/kragen/xcompose) and you'll be fluently typing unicode in an hour or two. Alternatives exist for Windows. On the other hand, unicode goes a long ways to make things clear. This isn't a full blown programming language, but it is proof of concept for the viability of math notation-like programing languages using unicode.

Also, I wanted to actually finish a haskell project. Previous haskell projects have been derailed by me obsessing over "doing things the right way" ™ and code neatness.

Warning!
--------

This code is a short hack. It isn't documented well, and I'm pretty much a haskell novice.

I expect there are many bugs and things I should have implemented and haven't yet. And I can't make any promises about me continuing to work on it -- Malthus is my priority!

Building
--------

Run "ghc --make ucalc.hs", ghc alone won't work. You need parsec, readline, and utf8-string.

Last Comments
-------------

parsec is awesome! I don't know that I could ever go back to another parser.


