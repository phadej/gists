---
title: "SO: How do I show that a Haskell type is inhabited by one and only one function?"
author: Oleg Grenrus
---

### The Question

We know that `forall a. a -> a` has only one element: `id = \x -> x`.
How to show it for more complicated type, like: `(b -> a) -> (a -> b -> c) -> b -> c`


### The answer

- You can apply [sequent calculus](https://en.wikipedia.org/wiki/Sequent_calculus)
- Or you can use `Yoneda lemma` -approach with smart choices of functors.

Read rest the rest on [StackOverflow](http://stackoverflow.com/questions/32576956/how-do-i-show-that-a-haskell-type-is-inhabited-by-one-and-only-one-function/32578222#32578222)
