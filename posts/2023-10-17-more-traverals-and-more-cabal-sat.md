---
title: More traversals and more Cabal SAT
author: Oleg Grenrus
---

In [the previous post] I discussed using traversals for batch operations.

I forgot to mention any libraries which actually do this.
They are kind of hard to find, as often the `Traversable` usage comes up very naturally.

One such example is [`unification-fd`](https://hackage.haskell.org/package/unification-fd).
As the name suggests the library is for doing unification.
One operation in the process is applying bindings, i.e. substituting
the unification values with the terms they have been unified to.
(I think that's what *zonking* is in GHC[^1]).

[^1]:
    GHC relies heavily on mutability in the typechecker for efficient operation.
    For this reason, throughout much of the type checking process meta type
    variables (the MetaTv constructor of TcTyVarDetails) are represented by mutable
    variables (known as TcRefs).
  
    Zonking is the process of ripping out these mutable variables and replacing them
    with a real Type. This involves traversing the entire type expression, but the
    interesting part of replacing the mutable variables occurs in zonkTyVarOcc.

The function type signature is 

```haskell
applyBindings :: (...)
              => UTerm t v -> em m (UTerm t v)
```

*But* the library also provides the *batched* method:

```haskell
applyBindingsAll :: (..., Traversable s)
                 => s (UTerm t v) -> em m (s (UTerm t v))
```

And the docs say:

<blockquote>
Same as applyBindings, but works on several terms simultaneously. This function preserves sharing across the entire collection of terms, whereas applying the bindings to each term separately would only preserve sharing within each term.
</blockquote>

The library also has `freshen` and `freshenAll`.

When I was studying how `unification-fd` works, having `applyBindingsAll` operation with `Traversable` is very natural,
the library make use of `Traversable` a lot already anyway.

There are probably more examples, but I cannot find them.
(If you know any others, please tell me, I'll be happy to learn more, and maybe include them into this post).

sat-simple
----------

One another example is [`sat-simple`](https://hackage.haskell.org/package/sat-simple),
a *hopefully* simple SAT library (e.g. simpler than `ersatz`).

/The/ operation of a library is

```haskell
solve :: Traversable model => model (Lit s) -> SAT s (model Bool)
```

We have some `model` with symbolic boolean variables (`Lit s`),
and the `solve` finds a concrete assignment of them `Bool`.

For comparison, `ersatz` uses type-family (`Decoded`):

```haskell
solveWith :: (Monad m, HasSAT s, Default s, Codec a)
          => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
```

while `ersatz` approach is arguably more expressive, find it somewhat more "magical":
The `Decoded x` value may look very different than `x`.

different types
---------------

I saw [a comment on Twitter/X](https://twitter.com/tomjaguarpaw/status/1712839658290233505)

<blockquote>
If arguments can have different types then you need to generalize somehow, and product-profunctors is one sufficient generalization.
</blockquote>

I never grasped [`product-profunctors`](https://hackage.haskell.org/package/product-profunctors-0.11.1.1/docs/Data-Profunctor-Product.html) library.
The `ProductProfunctor` class looks like

```haskell
class (forall a. Applicative p a, Profunctor p) => where
   (***!) :: p a b -> p a' b' -> p (a, a') (b, b')  -- Arrow has (***)
```

and it feels like a very ad-hoc collection of things.

indexed
-------

There is an alternative solution to "if arguments can have different types".
Often when you have singular thing, and you want to generalize to many things,
you make an indexed version of the singular thing.

By indexed here I mean, changing `Type` to `I -> Type` for some index `I`.

A simple example is recursive types. Suppose a language has recursive types,
so we can write

```haskell
data Nat = Zero | Succ Nat
```

but this language does not have *mutual* recursive types, but happens to have
`DataKinds` and `GADTs` like features.

So we cannot write

```haskell
data Even = Zero | SuccOdd  Odd
data Odd  = One  | SuccEven Even
```

but we can write

```haskell
data I = E | O

type :: I -> Type
data EvenOdd i where
    Zero     :: EvenOdd E
    SuccOdd  :: EvenOdd O -> EvenOdd E
    One      :: EvenOdd O
    SuccEven :: EvenOdd E -> EvenOdd O

type Even = EvenOdd E
type Odd  = EvenOdd O
```

And sometimes the latter encoding "works" better, e.g. mutual recursion becomes ordinary recursion on a single type.
I remember having better time satisfying Coq termination checker with a similar trick.

another indexed traversable
---------------------------

So what does "indexed" `Traversable` looks like. It looks like

```haskell
type FTraversable :: ((k -> Type) -> Type) -> Constraint
class (...) => FTraversable t where
  ftraverse :: Applicative m => (forall a. f a -> m (g a)) -> t f -> m (t g)
```

This class exists in many libraries on Hackage

- [`FTraversable` in `hkd`](https://hackage.haskell.org/package/hkd-0.2.1/docs/Data-HKD.html#t:FTraversable)
- [`TraversableB` in `barbies`](https://hackage.haskell.org/package/barbies-2.0.4.0/docs/Data-Functor-Barbie.html#t:TraversableB)
- [`Traversable` in `rank2classes`](https://hackage.haskell.org/package/rank2classes-1.5.2/docs/Rank2.html)

I use `FTraversable` in my [`version-sat`](https://github.com/phadej/version-sat) experiment.

It's like `simple-sat`, but adds `Version` literals. The `solve` function has more general type,
which however looks very similar.

```haskell
solve :: FTraversable model => model (Lit s) -> SAT s (model Identity)
```

We can have symbolic booleans `Lit s Bool`, but also symbolic versions `Lit s Version`,
the resulting model will have `Bool`s and `Version`s (wrapped in `Identity`).

"Historical" note: `simple-sat` started as `hkdsat`, trying to allow encodings like in `ersatz`,
and maybe it eventually will, if I find simple way to add them. [^2]

[^2]:
    In my opinion, the `Bool` only, `Traversable` based `solve` is very simple to work with, when it's enough.
    And the `Version` encoding in `version-sat` is (ab)using mutability a lot,
    I haven't tried to do it in `ersatz`.

version-sat: satisfiable
------------------------

What I do with `version-sat`. Well, it's just an experiment for now.
One thing you can do, is to ask whether a `Cabal` library
has *any* build plan.

That is very straight-forward: convert a library stanza (conditional tree)
information into proposition formula and ask whether it has any
satisfiable models.

It turns out that 417 of 125991 libraries are unsatisfiable.
For example [`vector-0.12.1.1`](https://hackage.haskell.org/package/vector-0.12.1.1/revisions/)
has been revisioned with `base <0` bound.

I think that is fine number. Mistakes happen and 0.33% is a very small amount of b0rked releases.
Many of these revisions are actually on my packages.
And probably the number should be a bit larger, as people *deprecate* package version,
which allows them still be installed, just less prioritized.

While you can look for unsatisfiable `build-depends` syntactically,
it's becomes less obvious with `if` conditionals etc.

Throwing problem at a SAT solver in full generality is a *complete* (i.e. always give a definitive answer) approach.

version-sat: disjoint automatic flags
-------------------------------------

Another question we can ask `version-sat` is

<blockquote>
Is there a install-plan which satisfies package definition
with an automatic flag turned on *and* off.
</blockquote>

That probably needs an explanation.
In [cabal-install solver as sat solver](https://oleg.fi/gists/posts/2023-08-30-using-cabal-install-solver-as-sat-solver.html) I briefly touched this topic.

Perfectly, the *automatic* flag assignment is disjoint,
so the assignment made by dependency solver is deterministic (function of package versions in install plan).

The easy way to ensure it is to have disjoint constraints:

```cabal
  if flag(old-locale)
    build-depends:
        old-locale  >=1.0.0.2 && <1.1
      , time        >=1.4     && <1.5

  else
    build-depends: time >=1.5 && <1.7
```

The `time <1.5` and `time >=1.5` constraints are disjoint,
so depending on which `time` package version is picked, the value of `old-locale` flag is forced.

I was surprised that 15776 of 136048 libraries are with non-disjoint automatic flags (11.60%).
That number seems very high.

There are obvious false positives however, e.g. `semigroups` has following structure

```
if impl(ghc < 7.11.20151002)
    if flag(bytestring)
      if flag(bytestring-builder)
        build-depends: bytestring         >= 0.9    && < 0.10.4,
                       bytestring-builder >= 0.10.4 && < 1
      else
        build-depends: bytestring         >= 0.10.4 && < 1
```

An automatic `bytestring-builder` flag has only an effect on old GHC and when manul `bytestring` flag are on.

However, the dependency solver would still try to flip `bytestring-builder` flag if it cannot satisfy the other dependencies.
Not a terrible cost in case of `semigroups`, but might be for some other packages (with non-trivial dependencies).
A way to force it would be to have

```cabal
if impl(ghc < 7.11.20151002)
  ...
else
  if flag(bytestring-builder)
    build-depends: base <0
```

Another example of invalid usage of automatic flag is e.g. `examples` flag in `Earley` (which [have been fixed](https://github.com/ollef/Earley/commit/7640364ccaae0d067b7a00e108c14d1a47d7a89c) long ago: `examples` is now a *manual* flag).
The flag disables building of example `executables`.
When it was automatic dependency solver could unnecessarily flip it, and try to satisfy the example dependencies as well.

Unfortunately there are a lot of what I consider invalid usage of automatic flags.
(Having flags automatic by default is really a wrong default, IMO).

But for example `accelerate`, `atomic-primops`, `hashtables`, `unordered-containers` have flags like `debug`, `bounds-checks` which affect the package code in a non-trivial way.
You definitely don't want dependency solver flipping flags like that.

The [`Cabal` documentation says](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-flag-manual)

<blockquote>
By default, Cabal will first try to satisfy dependencies with the default flag value and then, if that is not possible, with the negated value. 
</blockquote>

However, I don't think this can or should be relied upon.
The build plans are generally non-comparable.

A somewhat contrived example is

```
flag foo
  default: True

flag bar
  default: True

library
  ...

  if flag(foo) && flag(bar)
    build-depends: base <0
```

the solver will need to make a (non-deterministic choice) to flip either flag.

Secondly, it restricts possible alternative solver implementations.
I.e. they also would need to try hard to keep automatic flags at their default values.
Luckily e.g. `minisat` tries literals with `False` first, so one can initialise flag literals so their default value matches.
Still, SAT solver is a black box, there isn't hard guarantees it won't flip something just because it feels like that.

TL;DR from the [cabal-install solver as sat solver](https://oleg.fi/gists/posts/2023-08-30-using-cabal-install-solver-as-sat-solver.html) post:

<blockquote>
Only use automatic flags for encoding `if build-depends(...)` like constraints.
</blockquote>
