---
title: Indexed optics dilemma
author: Oleg Grenrus
tags: lens, optics
---

Indexed optics are occasionally very useful.
They generalize `mapWithKey`-like operations found for various containers.

```haskell
iover (imapped % _2) :: (i -> b -> c) ->  Map i (a, b) -> Map i (a, c)
```

Indexed lens are constructed with [`ilens`](https://hackage.haskell.org/package/optics-core-0.3.0.1/docs/Optics-IxLens.html#v:ilens) combinator.

```haskell
ilens :: (s -> (i, a)) -> (s -> b -> t) -> IxLens i s t a b
```

It is implicit that the getter and the (indexed) setter part have
to satisfy usual [lens laws](https://hackage.haskell.org/package/optics-core-0.3.0.1/docs/Optics-Lens.html#g:5).

However there are problematic combinators, e.g. 
[`indices`](https://hackage.haskell.org/package/optics-core-0.3.0.1/docs/Optics-IxTraversal.html#v:indices):

```haskell
indices :: (Is k A_Traversal, is `HasSingleIndex` i)
    => (i -> Bool) -> Optic k is s t a a -> IxTraversal i s t a a
```

An example usage is

```haskell
>>> toListOf (itraversed %& indices even) "foobar"
"foa"
```

If we **combine** `ilens` and `indices` we get a nasty thing:

```haskell
\p -> indices p (ilens (\a -> (a,a)) (\_ b ->  b))
    :: (i -> Bool) -> IxTraversal i i i i i
```

That is (almost) the type of [`unsafeFiltered`](https://hackage.haskell.org/package/optics-core-0.3.0.1/docs/Optics-AffineTraversal.html#v:unsafeFiltered),
which has a warning sign:

<blockquote>
Note: This is not a legal <code>Traversal</code>., unless you are very careful not to invalidate the predicate on the target.
</blockquote>

However, neither `indices` nor `ilens` have warning attached.

*There should be an additional indexed lens law(s)*.

My proposal is to require that indices and values are independent,
which for indexed lens can  be checked by following equation:

*Whatever you put in, you cannot change the index.*

    fst (iview l s) ≡ fst (iview l (iover l f s))

where `l :: IxLens i s t a b`, `s :: s`, `f :: i -> a -> b`.

This law is generalisable to other optic kinds.
For traversals replace `fst` and `iview` with `map fst` and `itoList`.
For setters it is harder to specify, but the idea is the same.

Similarly we can talk about indexed prisms or even isomorphisms.
The independence requirement would mean that that index
have to be *boring* (i.e. isomorphic to `()`),
thus there isn't any additional power.

However sometimes violating laws might be justified,
(e.g. when we quotient types would made program correct, but we don't have them in Haskell).

This new law doesn't prohibit having duplicate indices in a traversal.

This observation also extends in to `TraversableWithIndex`.
As far as I can tell, all instances satisfy the above requirement
(of indices being independent of values).
Should we make that (IMHO natural) assumption explicit?
