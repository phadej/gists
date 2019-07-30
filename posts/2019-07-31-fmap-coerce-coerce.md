---
title: Should fmap coerce = coerce hold?
author: Oleg Grenrus
---

Discussion around (limitations of)
[`Coercible`](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#coercible),
[`GeneralizedNewtypeDeriving`](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#extension-GeneralizedNewtypeDeriving) or
[`DerivingVia`](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia)
resume once in a while.

One common question is whether `fmap coerce = coerce` should be
a law of the `Functor` type-class, or not.
Intuitively, it's similar to the identity law: `fmap id = id` law, but intuition is not good enough reason alone.

Category theory may say...
--------------------------

The identity law comes from category theory, so what could category theory
say about `fmap coerce = coerce` like proposition?

Let us consider an endofunctor $F$ from a three element category, $\mathcal{{C}$ to itself,
which "rotates the category".

<img title="functor-abc" src="../images/functor-abc.png" />

$F$ maps objects $A, B, B'$ to $B', B, A$ respectively. Careful reader
would ask how $F$ maps morphisms. See an appendix, for a justification.

To make it more concrete we can think that as:

```haskell
type A     = Int
type B     = Word
newtype B' = B' Word
```

with isomorphisms in between (assuming `Int` and `Word` have the same amount of bits).

Then, we know that `Coercible B B'`. And arrows $g$ and $g^{-1}$ on the diagram
are `coerce @B @B'` and `coerce @B' @B`.
But `Coercible (F B) (F B') = Coercible B' A` doesn't exist.
$F(g)$ is a function like `\(B' w) -> fromIntegral w` which is not `coerce`.

The functor $F$ doesn't preserve the `Coercible` relation, even it's a categorically lawful functor.

... but ...
-----------

But the functor $F$ above is not a `Functor` as in Haskell.
Not even if we try to define

```haskell
data F :: Type -> Type where
   FA  :: A  -> F A
   FB  :: B  -> F B
   FB' :: B' -> F B'
```

we'll not be able to write `fmap :: (a -> b) -> F a -> F b`, as `b`
can be anything, not only `A`, `B` or `B'`.

`fmap` is *polymorphically parametric*.
There are papers and blogs explaining parametricity[^blogs].

[^blogs]: Some links about parametricity
- [Parametricity Tutorial](https://www.well-typed.com/blog/2015/05/parametricity)
- [Parametricity: Money for Nothing and Theorems for Free](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/)
- [Wadler: Parametricity](https://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html)

The key insight of parametricity is that we can read *types as relations*. For
example a list defined as

```
data List x = Nil | Cons x (List x)
```

can be read as a relation constructor:

Given a relation `R :: Relation X Y`,
`xs :: List X` and `ys :: List Y` are related with `List R :: Relation (List X) (List Y)`,
if either
- `xs = Nil` and `ys = Nil`, or
- `xs = Cons x xs'`, `ys = Cons y ys'`, and `x` and `y` are related by `R`, and
  `xs'` and `ys'` are recursively related by `List R`.

In other words, `List R` relates lists of equal length, when
all elements are related pointwise.

We can derive free theorems for polymorphic functions over lists, or ...

We could take `R X Y` to be `Coercible X Y`.
Relation doesn't need to be functional.
`Coercible` is a binary-relation.
We can reason that `List Coercible` is `Coercible` for lists:
Lists related by `coerce @(List X) @(List Y)` are of the same length and each
element is related by `coerce @X @Y`.

Functor should be parametric!
-----------------------------

Maybe instead of asking whether `fmap coerce = coerce` should be
a law of the `Functor` type-class, we should ask whether
`Functor` should be a type-class of *parametric* type constructors?
Parametricity would imply `fmap coerce = coerce`.
Non-requirement of parametricity, would mean that opaque types can be "lawfully"
`Functor`, e.g. similarly as  `Set a` has an `Eq` instance.

The latter choice allows us to write type internally used in the `bifunctors` library:

```haskell
data Mag a b t where
    Pure  :: t -> Mag a b t
    Map   :: (x -> t) -> Mag a b x -> Mag a b t
    Ap    :: Mag a b (t -> u) -> Mag a b t -> Mag a b u
    One   :: a -> Mag a b b

instance Functor (Mag a b) where
    fmap  = Map

instance Applicative (Mag a b) where
    pure  = Pure
    (<*>) = Ap
```

`Mag` is not lawful `Functor`, it violates `fmap id = id` property.
but is used to implement [`traverseBiaWith`](https://hackage.haskell.org/package/bifunctors-5.5.4/docs/Data-Biapplicative.html#v:traverseBiaWith).

However, there is a comment in the code:

```
-- Note: if it's necessary for some reason, we *could* relax GADTs to
-- `ExistentialQuantification` by changing the type of One to
-- 
--     One :: (b -> c) -> a -> Mag a b c
```

Then `Mag` will be `Coercible` in all argument,
current `Mag` has `type role Mag representational nominal nominal`.

A third alternative is to use

```
One :: Coercible b c -> a -> Mag a b c
```

GHC is smart enough to infer that this `Mag` variant is also `representational`
in all arguments. Unfortunately, to implement `traverseBiaWith`,
we'll need to change the required constraints 

```diff
-Biapplicative p
+( Biapplicative p
+, forall x y. (Coercible b x, Coercible c y) => Coercible (p b c) (p x y))
+)
```

... or require `Bifunctor` (a superclass of `Biapplicative`) to be parametric in its both arguments!

For me, it looks like that addition of `(forall x y. Coercible x y => Coercible (f x) (f y))`
constraint to `Functor` (and similarly for `Bifunctor`, `Profunctor`, `Contravariant` ...) can be worked around in *all cases*.
The `Mag` is the only example mentioned as *useful* but not lawful `Functor`,
and we have demonstrated a required change.

Note: `Mag` will still be unlawful, but it can be made `representational` in all arguments.
In the implementation of `traverseBiaWith` we will use `coerce` which is free operationally, so there shouldn't be any performance degradation.

I cannot come up with an example of `f :: Type -> Type` which would violate `fmap id = id`
law, but obey `fmap coerce = coerce` one (or an opposite direction).
And I cannot show that one follows from another.
[Edward Kmett explains how](https://www.schoolofhaskell.com/user/edwardk/snippets/fmap)
`Functor` composition law follows from identity law and parametricity.
Coerce law is implied by parametricity directly.

Conclusion
----------

Finally, my conclusion is that both

- `fmap coerce = coerce` should be a law of GHC Haskell `Functor`, and
- the `forall a b. Coercible a b => Coercible (f a) (f b)` be `Functor` super-constraint

even the change departs us far far from Haskell2010.
The justification is that these requirements are implied by *parametricity*,
and `Functor` class should contain only parametric type constructors.
We would still be able to have unlawful `Functor`s if really needed.
Some immediate benefits are ability to put [`join` into `Monad`](https://ryanglscott.github.io/2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad/)
or that van Laarhoven `Lens s t a b` would be `representable` in all arguments.


Appendix: Mapping of morphisms
------------------------------

In this category there is an unique arrow between any two objects, so the mapping is trivial.

We can extend the category by adding more objects and morphisms,
and $F$ so it keeps all other objects in place. In this case mapping of
morphisms is trickier. If we require that $f \circ f^{-1} = 1_A$ and others
identities suggested by names or arrows we can make it work.

Let us define two families of morphisms indexed by objects $X$ in $\mathcal{C}$:
$\mathsf{from}_X : X \to F(X)$ and $\mathsf{to}_X : F(X) \to X$

$$
\begin{aligned}
\mathsf{from}_A  &= f   \qquad & \mathsf{to}_A  &= f^{-1} \\
\mathsf{from}_B  &= g          & \mathsf{to}_B  &= g^{-1} \\
\mathsf{from}_B' &= h          & \mathsf{to}_B' &= h^{-1} \\
\mathsf{from}_X  &= 1_X        & \mathsf{to}_X  &= 1_X
\end{aligned}
$$

Using these, we can define mapping of morphisms as

$$
F (j : X \to Y) : F(X) \to F(Y) = \mathsf{from}_Y \circ j \circ \mathsf{to}_X
$$

For example $f : A \to B$ is mapped to

$$
\begin{aligned}
F(f) &= \mathsf{from}_B \circ f \circ \mathsf{to}_A \\
     &= g \circ f \circ f{^-1} \\
     &= g
\end{aligned}
$$

Check by looking at the diagram!

The identity and composition properties hold, for example

$$
\begin{aligned}
F(1_A) &= \mathsf{from}_A \circ 1_A \circ \mathsf{to}_A \\
       &= f \circ 1_A \circ f^{-1} \\
       &= 1_B \\
       &= 1_{F(A)}
\end{aligned}
$$
