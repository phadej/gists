---
title: Functor Optics
author: Oleg Grenrus
tags:  lens
---

It's good time to write tongue-in-cheek posts on Christmas Holidays.

You have probably have heard about *Profunctor Optics*, there
is a [paper by Matthew Pickering et. al](https://arxiv.org/abs/1703.10857),
and my [*Glassery* post](2017-04-18-glassery.html). *Functor optics* are
similar and *simple*. With profunctors you can define type-changing optics,
with bare `Functor` you can define only so called simple optics.

<div id="toc"></div>

<div class="hidden">

As this is a literal Haskell file, first a moderate prelude

```haskell
{-# LANGUAGE RankNTypes, TypeOperators, DeriveFunctor, GADTs #-}
module FunctorOptics where

import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Functor.Invariant
import Data.Type.Equality
import Data.Monoid (Endo (..))
import Data.Tuple (swap)
import Data.Bifunctor (first)

import qualified Data.Profunctor as P
```

</div>

Functor optics?
---------------

As I mentioned already, to express type changing optics  the `OpticP` (`P` for
profunctor) type synonym takes five arguments: a profunctor and four type
variables:

```haskell
type OpticP p s t a b = p a b -> p s t
```

But what if you don't want to change types? When `s ~ t` and `a ~ b`, we
can simplify the above into *Functor optics*:

```haskell
type OpticP' p s a = p a a -> p s s
type Optic f s a = f a -> f s
```

As with profunctor optics, there are different type-classes constraining
variable a -functor, that's how we get lenses, prisms, getters, setters etc.
We will explore them below.

Functor
-------

Every Haskell programmer is familiar with the `Functor` type class:

``` {.haskell .ignore}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

What might be surprising, in functor optics it's used to define `Review`
(similarly as `Bifunctor` is used to define `Review` in profunctor optics,
this kind of analogy is visible through all definitions).

```haskell
type Review s a = forall f. Functor f => Optic f s a
```

We have `upto` to create `Review` and `review` to use one. `upto` is simply a `fmap`,
and to define `review` we use `Identity` functor: `a -> b ≅ Identity a -> Identity b`.

```haskell
upto :: (a -> s) -> Review s a
upto = fmap

type AReview s a = Optic Identity s a

review :: AReview s a -> a -> s
review l a = runIdentity (l (Identity a))
```

By the way,
[A Representation Theorem for Second-Order Functionals by Mauro Jaskelioff and Russell O'Connor](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/div-classtitlea-representation-theorem-for-second-order-functionalsdiv/4B782B0BB6EB53E53935D7A0F8432E8E)
tells us that 

``` {.haskell .ignore}
forall f. Functor f => f a -> f s  ≅  a -> s
```

which is intuitively true. The left to right is witnessed by `review`,
and other direction by `upto`. However, the paper has more rigorous and general proof.

Existing classes
----------------

Let's continue with classes more or less widely used, i.e. there
is a package on Hackage!

<h3 id="t:Contravariant">Contravariant</h3>

"Dually" to `Functor` (dual of `Functor` is `Functor`),
the `Contravariant` type class gives rise to a `Getter`.

> Whereas in Haskell, one can think of a `Functor` as containing or producing
values, a contravariant functor, `Contravariant` is a functor that can be thought of as
*consuming* values.

`Contravariant` lives in a [`contravariant`](http://hackage.haskell.org/package/contravariant) package.

``` {.haskell .ignore}
class Contravariant f where
    contramap :: (b -> a) -> f a -> f b
```

As with `Review`, constructor of `Getter` is simply a member of the class,
in this case `contramap`:

```haskell
type Getter s a = forall f. Contravariant f => Optic f s a

to :: (s -> a) -> Getter s a
to = contramap
```

`to` was easy to define, for `view` we will need a new `newtype` `Flipped`:

```haskell
newtype Flipped a b = Flip { unflip :: b -> a }

instance Contravariant (Flipped a) where
    contramap f (Flip g) = Flip (g . f)

type AGetter s a = Optic (Flipped a) s a

view :: AGetter s a -> s -> a
view l s = unflip (l (Flip id)) s
```

<h3 id="t:Invariant">Invariant</h3>

The `Invariant` class is less known. It's a moral superclass of both, `Functor`
and `Contravariant` type classes. In fact, any `* -> *` type parametric (i.e.
ADTs) in the argument permits an instance of `Invariant`.  It lives in
[`invariant`](http://hackage.haskell.org/package/invariant) package.

``` {.haskell .ignore}
class Invariant f where
    invmap :: (a -> b) -> (b -> a) -> f a -> f b
```

For example, `Flipped` is also an `Invariant` functor:

```haskell
instance Invariant (Flipped a) where
    invmap _ = contramap
```

Reader could guess that `invmap` is a constructor for `Iso`.

```haskell
type Iso s a = forall f. Invariant f => Optic f s a

iso :: (a -> s) -> (s -> a) -> Iso s a
iso = invmap
```

`view` and `review` work with optics constructed with `iso`, e.g.

```haskell
negated :: Num a => Iso a a
negated = iso negate negate

-- evaluates to -2
example1 :: Int
example1 = view negated 2
```

One remark about `Invariant`. In
[*Profunctor Optics: The Categorical view*](https://bartoszmilewski.com/2017/07/07/profunctor-optics-the-categorical-view/)
Bartosz Milewski looks into profunctor optics. In very similar way, we can look into
Functor optics, yet instead of using pairs *<a, b>* and *<s, t>*, it's enought to 
use *a* and *s*, then we arrive to the similar conclusion:

``` {.haskell .ignore}
forall f. Invariant f => f a -> f s  ≅  (s -> a, a -> s)
```

<h3>No class at all</h3>

If we don't constraint `f`, we have an equality. Note: `data a :~: b` is a
non-parametric GADT, and therefore **not even ** an `Invariant`. 

```haskell
type Equality s a = forall f. Optic f s a

simple :: Equality a a
simple = id

toEquality :: a :~: s -> Equality s a
toEquality = gcastWith

fromEquality :: Equality s a -> a :~: s
fromEquality l = l Refl
```

Also note, here we havee a Leibnizian equality as in [`eq` package](http://hackage.haskell.org/package/eq),
just without a `newtype` wrapper:

```haskell
data a := b = Refl' { subst :: forall c. c a -> c b }
```

New classes
-----------

So far we handled the edges of optics lattice: `Identity`, `Iso`, `Getter`, and `Review`.
But how about simple `Lens`?

We can define `over` using a `Endo` newtype.

```haskell
type ASetter s a = Optic Endo s a

over:: ASetter s a -> (a -> a) -> (s -> s)
over l f = appEndo (l (Endo f))
```

Note: `Endo` is neither `Functor`, nor `Contravariant`, but it's `Invariant`!

```haskell
-- evaluates to -25
example2 :: Int
example2 = over negated (\x -> x * x) (- 5) 
```

But what's the type-class for `Setter`? Here, we can use what we know
about profunctor optics. There, the class is `Mapping`, we can have similar class too

```haskell
class Invariant f => Mapping f where
    map' :: Functor g => f a -> f (g a)
```

`Endo` is an instance of this class, but `Flipped` isn't (we cannot `view` through a `Setter`!):

```haskell
instance Mapping Endo where
    map' (Endo f) = Endo (fmap f)
```

So we can defined `Setter` as

```haskell
type Setter s a = forall f. Mapping f => Optic f s a
```

For `setting` we need an auxiliary `Functor Context`:

```haskell
setting :: ((a -> a) -> s -> s) -> Setter s a
setting f = invmap (\(Context g s) -> f g s) (Context id) . map'

data Context a b t = Context (b -> t) a deriving Functor
```

`Setter` was defined in a very same way as it would be in a profunctor optics,
so are many other optics, e.g. `Lens` we cover next.

<h3 id="t:Strong">Strong</h3>

We'll conclude the journey by defining `Lens`. As with `Setter`,
we can piggyback on what we know from profunctors:

```haskell
class Invariant f => Strong f where
    first' :: f a -> f (a, c)
    first' = invmap swap swap . second'

    second' :: f a -> f (c, a)
    second' = invmap swap swap . first'

    {-# MINIMAL first' | second' #-}
```

There aren't that many `Strong` functors, `Endo` and `Flipped` are
(we we can `over` / `set` and `view` through the lens, respectively):
    
```haskell
instance Strong Endo where
    first' (Endo f) = Endo (first f)

instance Strong (Flipped a) where
    first' (Flip f) = Flip (f . fst)
```

So finally we can define `Lens` type-alias and `lens` constructor:

```haskell
type Lens s a = forall f. Strong f => Optic f s a

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter fa = invmap 
     (\(a, s) -> setter s a)
     (\s -> (getter s, s))
     (first' fa)
```

Conclusion
----------

In this post we saw definitions *Functor optics*, which are simple
versions of *Profunctor optics*. This simplicity might be useful when
experimenting with optics, as there is less type-variables to deal with.
Working out `Prism` or `Traversal` is this "formalism" is left as an exercise to the reader.
Another interesting exercise is to figure out *Free `Strong`*, apply it
in the construction of Jaskelioff & O'Connor & Bartosz, turn the category theory wheels
and see `data ConcreteLens s a = CL (s -> a) (s -> a -> s)` pop out.

Please comment on [Reddit thread](https://www.reddit.com/r/haskell/comments/7lpcqe/functor_optics/) or on [Twitter](https://twitter.com/phadej/status/944601035364077570).

Postscriptum
------------

[Edward Kmett mentions on Twitter](https://twitter.com/kmett/status/944727625121124352)
<blockquote>
Fun fact: We figured out prisms, etc. for this form of optic before we figured out the profunctor versions.
</blockquote>

<h3>To and from Profunctor optics</h3>

[Phil Freeman wonders on Reddit](https://www.reddit.com/r/haskell/comments/7lpcqe/functor_optics/drpyh0k/)
on how you go between Profunctor and Functor optics.

Let's see a simple example converting `Lens`. From functor to profunctor is
simple using `Trace` newtype Phil mentions:

```haskell
data Trace p a = Trace { getTrace :: p a a }

instance P.Profunctor p => Invariant (Trace p) where
    invmap f g (Trace x) = Trace (P.dimap g f x)

instance P.Strong p => Strong (Trace p) where
    first' (Trace x) = Trace (P.first' x)

profunctorFirst :: P.Strong p => OpticP' p (a, c) a
profunctorFirst = getTrace . first' . Trace
```

Other way around is a little tricker!

Phil *thinks* using existential [`Split`](https://pursuit.purescript.org/packages/purescript-profunctor/3.1.0/docs/Data.Profunctor.Split#t:Split)
would work. And **he is right**.

```haskell
data Split f a b where
    Split :: (a -> x) -> (x -> b) -> f x -> Split f a b

instance P.Profunctor (Split f) where
    dimap f g (Split h i x) = Split (h . f) (g . i) x

split :: f a -> Split f a a
split = Split id id 

unsplit :: Invariant f => Split f a a -> f a
unsplit (Split h i x) = invmap i h x
```

We can even write `Strong` instance:

```haskell
instance Strong f => P.Strong (Split f) where
    first' (Split h i x) = Split (first h) (first i) (first' x)
```

So we can convert Profunctor Lens into Functor one:

```haskell
functorFirst :: Lens (a, c) a
functorFirst = unsplit . P.first' . split
```

This direction, using `Split`, isn't as elegant as other one using `Trace`,
but seems to work!

---

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)
