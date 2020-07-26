---
title: "Coerced: new optic kind"
author: Oleg Grenrus
tags: lens
---

Combining
[`Coercible`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Coerce.html#t:Coercible)
and
[`QuantifiedConstraints`](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#quantified-constraints)
to define new kind of optics, which I call `Coerced`.

And when we define it, we run into many old problems.

Prologue
--------

Small prologue: look ma, under ten extensions.

```haskell
{-# LANGUAGE TypeOperators, GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
import Data.Coerce
import Data.Type.Equality
import Data.Type.Coercion
import Data.Profunctor
```

This is a post about optics, and I will use profunctor encoding.

```haskell
type Optic p s t a b = p a b -> p s t
```

Isomorphisms
------------

In `optics` library they are the bottom of optics kind ordering.

That choice was made to make all optics parametric.
(we'll get to that means)

```haskell
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
```

With isomorphism we can 'view' (because all isomorphisms are lens) 

```
view :: Iso  s t a b -> s -> a
view :: Lens s t a b -> s -> a
```

```haskell
view
    :: forall s a. Optic (Forget a) s s a a -> s -> a
view l s =
    -- here and later I write (redundant) type signatures to show
    -- how function representing optic transforms a thingy
    --
    --     newtype Forget r a b = Forget (a -> r)
    --
    case l (Forget id :: Forget a a a) :: Forget a s s of
        Forget f -> f s
```

The special thing about isomorphisms, is that we can reverse them.

```
re :: Iso s t a b -> Iso b a t s
```

```haskell
re :: forall p a b s t. Optic (Re p a b) s t a b -> Optic p b a t s
re l =
    case l (Re id :: Re p a b a b) of
        Re l' -> l'
```

Note the pattern. For each operation we have to write a concrete (new)type.
For view we used `Forget`, for `re` we have `Re`.

```haskell
newtype Re p a b s t = Re (p t s -> p b a)
```

What instances we can write for these particular types,
determines what optics can go in (or out).

E.g. `Re p` is `Profunctor` if `p` is,
therefore on of `re` types is `Iso s t a b -> Iso b a t s`.

```haskell
instance Profunctor p => Profunctor (Re p s t) where
    dimap f g (Re p) = Re (p . dimap g f)
```

But there are other instances too. See
https://hackage.haskell.org/package/optics-core-0.3/docs/src/Optics.Re.html#Re

In `lens`, this is complicated by VL encoding. There we have `from` and `re`,
which are the same operation, but for different optic kinds.
    
Equality
--------

Isomorphism are not the true bottom of the optic order.
If we don't require _anything_ from `p`, we get an `Equality`.

```haskell
type Equality s t a b = forall p. Optic p s t a b
```

Equality is so powerful that we can transform, surprise, equalities ([`:~:`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Type-Equality.html#t::-126-:)):

```
equalityUpcast :: Equality s t a b -> a :~ :b -> s :~: b
```

```haskell
equalityUpcast
    :: forall s t a b. Optic (Identical a b) s t a b -> a :~: b -> s :~: t
equalityUpcast l Refl =
    case l (MkIdentical :: Identical a b a b) :: Identical a b s t of
        MkIdentical -> Refl

-- double :~:
data Identical a b s t where
    MkIdentical :: Identical a a s s
```

On the other hand, we cannot really construct anything else than
(This is more useful than you think, especially in `optics` where you cannot just use `id`):

```haskell
equality :: Equality s t s t
equality = id
```

Coerced
-------

Is there anything in between of 'Isomorphism' and 'Equality'?

The GHC-7.8 introduced roles and coercions.
GHC-8.6 introduced [quantified constraints](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#quantified-constraints).
We can put them together and get `Coerced`, a new kind of optics:

```haskell
type Coerced s t a b = forall p.
    (forall u v x y. (Coercible u v, Coercible x y)
        => Coercible (p u x) (p v y))
    => Optic p s t a b
```

As 'Equality' transforms equalities (`:~:`),
`Coerced` transforms ['Coercion'](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Type-Coercion.html#t:Coercion)s.

```
coercionUpcast :: Coerced s t a b -> Coercion a b -> Coercion s t
```

```haskell
coercionUpcast
    :: forall s t a b. Optic (Coercion2 a b) s t a b
    -> Coercion a b -> Coercion s t
coercionUpcast l Coercion =
    -- This is analogous to coercionUpcast
    case l (MkCoercion2 :: Coercion2 a b a b) :: Coercion2 a b s t of
        MkCoercion2 -> Coercion

data Coercion2 a b s t where
    MkCoercion2 :: (Coercible a b, Coercible s t) => Coercion2 a b s t
```

In `lens` and `optics` there is `Isomorphism`.

```haskell
coerced :: (Coercible s a, Coercible b t) => Iso s t a b
coerced = dimap coerce coerce
```

But in fact, 'coerced' is 'Coerced':

```haskell
maybeBetterCoerced :: (Coercible s a, Coercible b t) => Coerced s t a b
maybeBetterCoerced = coerce
```

/Note:/ Both `Equality` and `Coerced` are singleton types.
There is at most one value of type `Equality s t a b` or `Coerced s t a b`
for every fixed combination of `s`, `t`, `a` and `b`,
namely `id` or `coerce`, respectively.

Every `Equality` is `Coerced`.

```haskell
toCoerced :: Equality s t a b -> Coerced s t a b
toCoerced = id
```

A Problem
---------

But `Coerced` is not an `Isomorphism`!!!

This because the "`Coercible2"` (quantified) constraint
is not a superclass of `Profunctor`.
This problem is deep one, we would need to have similar quantified constraint
on `Functor`.

That would simplify
[some other things too](https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/),
not only make optics and lens nicer.

I'm more and more increasingly convinced that
`forall x y. Coercible x y => Coercible (f x) (f y)` should
be a constraint on `Functor`.

GHC Haskell is facing a dilemma.
Is it academic or industrial implementation?
Industrial one would never introduce this change, because some things will break.
Academic one would embrace new functionality, push for it,
and deal with fallout, which (I'm quite sure) will need some new research.

Quoting [Edwards comment](https://github.com/ekmett/profunctors/pull/70#discussion_r456211453)

<blockquote>
<p>
That isn't the only caveat. You'd need to fix that all the way down to `Functor`
and then folks who build 'moral' functor instances that use manual `Map`
constructors will (rightly) complain.
</p>

<p>...</p>

<p>
... without ruling out those "moral" functors, which pop up when folks do
things like pipes and want to have manual mapping layers and 'lift' layers in
their monad transformer stack and quotient them out in the interpreter.
</p>
</blockquote>

This is a good point. The `Proxy` type in `pipes` is

```haskell
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
```

It could be `representational` in all its arguments,
if we had higher-order roles in GHC Haskell.
(This is the "more research" required I was referring to).

The `M` constructor (which wraps `Proxy` in `m`),
makes every argument nominal, but if `m` itself had a role
`forall x y.Coercible x y => Coercible (m x) (m y)`,
then there wouldn't be problems.

And then, even if `Functor` had the quantified `Coercible` constraint,
we still could define

```haskell
instance Functor m => Functor (Proxy a' a b' b m) where
```

In fact, we won't need to change it at all, as `Functor m` context
would supply new extra assumptions needed.

There is a [Higher-order roles GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/233),
and even I don't like the solution proposed there
(we shouldn't need to change the definition of `Proxy`),
I believe that some solution will be eventually implemented.

That would also allow the `Optic` type in `optics` become
representational in its for `Type`-type arguments.
Currently we workaround that (so some things aren't simply `coerce`d),
and provide explicit coercion functions
in [`Optics.Coerce`](https://hackage.haskell.org/package/optics-core-0.3/docs/Optics-Coerce.html).

Exercises
---------

- Write a VL version of `Coerced`.
