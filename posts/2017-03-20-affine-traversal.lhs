---
title: Affine Traversal
author: Oleg Grenrus
tags: lens
---

Thanks to Matthew Pickering, I learned about affine traversals.
Affine traversal is an optic that has 0 or 1 target; the fact which
you can check by using specialised view function!
While playing with them, you have to use `Pointed` in the van Laarhoven
formulation; profunctor one doesn't suffer from this (subjective) unelegancy!

Behold, profunctors ahead:
```haskell
{-# LANGUAGE RankNTypes, TupleSections #-}
module AffineTraversal where
import Control.Applicative
import Data.Bifunctor
import Data.Default
import Data.Functor.Apply
import Data.Pointed
import Data.Profunctor
import Data.Semigroup.Traversable
```

*Affine traversal* is an optic that has 0 or 1 target
(see e.g. [`failing`](http://ekmett.github.io/lens/Control-Lens-Traversal.html#v:failing))

The simplest example would be:

```haskell
ex1 f = (_1 . _Right) f
```

The problem is that in `lens` (var Laarhoven encoding) a `Prism` is:
```haskell
type PrismVL s t a b =
    forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
```
here, `Applicative` is too restrictive,
forcing combination with
```haskell
type LensVL s t a b =
    forall f. (Functor f) => (a -> f b) -> s -> f t
```
to be full
```haskell
type TraversalVL s t a b =
    forall f. (Applicative f) => (a -> f b) -> s ->  f t
```

To define prism, the disputed [`Pointed`](http://hackage.haskell.org/package/pointed-5/docs/Data-Pointed.html)
class would be enough. In lens usage it won't be as bad, as library writer
controls how they will instantiate `f` in the optic!

**If** we had
```haskell
type PrismVL' s t a b =
    forall p f. (Choice p, Functor f, Pointed f) => p a (f b) -> p s (f t)

-- | Using 'point', not 'pure'
prismVL' :: (b -> t) -> (s -> Either t a) -> PrismVL' s t a b
prismVL' bt seta = dimap seta (either point (fmap bt)) . right'
```
then we could define
```haskell
type AffTraversalVL s t a b =
    forall p f. (Functor f, Pointed f) => (a -> f b) -> s -> f t
```
The "nice" consequence, is that
```haskell
type GettingVL r s a = (a -> Const r a) -> s -> Const r s
viewVL :: GettingVL a s a -> s -> a
viewVL l s = getConst (l Const s)
```
used on a `AffTraversalVL`
```haskell
ex1' :: AffTraversalVL (Either c a, d) (Either c b, d) a b
ex1' = _1 . _Right
```
would give
[`Default`](http://hackage.haskell.org/package/data-default-0.7.1.1/docs/Data-Default.html#t:Default)
values on non-match (and not `mempty`):
```
λ> viewVL ex1 (Right 42, True) :: Int
42
λ> viewVL ex1 (Left 'a', True) :: Int
0
```
because `Pointed (Const r)` is defined in terms of `Default r`.

Profunctor approach
-------------------

If we'd use profunctor optics, `Lens` and `Prism` are defined more uniformly:
```haskell
type LensP  s t a b = forall p. Strong p => p a b -> p s t
type PrismP s t a b = forall p. Choice p => p a b -> p s t
```
It turns out that their combination is the affine traversal!
```haskell
type AffTraversalP s t a b =
    forall p. (Strong p, Choice p) => p a b -> p s t
```

We can repeat the above example, if we define `Choice (Forget r)` using
`Default` (instances are at the end):

```haskell
newtype ForgetD r a b = ForgetD { runForgetD :: a -> r }
type GettingP r s a = ForgetD r a a -> ForgetD r s s

viewP :: GettingP a s a -> s -> a
viewP l = runForgetD (l (ForgetD id))
```
used on an optic
```haskell
ex2 :: AffTraversalP (Either c a, d) (Either c b, d) a b
ex2 = first' . right'
```
seems to work:
```
λ> viewP ex2 (Right 42, True) :: Int
42
λ> viewP ex2 (Left 'a', True) :: Int
0
```

Another approach is to define
```haskell
newtype ForgetM r a b = ForgetM { runForgetM :: a -> Maybe r }
type AffGettingP r s a = ForgetM r a a -> ForgetM r s s

affviewP :: AffGettingP a s a -> s -> Maybe a
affviewP l = runForgetM (l (ForgetM Just))
```

```
λ> affviewP ex2 (Right 42, True) :: Maybe Int
Just 42
λ> affviewP ex2 (Left 'a', True) :: Maybe Int
Nothing
```

And as `ForgetM` isn't (cannot be?) `Traversing`, this won't type-check:
```
λ> affviewP traverse' ["foo", "bar"]

<interactive>:_:10: error:
    • No instance for (Traversing (ForgetM [Char]))
```
but the `viewP` does fold:
```
λ> viewP traverse' ["foo", "bar"]
"foobar"
```

We *could* define `Traversing` instance for `ForgetM`, but we deliberately
don't, as we want that type-check failure to occur.

The profunctor approach is more elegant, as we don't need to rely on `Pointed`,
the `point` is baked into `affviewP` formulation!

I think the `AffTraversal` can be useful in practice. There are situations
where you know that there is *at most one* value, hidden inside your big
structure. By using `firstOf`, we don't enforce that fact, but we could!

Equivalence
-----------

We can show that the definitions are equivalent. We start by specifying
the constructors:

```haskell
afftraversalVL
    :: (s -> Either t a)
    -> (s -> b -> t)
    -> AffTraversalVL s t a b
afftraversalVL getter setter f s = case getter s of
    Left t  -> point t
    Right a -> (\b -> setter s b) <$> f a

afftraversalP
    :: (s -> Either t a)
    -> (s -> b -> t)
    -> AffTraversalP s t a b
afftraversalP getter setter pab = dimap r l (first' (right' pab))
  where
    r s = (getter s, setter s)
    l (Left t, _) = t
    l (Right b, g) = g b
```

Then to go from profunctor to van Laarhoven, we'll need a profunctor kiosk:

```haskell
data Kiosk a b s t = Kiosk (s -> Either t a) (s -> b -> t)

sellKiosk :: Kiosk a b a b
sellKiosk = Kiosk Right (\_ -> id)

instance Profunctor (Kiosk u v) where
    dimap f g (Kiosk getter setter) = Kiosk
        (\a -> first g $ getter (f a))
        (\a v -> g (setter (f a) v))

instance Strong (Kiosk u v) where
    first' (Kiosk getter setter) = Kiosk
        (\(a, c) -> first (,c) $ getter a)
        (\(a, c) v -> (setter a v, c))

instance Choice (Kiosk u v) where
    right' (Kiosk getter setter) = Kiosk
        (\eca -> assoc (second getter eca))
        (\eca v -> second (`setter` v) eca)
      where
        assoc :: Either a (Either b c) -> Either (Either a b) c
        assoc (Left a)          = Left (Left a)
        assoc (Right (Left b))  = Left (Right b)
        assoc (Right (Right c)) = Right c
```

Then conversion is simple, as `Kiosk` characterizes affine traversal,
we can run the profunctor optic to get getters and setters,
which in turn are used to construct van Laarhoven variant:
```haskell
toVL :: AffTraversalP s t a b -> AffTraversalVL s t a b
toVL l = afftraversalVL getter setter
  where
    Kiosk getter setter = l sellKiosk
```

To go in other direction we need a functor kiask:

```haskell
newtype Kiask a b t = Kiask { runKiask :: (Either t a, b -> t) }

sellKiask :: a -> Kiask a b b
sellKiask a = Kiask (Right a, id)

instance Functor (Kiask a b) where
    fmap f (Kiask (Left t, g)) = Kiask (Left (f t), f . g)
    fmap f (Kiask (Right a, g)) = Kiask (Right a, f . g)

instance Pointed (Kiask a b) where
    point x = Kiask (Left x, const x)
```

And the conversion functions follows the same principle as previous one:
```haskell
toP :: AffTraversalVL s t a b -> AffTraversalP s t a b
toP l = afftraversalP (fst . b) (snd . b)
  where
    b s = runKiask $ l sellKiask s
```

And those seem to work!
```
λ> viewVL (toVL ex2) (Right 42, True) :: Int
42
λ> viewVL (toVL ex2) (Left 'a', True) :: Int
0
λ> viewP (toP ex1) (Right 42, True) :: Int
42
λ> viewP (toP ex1) (Left 'a', True) :: Int
0
```

Traversing
----------

The traversal in profunctor optics is defined using `Traversing` or `Wander`
class:

```haskell
class (Choice p, Strong p) => Traversing p where
    traverse' :: Traversable f => p a b -> p (f a) (f b)
    traverse' = wander traverse

    wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
           -> p a b -> p s t
```

It's trivial to define `Wander1` for non-empty traversals:
replace `Applicative` with `Apply` and postfix `1` to the symbol names:
```haskell
class (Choice p, Strong p) => Traversing1 p where
    traverse1' :: Traversable1 f => p a b -> p (f a) (f b)
    traverse1' = wander1 traverse1

    wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t)
            -> p a b -> p s t
```

So let's go for affine traversal as well:

```haskell
class (Choice p, Strong p) => AffTraversing p where
    affwander
        :: (forall f. (Functor f, Pointed f) => (a -> f b) -> s -> f t)
        -> p a b -> p s t

    afftraverse' :: AffTraversable f => p a b -> p (f a) (f b)
    afftraverse' = affwander afftraverse
```

where `AffTraversable` has obvious definition, and `Maybe` is a canonical instance:

```haskell
class Functor t => AffTraversable t where
    afftraverse :: (Functor f, Pointed f) => (a -> f b) -> t a -> f (t b)

instance AffTraversable Maybe where
    afftraverse _ Nothing  = point Nothing
    afftraverse f (Just x) = Just <$> f x
```

Using the conversion functions from the previous section, we can show that
```haskell
type X s t a b = forall p. (Strong p, Choice p) => p a b -> p s t
type Y s t a b = forall p. (AffTraversing p)    => p a b -> p s t
```
are isomorphic! It's 10 at the morning, so I don't try to conclude from that
`AffTraversing p` is equivalent to `(Strong p, Choice p)`, but they are close.
And what that means to the `Default` & `Pointed` story?

| Class | Functor | Container | Example | Cat |
| --- | --- | --- | --- | --- |
| Default   | Pointed     | *AffTraversable* | Maybe    | ?            |
| Semigroup | Apply       | Traversable1     | NonEmpty | Semigroupoid |
| Monoid    | Applicative | Traversable      | []       | Category     |

Lens with Maybe
-------------

`AffTraversal` isn't equaivalent to `Lens s t (Maybe a) (Maybe b)`
as the latter let you *remove* values from the structure (e.g. `at`).

The `Lens s t (Maybe a) b` is closer:

```haskell
ex3 :: LensVL (Either c a, d) (Either c b, d) (Maybe a) b
ex3 = lensVL getter setter
  where
    getter (Right a, _) = Just a
    getter (_, _)       = Nothing

    setter (Right _, d) b = (Right b, d)
    setter (Left x, d)  b = (Left x, d)
```
but doesn't seem practical (and needs differently specified lens laws!)

Appendix
--------

Simple van Laarhoven optics:

```haskell
lensVL :: (s -> a) -> (s -> b -> t) -> LensVL s t a b
lensVL sa sbt afb s = sbt s <$> afb (sa s)

_1 :: LensVL (a, c) (b, c) a b
_1 f (a, c) = flip (,) c <$> f a

_Right :: PrismVL' (Either c a) (Either c b) a b
_Right = prismVL' Right $ \e -> case e of
    Left c -> Left (Left c)
    Right a -> Right a
```

<h3>Forgotten instances</h3>

```haskell
instance Profunctor (ForgetD r) where
    dimap f _ (ForgetD k) = ForgetD (k . f)

instance Strong (ForgetD r) where
    first' (ForgetD k) = ForgetD (k . fst)

-- | 'Default', not 'Monoid'
instance Default r => Choice (ForgetD r) where
    left' (ForgetD k) = ForgetD (either k (const def))

instance (Default r, Monoid r) => Traversing (ForgetD r) where
    wander f (ForgetD k) = ForgetD $ \s ->
        getConst (f (Const . k) s)


instance Profunctor (ForgetM r) where
    dimap f _ (ForgetM k) = ForgetM (k . f)

instance Strong (ForgetM r) where
    first' (ForgetM k) = ForgetM (k . fst)

instance Choice (ForgetM r) where
    left' (ForgetM k) = ForgetM (either k (const Nothing))
```

---

See discussion in [r/haskell](https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/)

---

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
λ> :l affine-traversal.lhs
```
fetch the source from
[https://gist.github.com/phadej/0280d0748c7f3205daf7de07cc4dd7d0](https://gist.github.com/phadej/0280d0748c7f3205daf7de07cc4dd7d0)
