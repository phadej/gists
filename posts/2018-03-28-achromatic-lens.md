---
title: Achromatic lens
author: Oleg Grenrus
tags: lens
---

Russel O'Connor mentionend on `#haskell-lens` about Guillaume Boisseau's masters thesis,
which mentions **Achromatic lenses**:

<blockquote>
oconnor: Guillaume also has a nice derivation of achromatic lenes.
</blockquote>

Because the thesis is not online, we have to guess a little (while waiting for
https://2018.programmingconference.org/event/bx-2018-papers-understanding-profunctor-optics-a-representation-theorem).

This is post is my short notes on the topic, it's a brain dump (which type-checks though).
More Profunctor Optics.

<div id="toc"></div>

```haskell
module Achromatic where

import Data.Profunctor (Profunctor (..), Forget (..))
import Data.Tagged (Tagged (..))
import Data.Function ((&))
import Data.Default (def)

type Optic p s t a b = p a b -> p s t

-- Classifies achromatic lenses
class Profunctor p => PointedCartesian p where
    pointedSecond :: p a b -> p (Maybe c, a) (Maybe c, b)

class PointedCartesian p => Strong p where
    second' :: p a b -> p (c, a) (c, b)
```

Operations
----------

<blockquote>
roconnor: A lost of the bidirectional programming folks like a variant of lenses that have get, put, and create.
</blockquote>

Achromatic lens has the operations of `lens`:

``` {.haskell .ignore}
view :: Optic _ s s a a -> s -> a
over :: Optic _ s t a b -> (a -> b) -> s -> t
```

but additionally it supports

``` {.haskell .ignore}
review :: Optic _ t t b b -> b -> t
```

Or so what the type says. Let's try to implement these functions.
We can check from the [Glassery](2017-04-18-glassery.html) that we need
`PointedCartesian` instance for `Forget a`, `(->)` and `Tagged` (for `view`, `over` and `review` respectively).

```haskell
instance PointedCartesian (Forget a) where
    pointedSecond = second'

instance PointedCartesian (->) where
    pointedSecond = second'

-- Tagged isn't Strong!
instance PointedCartesian Tagged where
    pointedSecond (Tagged b) = Tagged (Nothing, b)
```

Upsert
------

Three characterizing functions is difficult, so we can get away with two:
`view` and

```haskell
upsert' :: Optic Upsert s t a b -> b -> Maybe s -> t
upsert' p b = runUpsert (p (Upsert (const b)))
```

or even better:

```haskell
upsert :: Optic Upsert s t a b -> (Maybe a -> b) -> Maybe s -> t
upsert p = runUpsert . p . Upsert
```

implemented using an auxiliary `Profunctor`:

```haskell
newtype Upsert a b = Upsert { runUpsert :: Maybe a -> b }

instance Profunctor Upsert where
    dimap f g (Upsert ab) = Upsert (g . ab . fmap f)

-- Not code-golfed:
instance PointedCartesian Upsert where
    pointedSecond (Upsert mab) = Upsert $ \x -> case x of
        Nothing      -> (Nothing, mab Nothing)
        Just (mc, b) -> (mc,      mab (Just b))
```

Another option is to use isomophic type which is product of `(->)` and `Tagged`.

```haskell
data Upsert' a b = Upsert' (a -> b) b
```

**Exercise:** Prove that in [bicartesian closed category](https://ncatlab.org/nlab/show/bicartesian+closed+category):

$$
B^A \times B \cong B^{A + 1}
$$

Constructor
-----------

We can create a constructor from `view` and `upsert` like functions:

```haskell
achroma
    :: PointedCartesian p
    => (s -> a)             -- ^ view
    -> (Maybe s -> b -> t)  -- ^ upsert
    -> Optic p s t a b
achroma sa msbt
    = dimap (\s -> (Just s, sa s)) (uncurry msbt)
    . pointedSecond
```

Example
-------

Let's play with cats:

```haskell
data Cat = Cat
    { _catName  :: String
    , _catOwner :: Maybe String
    , _catLifes :: Maybe Int
    }
  deriving Show
```

We can define an achromatic lens into name of a cat:

```haskell
catName :: PointedCartesian p => Optic p Cat Cat String String
catName = achroma _catName $ \ms n ->
    maybe (Cat n Nothing Nothing) (\s -> s {_catName = n }) ms
```

And then we can write use `catName` as a constructor, getter or setter:

```haskell
-- >>> felix
-- Cat {_catName = "Felix", _catOwner = Nothing, _catLifes = Just 7}
--
-- >>> felix & catName .~ "Haskell"
-- Cat {_catName = "Haskell", _catOwner = Nothing, _catLifes = Just 7}
--
-- >>> felix ^. catName
--"Felix"
--
felix :: Cat
felix
    = review catName "Felix"
    & catLifes ?~ 7
```

Laws
----

Obviously the Lens laws:
``` {.haskell .ignore}
view l (set l v s)   ≡ v
set l (view l s) s   ≡ s
set l v' (set l v s) ≡ set l v' s
```

Then we can adopt the first prism (or iso) law:

``` {.haskell .ignore}
view l (review l b) ≡ b
```

Note: that second prism law dowsn't hold

``` {.haskell .ignore}
review l (view l s) ≢ s
```

with a counterexample

``` {.haskell .ignore}
felix =
    Cat {_catName = "Felix", _catOwner = Nothing, _catLifes = Just 7}
review catName (view catName felix) =
    Cat {_catName = "Felix", _catOwner = Nothing, _catLifes = Nothing}
```

so an *achromatic lens* is not a *prism*.

**Exercise:** Formulate laws using `view` and `upsert`.

Default
-------

<blockquote>
roconnor phadej: Guillaume's masters thesis suggests an API where the Default class is used for field types for achromatic lenses.
</blockquote>

We can rewrite the example achromatic lens with `def` from `Default`.

```haskell
catName' :: PointedCartesian p => Optic p Cat Cat String String
catName' = achroma _catName $ \ms n ->
    maybe (Cat n def def) (\s -> s {_catName = n }) ms
```

Because `instance Default (Maybe a) where def = Nothing`, this variant
is the same as above where explicit `Nothing` is used.

You indeed don't need `Eq` or anything like that. (I was wrong when you check the IRC logs).

van Laarhoven
-------------

Looks like we have to use `PointedCartesian` also in *van Laarhoven* formulation (the one `lens` library uses)
of achromatic lens.

```haskell
type OpticVL p f s t a b = p a (f b) -> p s (f t)

achromaVL
    :: (PointedCartesian p, Functor f)
    => (s -> a)             -- ^ view
    -> (Maybe s -> b -> t)  -- ^ upsert
    -> OpticVL  p f s t a b
achromaVL sa msbt
    = dimap (\s -> (Just s, sa s))(\(s, fb) -> fmap (msbt s) fb)
    . pointedSecond
```

We can define `catNameVL` achromatic lens as in above examples:

```haskell
catNameVL
    :: (PointedCartesian p, Functor f)
    => OpticVL p f Cat Cat String String
catNameVL = achromaVL _catName $ \ms n ->
    maybe (Cat n def def) (\s -> s {_catName = n }) ms
```

And this definition works with `lens` library out-of-the-box:

``` {.haskell .ignore}
import qualified Control.Lens          as L
import qualified Data.Generics.Product as G -- generic-lens

>>> L.review catNameVL "felix"
Cat {_catName = "felix", _catOwner = Nothing, _catLifes = Nothing}

>>> let felix2 = L.review catNameVL "felix" & G.field @"_catLifes" L.?~ 7
>>> felix2
Cat {_catName = "felix", _catOwner = Nothing, _catLifes = Just 7}

>>> felix2 & catNameVL L..~ "Haskell"
Cat {_catName = "Haskell", _catOwner = Nothing, _catLifes = Just 7}

>>> felix2 L.^. catNameVL
"felix"
```

Appendix
--------

Definitions used above, but which are standard profunctor optics stuff.

```haskell
review :: Optic Tagged t t b b -> b -> t
review o b = unTagged (o (Tagged b))

infixr 4 ?~
(?~) :: Optic (->) s t a (Maybe b) -> b -> s -> t
(?~) l b = l (const (Just b))

infixr 4 .~
(.~) :: Optic (->) s t a b -> b -> s -> t
(.~) l b = l (const b)

infixl 8 ^.
(^.) :: s -> Optic (Forget a) s s a a -> a
(^.) = flip view

view :: Optic (Forget a) s s a a -> s -> a
view l = runForget (l (Forget id))

catLifes  :: Strong p => Optic p Cat Cat (Maybe Int) (Maybe Int)
catLifes
    = dimap (\(Cat n o l) -> ((n, o), l)) (\((n, o), l) -> Cat n o l)
    . second'

instance Strong (->) where
    second' f (c, a) = (c, f a)

instance Strong (Forget a) where
    second' (Forget p) = Forget (p . snd)
```

---

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)
