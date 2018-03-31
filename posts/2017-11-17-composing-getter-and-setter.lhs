---
title: Composing Getter and Setter
author: Oleg Grenrus
tags:  lens
---

Adam Gundry gave a talk about optics at Haskell Exchange 2017,
[*Through a Glass, Abstractly: Lenses and the Power of Abstraction*](https://skillsmatter.com/skillscasts/10692-through-a-glass-abstractly-lenses-and-the-power-of-abstraction).
One take-away point from the talk is, that you want composition of `Getter` and `Setter`
to be an error. You can compose getters with setters, in teir van Laarhoven or
profunctor encoding of optics, but you can't do anything *useful*.
So you'd rather fail early.

Except, it's Friday.

As this is a literal Haskell file, First a moderate prelude:

```haskell
{-# LANGUAGE RankNTypes, DeriveFunctor #-}
module ComposeGetterSetter where

import Control.DeepSeq
import Data.Boring (Boring (..))
import Data.Functor.Identity
import Data.Distributive
import Data.Profunctor hiding (Mapping (..))
import Data.Profunctor.Traversing hiding (Mapping (..))
```

Let's work with *profunctor* encoding of optics, then

```haskell
type Optic' p s a = p a a -> p s s
type Setter' s a = forall p. Mapping p => Optic' p s a
type Getter s a = forall p. (Bicontravariant p, Strong p) => Optic' p s a 
```

If we try to compose `Getter` and `Setter`, it will succeed, we'll get
``` haskell
type GetterSetter s a =
    forall p. (Bicontravariant p, Mapping p) => Optic' p s a
```

What would satisfy that? Well, a [`Boring`](http://hackage.haskell.org/package/boring) type:

```haskell
newtype Boom a b = Boom { runBoom :: a -> () } 

instance Boring (Boom a b) where
    boring = Boom boring
```

How that could be useful?
[By fast and loose reasoning](https://dl.acm.org/citation.cfm?id=1111056) it won't.
But in Haskell, we also have `seq`.

If we take care, and write instances which **force** the arguments:

```haskell
instance Profunctor Boom where
    dimap f _ (Boom h) = Boom (h . f)

instance Strong Boom where
    first' (Boom f) = Boom $ \(a, _) -> f a

instance Choice Boom where
    right' (Boom f) = Boom $ \x -> case x of
        Right y -> f y
        Left  _ -> ()

instance Bicontravariant Boom where
    cimap f _ (Boom h) = Boom (h . f)

instance Closed Boom where
    closed (Boom f) = Boom $ \_ -> ()

instance Traversing Boom where
    wander f (Boom g) = Boom $ \s -> f (\x -> g x `seq` Proxy') s `seq` ()

instance Mapping Boom where
    map' (Boom g) = Boom $ \fa -> fmap g fa `seq` ()
    roam f (Boom g) = Boom $ \s -> f (\x -> g x `seq` Proxy') s `seq` ()
```

by using **strict** `Proxy'`:

```haskell
data Proxy' a = Proxy' deriving Show

instance Functor Proxy' where
    fmap _ Proxy' = Proxy'

instance Applicative Proxy' where
    pure x = x `seq` Proxy'
    Proxy' <*> Proxy' = Proxy'

instance Distributive Proxy' where
    collect agb fa = Proxy'
```

we can define something interesting (maybe):

```haskell
partialNF :: NFData a => Optic' Boom s a -> s -> ()
partialNF l s = runBoom (l (Boom rnf)) s
```

`partialNF` will use an optic to drill inside the structure, and evaluate the values inside.

Examples
--------

First example is using `Stream`:

```haskell
infixr 5 :::
data Stream a = a ::: Stream a deriving (Show)
```

Let's us define some optics, 'Getter' for the head, and 'Setter' for the tail of the 'Stream'.
We don't define `tl` using `setting`, we'll see later why so.

```haskell
hd :: Getter (Stream a) a
hd = to (\(x ::: _) -> x)

tl :: Setter' (Stream a) (Stream a)
tl = dimap (\(x ::: xs) -> (x, xs)) (uncurry (:::)) . second'
```

Now we can play with these:
```
λ> partialNF hd $ let s = () ::: s in s
()
```
that's quite boring.

How about:

```
λ> partialNF hd $ let s = () ::: error "friday" ::: s in s
()
```

or 

```
λ> partialNF (tl . tl . hd) $ let s = () ::: error "friday" ::: s in s
()
```

Still, quite boring.

```
λ> partialNF (tl . hd) $ let s = () ::: error "friday" ::: s in s
*** Exception: friday
```

BOOM!

However, if we'd define:

```haskell
tl' :: Setter' (Stream a) (Stream a)
tl' = setting $ \f (x ::: xs) -> x ::: f xs
```

Then the *magic of laziness*  will prevent us from
doing bad stuff :)

```
λ> partialNF (tl' . hd) $ let s = () ::: error "friday" ::: s in s
()
```

For the same reason 

```
λ> partialNF (map' . first') [(1,2), (error "friday!",4)]
()
```

doesn't throw, but

```
λ> partialNF (traverse' . first') [(1,2), (error "friday!",4)]
*** Exception: friday!
```

does.

To conclude, a composition of `Getter` and `Setter` has at least one,
though quite questionable use case. However, partial normalisation doesn't if
a `Setter` is defined using `map'` or `roam`, as they don't give opportunity
to force the values "inside" the `Functor`.

Appendix
--------

Various definitions

```haskell
-- type aliases
type Optic p s t a b = p a b -> p s t
type Lens' s a = forall p. Strong p => Optic' p s a

-- Mapping with roam
class (Traversing p, Closed p) => Mapping p where
    map' :: Functor f => p a b -> p (f a) (f b)
    map' = roam collect

    roam :: (forall f. (Applicative f,  Distributive f)
         => (a -> f b) -> s -> f t)
         -> p a b -> p s t
    roam = roamMap' map'

    {-# MINIMAL map' | roam #-}

roamMap' :: Profunctor p
         => (forall f. Functor f => p a b -> p (f a) (f b))  -- ^ map'
         -> (forall f. (Distributive f, Applicative f)
                     => (a -> f b) -> s -> f t)
         -> p a b -> p s t
roamMap' m f = dimap (\s -> Bar $ \afb -> f afb s) lent . m
  where
    lent :: Bar t a a -> t
    lent m = runIdentity (runBar m Identity)

newtype Bar t b a = Bar
   { runBar :: forall f. (Distributive f, Applicative f)
            => (a -> f b) -> f t
   }
   deriving Functor

-- Bicontravariant
class Bicontravariant p where
    cimap :: (b -> a) -> (d -> c) -> p a c -> p b d

-- Constructors
to :: Bicontravariant p => (s -> a) -> Optic' p s a
to f = cimap f f

setting :: Mapping p => ((a -> b) -> s -> t) -> Optic p s t a b
setting f = dimap (Context id) (\(Context g s) -> f g s) . map'

data Context a b t = Context (b -> t) a deriving Functor

-- Lenses for Stream
hdl :: Lens' (Stream a) a
hdl = dimap (\(x ::: xs) -> (x, xs)) (uncurry (:::)) . first'

tll :: Lens' (Stream a) (Stream a)
tll = dimap (\(x ::: xs) -> (x, xs)) (uncurry (:::)) . second'
```
