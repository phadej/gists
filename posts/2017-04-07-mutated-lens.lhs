---
title: Mutated lenses
author: Oleg Grenrus
tags: lens
---

It's not an April's Fool day anymore, but don't take this seriously anyway.
Tongue in cheek literate Haskell post on lenses (if interested in more serious ones, see
[Compiling Lenses](2017-03-31-compiling-lenses.html),
and [Affine Traversal](http://oleg.fi/gists/posts/2017-03-20-affine-traversal.html)
).

A quote by [metafunctor](https://www.reddit.com/r/haskell/comments/62jv9g/compiling_lenses/dfycbd5/?utm_content=permalink&utm_medium=front&utm_source=reddit&utm_name=haskell)
<blockquote>
Haskell is so advanced that a inordinate amount of literature is devoted to
what in any standard language would be simple getters setters and loops.
</blockquote>
For loops are too much for me, but getters and setters we can do (in Haskell).

<div id="toc"></div>

Preamble
--------

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MutatedLens where
import Control.Monad.ST
import Control.Lens
import Data.STRef
```

Mutated Lens
-------

[`readSTRef`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-STRef.html#v:readSTRef)
and [`writeSTRef`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-STRef.html#v:writeSTRef)
types are just right so we can pass them into [`lens`](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Lens.html#v:lens)

```haskell
stref :: Lens (STRef s a) (ST s ()) (ST s a) a
stref = lens readSTRef writeSTRef
```

Unfortunately the type of [`view`](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Getter.html#v:view) is too restrictive, so we have
to define a polymorphic version:

```haskell
type GettingP r s t a b = (a -> Const r b) -> s -> Const r t

viewP :: GettingP a s t a b -> s -> a
viewP f s = getConst (f Const s)
```

Seems to work:

```
λ> :t viewP stref 
viewP stref :: STRef s a -> ST s a
```

Example
-------

Let's define an imperative record, with mutable fields:

```haskell
data ImpRecord s = ImpRecord
    { _intField  :: STRef s Int
    , _boolField :: STRef s Bool
    }
```

and a "lens" into the first field:

```haskell
intField :: Lens (ImpRecord s) (ST s ()) (STRef s Int) (ST s ())
intField = lens _intField (\_ -> id)
```

Now we can *read* and *write* through `intField` and `stref`.
First we create a record, then read its field; after that we mutate
the field and read it again:

```haskell
program :: ST s (Int, Int)
program = do
    iRef <- newSTRef 1
    bRef <- newSTRef True
    let mrecord = ImpRecord iRef bRef

    i1 <- viewP (intField . stref) mrecord

    set (intField . stref) 2 mrecord
    i2 <- viewP (intField . stref) mrecord

    pure (i1, i2)
```

If we run the program, it works:

```
λ> print (runST program)
(1,2)
```

The problem with this approach, is that we cannot drill through `STRef s (STRef s) ...`.

Serious bit
-----------

Normal lens are in `(->)`, but we can work in `Kleisli (ST s)` too. So
if we change all normal arrows to `Kleisli (ST s)` (yet unwrapped),
we can define `STLens`:

```haskell
type STLens z s t a b = forall f. MonadTransFunctor f =>
    (ST z a -> f (ST z) b) -> s -> f (ST z) t

class MonadTransFunctor t where
    tfmap    :: Monad m => (a -> m b) -> t m a -> t m b
    semibind :: Monad m => m a -> (a -> t m b) -> t m b

stlens :: (s -> ST z a) -> (s -> b -> ST z t) -> STLens z s t a b
stlens getter setter f s = tfmap (setter s) (f (getter s))
```

They compose:

```haskell
infixr 8 `o`
o :: STLens z s t a b -> STLens z a b u v -> STLens z s t u v
o stab abuv f s = stab (\sta -> semibind sta $ \a -> abuv f a) s
```

and we can redefine `stref'`:

```haskell
stref' :: STLens z (STRef z a) (STRef z a) a a
stref' = stlens readSTRef (\s b -> writeSTRef s b >> pure s)
```

The following definitions for `stview` and `stover` are very similar to
`lens` counterparts.

<h3 id="stgetting">STGetting</h3>

```haskell
newtype ConstT r (m :: * -> *) a = ConstT { getConstT :: m r }

instance MonadTransFunctor (ConstT r) where
    tfmap f (ConstT r) = ConstT r
    semibind m k = ConstT $ m >>= getConstT . k

type STGetting r z s a =
    (ST z a -> ConstT r (ST z) a) -> s -> ConstT r (ST z) s

stview :: STGetting a z s a -> s -> ST z a
stview l s = getConstT (l ConstT s)
```

<h3 id="stsetter">STSetter</h3>

```haskell
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

type STSetter z s t a b =
    (ST z a -> IdentityT (ST z) b) -> s -> IdentityT (ST z) t

instance MonadTransFunctor IdentityT where
    tfmap f (IdentityT x) = IdentityT (x >>= f)
    semibind m k = IdentityT (m >>= runIdentityT . k)

stover :: STSetter z s t a b -> (a -> ST z b) -> s -> ST z t
stover l f s = runIdentityT (l (\a -> IdentityT (a >>= f)) s)

stset :: STSetter z s t a b -> s -> b -> ST z t
stset l s b = stover l (\_ -> pure b) s
```

<h3 id="Example">Example</h3>

And we can now drill through multiple `STRef`!

```haskell
program2 :: ST s (Int, Int)
program2 = do
    ref1 <- newSTRef 42
    ref2 <- newSTRef ref1

    i1 <- stview (stref' `o` stref') ref2

    _ <- stset (stref' `o` stref') ref2 99
    i2 <- stview (stref' `o` stref') ref2

    pure (i1, i2)
```

```
λ> print (runST program2)
(42,99)
```

Conclusion
----------

So, yet another example that Haskell is good imperative language.
And we can have controlled effects in imperative language, you just need
expressive enough language. :)

---

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
λ> :l mutated-lenses.lhs
```
fetch the source from
[https://gist.github.com/phadej/a74f0d14749a352b9b3430c10bf35706](https://gist.github.com/phadej/a74f0d14749a352b9b3430c10bf35706)
