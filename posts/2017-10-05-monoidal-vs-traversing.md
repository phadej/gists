---
title: Monoidal vs Traversing
author: Oleg Grenrus
tags:  lens
---

In the [reddit thread](https://www.reddit.com/r/haskell/comments/73tlhs/dont_fear_the_profunctor_optics/)
about [Don't Fear The Profunctor Optics](https://github.com/hablapps/DontFearTheProfunctorOptics)
there is discussion about `Monoidal`.

Discussion is about what's `Monoidal`?

```haskell
class Profunctor p => Monoidal p where
    par   :: p a b -> p c d -> p (a, c) (b, d)
    empty :: p () ()
```

`Monoidal` is used to implement traversals.
As a concrete example we can pick lists:

```haskell
listTraversing :: (Choice p, Monoidal p) ->
               => p a b -> p [a] [b]
listTraversing k = go where
    go = dimap out inn (right (par k go))

    out :: [a] -> Either () (a, [a])
    out []     = Left ()
    out (x:xs) = Right (x, xs)

    inn :: Either () (a, [a]) -> [a]
    inn (Left ())       = []
    inn (Right (x, xs)) = x : xs
```

When `[a]` comes in, we pattern match it with `out` to convert it into `Either () (a, [a])` (and use `inn` to convert back).
We let a unit `()` pass-through by using `right` (*note*: we **need** `Choice`).
And using `Monoidal`'s `par` we can combine `k :: p a b` and recursive application
of `listTraversing` to get `p (a, [a]) (b, [b])`.

Another example: `both` for a homogenic pair. Here we don't need `Choice`!

```haskell
both :: Monoidal p => p a b -> p (a, a) (b, b)
both k = par k k
```

My point: `Monoidal` (by itself) is only powerful enough to express
traversals over `Representable` containers, and only where `Rep f` is finite!
The idea is that, as `Rep f` is finite we have a bijection between `f a` and `Vec n a`.
for some static `n :: Nat`.

```haskell
data Nat where
    Z :: Nat
    S :: Nat -> Nat

data SNat (n :: Nat) where
    SZ :: SNat 'Z
    SS :: INat n => SNat ('S n)

class              INat (n :: Nat) where inat :: SNat n
instance           INat 'Z         where inat = SZ
instance INat n => INat ('S n)     where inat = SS

data Vec n a where
    VNil  :: Vec 'Z a
    VCons :: a -> Vec n a -> Vec ('S n) a 

vecTraversing :: forall n p a b. (INat n, Monoidal p)
              => p a b -> p (Vec n a) (Vec n b)
vecTraversing k = case inat :: SNat n of
    SZ -> dimap (const ()) (const VNil) empty
    SS -> dimap out inn (par k (vecTraversing k))
  where
    out :: forall m. INat m => Vec ('S m) a -> (a, Vec m a)
    out (VCons x xs) = (x, xs)
    
    inn :: forall m. (b, Vec m b) -> Vec ('S m) b
    inn (x, xs) = VCons x xs
```

At this point point it starts to be clear that `Traversing`

```haskell
class (Choice p, Strong p) => Traversing p where
    traverse' :: Traversable f => p a b -> p (f a) (f b)

    wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
           -> p a b -> p s t
```
is more powerful than `Monoidal` alone.

It can do `Choice` (it can also do `Strong`, which `Monoidal` cannot without `arr :: p a a`!).
Quite importantly `Traversing` can handle infinite structures.
I don't have a good argument for or against whether `Traversal`s should consider
only finite structures, yet traversals over infinite structures are useful in
practice.

On the other hand, as [`davemenendez`](https://www.reddit.com/r/haskell/comments/73tlhs/dont_fear_the_profunctor_optics/dntyqas/) mentions,
we cannot define `Monoidal` using `Traversing`! Informally we could say
that the relative power is something like:

```
Monoidal < Traversing < Monoidal + Choice + Strong
```

It's a very good question, if there is *practical* difference.

On the other hand, and to conclude:
there is a [*Indexed Containers* paper](http://strictlypositive.org/indexed-containers.pdf)
discussing containers. And I think that something like that is needed so we
can have three classes of optics:

- Lenses for products
- Prisms for sums
- ??? for containers

Then we'll have solid theoretical foundation for optics (IMHO), though
probably not expressable in Haskell.
