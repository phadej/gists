---
title: Singleton containers
author: Oleg Grenrus
tags: lens, notes
---

Singleton containers, all `t a` values have a single `a` value in it.
We can use `peekaboo` as a `Lens` to extract the value inside.

```haskell
class Traversable t => Singleton t where
    peekaboo :: Functor f => (a -> f b) -> t a -> f (t b)
```

There're plenty of instances, for example:

```haskell
instance Singleton Identity where
    peekaboo f (Identity x)  = fmap Identity (f x)

instance Singleton ((,) a) where
    peekaboo f (a, b)        = fmap (\x -> (a , x)) (f b)

instance (Singleton f, Singleton g) => Singleton (Compose f g) where
    peekaboo f (Compose x)   = fmap Compose (peekaboo (peekaboo f) x)
```

We can use `Singleton` to define `Strong`, the class used to characterise 
`Lens` in profunctor optics.

```haskell
class Profunctor p => Strong' p where
    peekaboo' :: Singleton f => p a b -> p (f a) (f b)
```

`Strong` and `Strong'` are equivalent, from `Strong'` direction is trivial:

```haskell
newtype WrappedStrong' p a b = WrapStrong' { unwrapStrong' :: p a b }
instance Profunctor p => Profunctor (WrappedStrong' p) where
    dimap f g = WrapStrong' . dimap f g . unwrapStrong'
instance Strong' p => Strong (WrappedStrong' p) where
    second' = WrapStrong' . peekaboo' . unwrapStrong'
```

but the other is more involved:

```haskell
newtype WrappedStrong p a b = WrapStrong { unwrapStrong :: p a b }
instance Profunctor p => Profunctor (WrappedStrong p) where
    dimap f g = WrapStrong . dimap f g . unwrapStrong
instance Strong p => Strong' (WrappedStrong p) where
    peekaboo'  =  WrapStrong
               .  dimap (\x -> (x, view peekaboo x))
                        (\(fa, b) -> fmap (const b) fa)
               .  second' . unwrapStrong
```
