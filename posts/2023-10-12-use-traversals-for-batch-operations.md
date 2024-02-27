---
title: Use traversals for batch operations
author: Oleg Grenrus
---

Often enough we have an API which may (or need) to provide a batch operation:
"give me many inputs, and I'll give you many outputs".

For example, [`shake`](https://hackage.haskell.org/package/shake) has operators
like

```haskell
-- Define a rule for building multiple files at the same time.
(&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
```


And the usage looks like

```haskell
["*.o","*.hi"] &%> \[o,hi] -> do
    let hs = o -<.> "hs"
    need ... -- all files the .hs import
    cmd "ghc -c" [hs]
```

but that is *terrible*. `\[o, hi] -> ...` is a incomplete pattern match.
Recent GHCs included `-Wincomplete-uni-patterns` in `-Wall`:

```
warning: [GHC-62161] [-Wincomplete-uni-patterns]
    Pattern match(es) are non-exhaustive
```

There is a relation: the inputs and outputs counts should match, but that is not
encoded in the types, so compiler cannot know.

---

One option is to use [`Vec`](https://hackage.haskell.org/package/vec-0.5/docs/Data-Vec-Lazy.html#t:Vec):

```haskell
(&%>) :: Vec n FilePattern -> (Vec n FilePath -> Action ()) -> Rules ()
```

Here it's clear that the output count will match the input count.

Than the usage will look like:

```haskell
("*.o" ::: "*.hi" ::: Nil) &%> \(o ::: hi ::: Nil) -> do
    let hs = o -<.> "hs"
    need ... -- all files the .hs import
    cmd "ghc -c" [hs]
```

This is slightly more noisy[^1] but the pattern match is *complete*.

[^1]: And if we had different `OverloadedLists`, this could look like previous,
though I'm not aware if anyone figure how to do overloaded pattern matches for
list-like structures so that `Vec` would fit it too.

---

Another alternative is to use *traversals*.

```haskell
(&%>) :: Traversable t
      => t FilePattern -> (t FilePath -> Action ()) -> Rules ()
```

This abstracts over both previous usages.
You may use `Vec`s if you really don't like (turning off) the incomplete
pattern warnings.
Or you may continue use lists, as lists are `Traversable`,
and the signature of this variant of `(&%>)` tells (and restricts the implementation)
to just *traversing* the structure.

You can go one step further and use [`Each` class from `lens`](https://hackage.haskell.org/package/lens-5.2.3/docs/Control-Lens-Each.html#t:Each)[^2],
which generalises `Traversable`:

```haskell
(&%>) :: Each FilePattern FilePath ps fs
      => ps -> (fs -> Action ()) -> Rules ()
```

As `Each` has special instance for tuples (forcing them to be homogeneous),
our running example can be written neatly as:

```haskell
("*.o","*.hi") &%> \(o,hi) -> do
    let hs = o -<.> "hs"
    need ... -- all files the .hs import
    cmd "ghc -c" [hs]
```

[^2]: Which I'd like to split out into own tiny package https://github.com/ekmett/lens/issues/1050

---

Each traversal `:: Applicative f => (a -> f b) -> s -> f t`
can be converted into a `s -> FunList a b t` function and back:

```haskell
data FunList a b t = Done t
                   | More a (FunList a b (b -> t))

-- this can be done more efficent using Curried Yoneda,
-- without using `append`.
-- See https://dl.acm.org/doi/10.1145/3236780
-- and https://gist.github.com/phadej/f5e8107e303265241e6b7b556db5ca48
funList :: (forall f. Applicative f => (a -> f b) -> s -> f t)
        -> s -> FunList a b t
funList trav s = trav singleton s

unfunList :: forall f s t a b. Applicative f => (s -> FunList a b t)
          -> (a -> f b) -> s -> f t
unfunList f afb s = go (f s) where
    go :: FunList a b r -> f r
    go (Done t)    = pure t
    go (More x xs) = liftA2 (&) (afb x) (go xs)
```

where

```haskell
empty :: t -> FunList a b t
empty = Done

append :: (t -> s -> r) -> FunList a b t -> FunList a b s -> FunList a b r
append h (Done t)    ys = fmap (\s -> h t s) ys
append h (More x xs) ys = More x $ append (\bt s b -> h (bt b) s) xs ys

singleton :: a -> FunList a b b
singleton x = More x (Done id)

instance Applicative (FunList a b) where
    pure = empty
    liftA2 = append
```

so if your underlying implementation would be easier using a concrete type
(instead of using traversal directly) 
then a `FunList` is one candidate:

```haskell
(&%>) :: FunList FilePattern FilePath res -> (res -> Action ()) -> Rules ()
```

that would be terrible to use, but it might be about as easy to implement
as list variant.

---

Alternatively, you can "cheat" like `lens` does in `partsOf` implementation,
by using a state monad:

Given an like operation `fooList :: Monad m => [k] -> m [v]`, we can write
a generalized version

```haskell
fooTrav :: (Monad m, Traversable t) => t k -> m (t v)
fooTrav ks = do
    -- convert to list and use fooList
    vs <- fooList (toList ks)

    -- traverse ks again, replacing each k with a v from the state
    evalStateT (traverse (\_k -> state un) vs) ks
  where
    un []     = error "invalid traversal"
    un (x:xs) = (x, xs)
```

Implementation using `Each` would look somewhat similar.

---

Finally, not only `Traversable`-powered interface allows to use
complete pattern matches as in `shake` like use-cases, it also allows
using more elaborate data-structures for batch operations.

For example, if you have a `Map Client [Key]` and you want to lookup
every value getting `Map Client [Value]` back.

With `Traversable` interface it's as easy as using `Compose`, turning `Map
Client [Key]` into `Compose (Map Client) [] Key` which fits the `Traversable`
interface perfectly, so you avoid bundling-and-distributing code in the use
sites: `Map` in, `Map` out.

The answer is always `traverse`.
