---
title: Fancy types for cassava
author: Oleg Grenrus
tags: fancy-types
---

This blog post is an expanded answer to
[Marco Zocca's tweet](https://twitter.com/ocramz_yo/status/1146692809187090432).
Thanks Marco for all Data Haskell and other work you do.

Can we make `cassava` stuff a bit safer using *fancy types*?
This is not a proposal to change `cassava`, only some ideas and a demonstration
of "real world" fancy types (though [STLC-implementation](https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.pdf) also count as real in my world ;).
Also a bit of Generics.

<div id="toc"></div>

Prologue
--------

This post is not only Literate Haskell file, but also
can be simply run with (given a very recent version of `cabal-install`),

```bash
cabal-dev run --index-state=2019-06-30T22:28:46Z posts/2019-07-07-fancy-types-for-cassava.lhs
```

as we specify dependencies. We'll use
[`fin`](https://hackage.haskell.org/package/fin) and
[`vec`](https://hackage.haskell.org/package/vec) packages.
I assume that `data Nat = Z | S Nat` and `data Vec :: Nat -> Type -> Type`
are familiar to you.[^finvec]

[^finvec]: If they aren't, read through e.g.
[Stitch](https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.pdf) paper
and / or watch a video of a talk Richard Eisenberg presented at ZuriHac '19 (which i saw live, hopefully it will be posted somewhere soon).
or [older version from NYC Haskell Group meetup](https://www.youtube.com/watch?v=XJ8hm3Tq2k8) (which I didn't watch, only googled for).

```haskell
{- cabal:
build-depends:
  , base   ^>=4.10 || ^>=4.11
  , fin    ^>=0.0.3
  , vec    ^>=0.1.1
  , tagged ^>=0.8.6
  , text   ^>=1.2.3.0

ghc-options:        -Wall -pgmL markdown-unlit
build-tool-depends: markdown-unlit:markdown-unlit ^>=0.5.0
-}
```

Next a `{-# LANGUAGE Dependent #-}` collection of extensions...

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures -Wno-unused-imports #-}
```

... and imports

```haskell
module Main where

import Control.Monad (forM)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import Data.Type.Nat (Nat (..))
import Data.Vec.Lazy (Vec (..))
import GHC.Generics
import Text.Read (readMaybe)

import qualified Data.Fin as F
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V
```

Encoding
--------

*Encoding* is often easier than decoding, so let's start with it. Our running
example will be programming languages:

```haskell
data PL = PL
    { plName   :: Text
    , plYear   :: Int
    , plPerson :: Text
    }
  deriving (Eq, Ord, Show, Generic)
```

We can create a small database of programming languages we have
heard of:

```haskell
pls :: [PL]
pls =
    [ PL "Haskell" 1990 "Simon"
    , PL "Scala"   2004 "Martin"
    , PL "Idris"   2009 "Edwin"
    , PL "Perl"    1987 "Larry"
    ]
```

<h3>ToField</h3>

For encoding, we'll need to be able to encode individual fields / cells.
This is similar to what we have in `cassava` now.
I'm *for* using type-classses for such cases, because I want to be able
to leverage *Generics*, we'll see that soon.

Thanks to the fancier types, it would be possible to avoid type-classes,
still getting something for free, but that's a topic for another post.

```haskell
class    ToField a    where toField :: a -> Text
instance ToField Text where toField = id
instance ToField Int  where toField = T.pack . show
```

<h3>ToRecord</h3>

As we can serialise individual fields, let us serialise records.
We could have a method `toRecord :: r -> [Text]` as in `cassava` now,
but it is potentially unsafe.
Length of the list may vary depending on the record value.
So we'd rather use fancy types!

```haskell
type family Size (a :: Type) :: Nat

class ToRecord r where
    toRecord :: r -> Vec (Size r) Text
```

It's easy to imagine the `ToRecord PL` instance. But we rather write
a generic implementation for it.

First a generic `Size`. Recall that `GHC.Generics` represents
records as binary tree. To avoid adding `Nat`s and concatenating `Vec`s, we'll
`foldr` like implementation. There is a nested application of `GSizeF` type
family in the `:*:`-case. GHC wants `UndecidableInstances`.

```haskell
type GSize a = GSizeF (Rep a) 'Z -- start from zero

type family GSizeF (f :: Type -> Type) (acc :: Nat) :: Nat where
    GSizeF U1         acc = acc
    GSizeF (K1 i a)   acc = 'S acc
    GSizeF (M1 i c f) acc = GSizeF f acc
    GSizeF (f :*: g)  acc = GSizeF f (GSizeF g acc)
```

Using that type family, we can say succintly define:

```haskell
type instance Size PL = GSize PL
```

Also we can check this dependent-language style.
If this test fails, it will be a **compilation error**.
In a dependently typed language we run tests in compile-time.

```haskell
check1 :: Size PL :~: N.Nat3
check1 = Refl
```

Using similar induction on the structure, we can write a generic
implementation for `ToRecord`. Different clauses are handled
by different instances of a workhorse class `GToRecord`.

```haskell
genericToRecord
    :: forall r. (Generic r, GToRecord (Rep r))
    => r -> Vec (GSize r) Text
genericToRecord = gtoRecord VNil . from

class GToRecord rep where
    gtoRecord :: Vec acc Text -> rep () -> Vec (GSizeF rep acc) Text
instance GToRecord U1 where
    gtoRecord xs _ = xs
instance ToField c => GToRecord (K1 i c) where
    gtoRecord xs (K1 c) = toField c ::: xs
instance GToRecord f => GToRecord (M1 i c f) where
    gtoRecord xs (M1 f) = gtoRecord xs f
instance (GToRecord f, GToRecord g) => GToRecord (f :*: g) where
    gtoRecord xs (f :*: g) = gtoRecord (gtoRecord xs g) f
```

The `ToRecord PL` instance in the user code is a one-liner:

```haskell
instance ToRecord PL where toRecord = genericToRecord
```

<h3>Column names</h3>

One more thing. Column names. Often CSV files start with a header line.
This is where using `Size` pays off again:
Header have to be the same size as the content rows.
Here I use `Tagged` to avoid `AllowAmbiguousTypes` and `Proxy r` extra argument.

```haskell
class Header r where
    header :: Tagged r (Vec (Size r) Text)
```

We could write generic implementation for it, but as dealing
with metadata in `GHC.Generics` is not pretty, I'll implement `PL` instance
manually:

```haskell
instance Header PL where
    header = Tagged $ "name" ::: "year" ::: "person" ::: VNil
```

<h3>Encoding function</h3>

The one piece left is an actual `encode` function.
I cut corners by not dealing with escaping, etc for the sake of brevity.
It's important to notice, that in implementation of encoding,
we don't care that much about the fact we get same `Vec n Text`
from each record, as we serialise to "untyped" Text anyway.
Fancy types are here to help users of a library write correct & coherent (by construction)
instances!

```haskell
encode :: forall r. (Header r, ToRecord r) => [r] -> Text
encode rs = T.unlines $ map (T.intercalate "," . V.toList)
    $ unTagged (header :: Tagged r _)
    : map toRecord rs
```

And it seems to work:

```
*Main> T.putStr $ encode pls
name,year,person
Haskell,1990,Simon
Scala,2004,Martin
Idris,2009,Edwin
Perl,1987,Larry
```

Good. Let's try to reverse this.


Decoding
--------

Other direction, *decoding* is trickier. Everything could fail. Fields can contain
garbage, there might be not enough fields (too much is not such a problem),
but most importantly the fields can come in wrong order. Luckily we have
fancy types helping us.

<h3>FromField</h3>

Like `ToField`, `FromField` is a copy of `cassava` class:

```haskell
type Error = String

class    FromField a    where fromField :: Text -> Either String a
instance FromField Text where fromField = Right
instance FromField Int  where
    fromField t
        = maybe (Left $ "Invalid Int: " ++ show t) Right
        $ readMaybe $ T.unpack t
```

<h3>FromRecord</h3>

Also like `ToRecord`, `FromRecord` is simple class as well.
Note how library users need to deal only with
vector of the right size (versus list of any length).
We'll also assume that vector is sorted,
to match header columns
(which could be encoded with even fancier types!)

```haskell
class FromRecord r where
    fromRecord :: Vec (Size r) Text -> Either Error r
```

Generic implementation "peals off" provided vector:

```haskell
genericFromRecord
    :: forall r. (Generic r, GFromRecord (Rep r))
    => Vec (GSize r) Text -> Either String r
genericFromRecord ts =
    let tmp :: Either Error (Rep r (), Vec 'Z Text)
        tmp = gfromRecord ts
    in to . fst <$> tmp

class GFromRecord rep where
    gfromRecord :: Vec (GSizeF rep acc) Text -> Either Error (rep (), Vec acc Text)
instance GFromRecord U1 where
    gfromRecord xs = return (U1, xs)
instance FromField c => GFromRecord (K1 i c) where
    gfromRecord (x ::: xs) = do
        y <- fromField x
        return (K1 y, xs)
instance GFromRecord f => GFromRecord (M1 i c f) where
    gfromRecord = fmap (first M1) . gfromRecord
instance (GFromRecord f, GFromRecord g) => GFromRecord (f :*: g) where
    gfromRecord xs = do
        (f, xs')  <- gfromRecord xs
        (g, xs'') <- gfromRecord xs'
        return (f :*: g, xs'')

instance FromRecord PL where
    fromRecord = genericFromRecord
```

And a small sanity check:

```
*Main> fromRecord ("Python" ::: "1990" ::: "Guido" ::: VNil) :: Either String PL
Right (PL {plName = "Python", plYear = 1990, plPerson = "Guido"})

*Main> fromRecord ("Lambda Calculus" ::: "in the 1930s" ::: "Alonzo" ::: VNil) :: Either String PL
Left "Invalid Int: \"in the 1930s\""
```

The difficult part
------------------

We are now solved all the easy problems. We have set up the public
api of library. `To*` and `From*` classes use fancy types, we have
taken some burden from library users. However the difficult task
is still undone: implementing `decode`.

The example we'll want to work will have extra fields,
and fields unorder:

```haskell
input :: Text
input = T.unlines
    [ "year,name,types,person,website"
    , "1987,Perl,no,Larry,https://www.perl.org/"
    , "1990,Haskell,nice,Simon,https://www.haskell.org/"
    , "2004,Scala,weird,Martin,https://www.scala-lang.org/"
    , "2009,Idris,fancy,Edwin,https://www.idris-lang.org/"
    ]
```

which is

```
*Main> T.putStr input
year,name,types,person,website
1987,Perl,no,Larry,https://www.perl.org/
1990,Haskell,nice,Simon,https://www.haskell.org/
2004,Scala,weird,Martin,https://www.scala-lang.org/
2009,Idris,fancy,Edwin,https://www.idris-lang.org/
```

There's enough information there, so we should be able to successfully
extract `PL` from there.

Zeroth step is to split input into lines, and lines into cells,
and extract the header row. That's not the hard:

```haskell
prepare :: Text -> Either Error ([Text], [[Text]])
prepare i = case map (T.splitOn ",") (T.lines i) of
    []     -> Left "No header"
    (r:rs) -> Right (r, rs)
```

<h3>Sorting columns</h3>

The hard part is to decode from `[Text]` into `Vec (Size r) Text`.
And not only we need to decode, but also sort the columns.
Our plan would be to

1. sort the header row, *returning the trace of a sort*
2. use that trace to sort content rows similarly.

We'll require that content rows contain at least as many columns
as header row. It's reasonable requirement, and simplifies things a bit.
More relaxed requirement would be to require only as much rows as needed,
e.g. in our example we could require only 4 fields, as we aren't interested
in the `website` field.

What's the *trace* of a sort? Technically it's *permutation*.
However in this case, it's not regular permutation, as we aren't interested
in all fields. It's easier to think backwards, and think which kind
of trace would determine the execution in the step 2.
We'll be given a `Vec n Text` for some `n`, and we'll need to produce
a `Vec m Text` for some other `m` (= `Size r`). Let's try to write
that as a data type:

```haskell
data Extract :: Nat -> Nat -> Type where
    Step :: F.Fin ('S n)           -- take a nth value
         -> Extract n m            -- recursively extract rest
         -> Extract ('S n) ('S m)  
    Done :: Extract n 'Z           -- or we are done.

deriving instance Show (Extract n m)
```

We can immediately try this type in action.  For what it's worth, the
implementations of following functions is quite restricted by their types.
There are not much places where you can make a mistake.  To be fair, writing
`extract` and `Extract` was an iterative process: write a type, tries how it
work, tweak the type, tweak the functions ... Very often in dependently type
programming, the first idea for a fancy type doesn't always work.

```haskell
extract :: Extract n m -> Vec n a -> Vec m a
extract Done       _  = VNil
extract (Step n e) xs = case delete n xs of
    (x, xs') -> x ::: extract e xs'

-- this probably should be in `vec` library
delete :: F.Fin ('S n) -> Vec ('S n) a -> (a, Vec n a)
delete F.Z            (x ::: xs)       = (x, xs)
delete (F.S F.Z)      (x ::: y ::: xs) = (y, x ::: xs)
delete (F.S n@F.S {}) (x ::: xs)       = case delete n  xs of
    (y, ys) -> (y, x ::: ys)
```

For example, given a row and a trace, we can extract fields we want
(writing correct trace by hand *is* tricky).

```
*Main> row = "1987" ::: "Perl" ::: "no" ::: "Larry" ::: "https://www.perl.org/" ::: VNil
*Main> trc = Step 1 $ Step F.fin0 $ Step F.fin1 Done :: Extract N.Nat5 N.Nat3
*Main> extract trc row
"Perl" ::: "1987" ::: "Larry" ::: VNil
```

That starts to feel like magic, doesn't it?
To complete the whole spell, we need to complete part 1, i.e. construct
`Extract` traces.
Luckily, types are there to guide us:

```haskell
columns
    :: (Eq a, Show a)
    => Vec m a         -- ^ wanted header values
    -> Vec n a         -- ^ given header values
    -> Either Error (Extract n m)
columns VNil       _            = Right Done
columns (_ ::: _)  VNil         = Left "not enought header values"
columns (h ::: hs) xs@(_ ::: _) = do
    (n, xs') <- find' h xs  -- find first value
    rest <- columns hs xs'  -- recurse
    return $ Step n rest    -- record the trace
```

where we use a helper function `find'`, which finds a value in the `Vec ('S n)`
and returns not only an index, but also a leftover vector.
We could write a test:

- if `Right (n, ys) = find'` x xs
- then `ys = delete n xs`

```haskell
find'
    :: (Eq a, Show a)
    => a
    -> Vec ('S n) a
    -> Either Error (F.Fin ('S n), Vec n a)
find' x (y ::: ys)
    | x == y    = Right (F.Z, ys)
    | otherwise = case ys of
        VNil    -> Left $ "Cannot find header value " ++ show x
        _ ::: _ -> do
            (n, zs) <- find' x ys
            return (F.S n, y ::: zs)
```

Let's try `columns`. It takes some time to understand to interpret
`Extract` values. Luckily the machine is there to do that.

```
*Main> columns ("name" ::: "year" ::: VNil) ("name" ::: "year" ::: VNil)
Right (Step 0 (Step 0 Done))

*Main> columns ("name" ::: "year" ::: VNil) ("year" ::: "name" ::: VNil)
Right (Step 1 (Step 0 Done))

*Main> columns ("name" ::: "year" ::: VNil) ("year" ::: "extra" ::: "name" ::: VNil)
Right (Step 2 (Step 0 Done))

*Main> columns ("name" ::: "year" ::: VNil) ("year" ::: "extra" ::: "foo" ::: VNil)
Left "Cannot find header value \"name\""

*Main> columns ("name" ::: "year" ::: VNil) ("name" ::: VNil)
Left "not enought header values"
```

<h3>Assembling all parts together</h3>

We have three steps

1. `prepare` to split input data into header and content rows
2. `columns` which checks whether there are all fields we want in the provided
   header, and returns an `Extract` value saying how to permute content rows.
3. `extract` which uses an `Extract` to extract (and order) correct
   data columns.

We'll use few two functions from [`vec`](https://hackage.haskell.org/package/vec):
[`reifyList`](https://hackage.haskell.org/package/vec-0.1.1/docs/Data-Vec-Lazy.html#v:reifyList)
and [`fromListPrefix`](https://hackage.haskell.org/package/vec-0.1.1/docs/Data-Vec-Lazy.html#v:fromListPrefix).

```
-- Reify any list [a] to Vec n a.
reifyList :: [a] -> (forall n. SNat n => Vec n a -> r) -> r

-- Convert list [a] to Vec n a. Returns Nothing if input list is too short.
fromListPrefix :: SNatI n => [a] -> Maybe (Vec n a)
```

They both convert a list `[a]` into `Vec n a`, however they are different

- `reifyList` works for *any* list. As we don't know the length
  of dynamic inputs, `reifyList` takes a continuation which accepts
  `Vec` of *any* length. That continuation however would know
  and be able to use the vector length.
- `fromListPrefix` tries to convert a list to a vector of *known length*,
  and thus may fail.

To put it differently, using `reifyList` we learn the length of the header,
and then we require that subsequent content rows are of atleast the same length.
Lifting (or promoting) some information to the type level, reduces
the amount of dynamic checks we'll need to consequtively
e.g. `extract` doesn't do any checks anymore.

```haskell
decode :: forall r. (Header r, FromRecord r) => Text -> Either String [r]
decode contents = do
    (hs,xss) <- prepare contents
    V.reifyList hs $ \hs' -> do
        trc <- columns (unTagged (header :: Tagged r _)) hs'
        forM xss $ \xs -> do
            xs' <- maybe (Left "not enough columns") Right
                $  V.fromListPrefix xs
            fromRecord (extract trc xs')
```

All done! To convince you that it works, let's run `decode` on
an `input` we defined at the beginning of this section.

```haskell
main :: IO ()
main = case decode input :: Either String [PL] of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right xs -> mapM_ print xs
```

```
*Main> main
PL {plName = "Perl", plYear = 1987, plPerson = "Larry"}
PL {plName = "Haskell", plYear = 1990, plPerson = "Simon"}
PL {plName = "Scala", plYear = 2004, plPerson = "Martin"}
PL {plName = "Idris", plYear = 2009, plPerson = "Edwin"}
```

Even fancier types
------------------

There is still a lot places where we can make mistakes.
We use `Vec n a`, so we have `n` elements to pick.
If we instead use heterogenous lists, e.g.
[`NP` from `sop-core`](https://hackage.haskell.org/package/sop-core-0.5.0.0/docs/Data-SOP-NP.html#t:NP)
The types would become more precise.
We could change our public interface to:

```{.haskell.ignore}
type family Fields r :: [Type]
class ToRecord' r where
    toRecord' :: r -> NP I (Fields r)
class Header' r where
    header' :: Tagged r (NP (K Text) (Fields r))
```

then writing correct versions of `delete`, `extract` etc will
be even more type-directed.
That's is left as an exericise,
I suspect that the code shape will be quite the same.

<h3>Row types</h3>

One valid question to ask, is whether row-types would simplify something
here. Not really.

For example [`vinyl`](https://hackage.haskell.org/package/vinyl)'s
[`Rec`](https://hackage.haskell.org/package/vinyl-0.11.0/docs/Data-Vinyl-Core.html#t:Rec)
type is essentially the same as `NP`.
Even if there were anonymous records in Haskell,
so `toRecord` could be implemented directly using a built-in function,
it would remove only a single problem from many.
At it's not much, as `toRecord` is generically derivable.

Conclusion
----------

In this post I described a complete fancy types usage example,
helping us to deal with the untyped real world.
Fancy types make library API more precise: we encode preconditions
like "lists have to be of equal length" in the types.
This prevents users from making silly errors,
and also make the implementation more robust.
