---
title: Mutually recursive traversals
author: Oleg Grenrus
tags: lens
---

Michael PJ recently wrote a post about
[Lenses for Tree Traversals](https://www.michaelpj.com/blog/2020/08/02/lenses-for-tree-traversals.html).
In a [r/haskell discussion](https://www.reddit.com/r/haskell/comments/i2js6q/lenses_for_tree_traversals/)
there is [a comment](https://www.reddit.com/r/haskell/comments/i2js6q/lenses_for_tree_traversals/g0748mh/) which got my attention.

<blockquote>
And here is the problem. With mutually recursive datatypes even with generics we can't write generic type-safe traversal. We have to do with boilerplate.
</blockquote>

*Challenge accepted.*

---

As pointed out on Twitter,
the approach below is similar to what
[`multiplate`](https://hackage.haskell.org/package/multiplate)
library by Russell O'Connor gives combinators for.
It's usable with `lens` too.

---

```haskell
{-# LANGUAGE RankNTypes #-}

-- For GPlated
{-# LANGUAGE DeriveGeneric, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}
module MutualTraversals where

import Control.Lens (transformOf)
import GHC.Generics

```

---

I will use the same example as Michael, simply implemented,
simply typed lambda calculus.

```haskell
type Name = String

data Type = IntegerType | FunType Type Type deriving (Eq, Show)
```

but instead of direct variant

```
data Term = 
    Var Name
    | Lam Name Type Term
    | App Term Term
    | Plus Term Term
    | Constant Integer
```

let us have bidirectional version.
(I just wrote a post about [Bidirectional Pure Type Systems](https://oleg.fi/gists/posts/2020-08-03-bidi-pts.html), so an obvious choice).

Bidirectionality forces us to think about `Plus`.
A solution is to add an additional constructor.

```haskell
data Syn
    = Var Name
    | App Syn Chk
    | Ann Chk Type
  deriving (Show, Generic)

data Chk
    = Lam Name Chk            -- note, no type annotation
    | Constant Integer
    | UnaryPlus Integer Syn   -- additional constructor
    | Plus Syn Syn
    | Conv Syn
  deriving (Show, Generic)
```

Note, how `UnaryPlus` and `Plus` are "stuck" on `Syn` terms.

The goal is fold constants. Interesting stuff happens in checkable
terms, `Chk`.

```haskell
cfChk :: Chk -> Chk
cfChk t = case t of
    UnaryPlus n (Ann (UnaryPlus m p) _) -> UnaryPlus (n + m) p
    UnaryPlus n (Ann (Constant m) _)    -> Constant (n + m)
    
    Plus (Ann (UnaryPlus n m) ty) p     -> UnaryPlus n (Ann (Plus m p) ty)
    Plus n (Ann (UnaryPlus m p) ty)     -> UnaryPlus m (Ann (Plus n p) ty)
    Plus (Ann (Constant n) _) m         -> UnaryPlus n m
    Plus n (Ann (Constant m) _)         -> UnaryPlus m n

    _ -> t
```

We have more rules because we added `UnaryPlus`, but
we can fold more constants, exploiting the commutativity and associativity of addition.

But how to write `constantFold`, we have two mutually recursive types.
The answer is obvious after you hear it.
If you have two mutually recursive types,
then there are two mutually recursive traversals.

They look like monomorphic [`Bitraversable`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Bitraversable.html#t:Bitraversable).

Let me define helper type-aliases:

```haskell
type Star f a b = a -> f b

type LensLike'   f s a   = Star f a a               -> Star f s s
type BilensLike' f s a b = Star f a a -> Star f b b -> Star f s s

type Traversal' s a = forall f. Applicative f => LensLike' f s a
```

Then we can define bitraversals:

```haskell
chkSubterms' :: Applicative f => BilensLike' f Chk Syn Chk
chkSubterms' _syn  chk (Lam n x)       = Lam n <$> chk x
chkSubterms' _syn _chk t@Constant{}    = pure t
chkSubterms'  syn _chk (UnaryPlus n x) = UnaryPlus n <$> syn x
chkSubterms'  syn _chk (Plus x y)      = Plus <$> syn x <*> syn y
chkSubterms'  syn _chk (Conv x)        = Conv <$> syn x

synSubterms' :: Applicative f => BilensLike' f Syn Syn Chk
synSubterms'  syn  chk (App f x) = App <$> syn f <*> chk x
synSubterms' _syn  chk (Ann x t) = Ann <$> chk x <*> pure t
synSubterms' _syn _chk t@Var {}  = pure t
```

And using these we can define

```haskell
chkSubterms  :: Traversal' Chk Chk
chkSubterms f = chkSubterms' aux f where aux = synSubterms' aux f
```

The above definition is slightly complicated.
We have to make recursive `aux` to drill through `Syn` terms until
it finds `Chk` terms.

But after all the setup, we can define `constantFold`.

```haskell
constantFold :: Chk -> Chk
constantFold = transformOf chkSubterms cfChk
```

Let us also try it out.
We are going to write a redundant program,
there are plenty of type annotations highlighting that.

```haskell
expr1 :: Chk
expr1
    = Plus (annZ (Constant 2))
    $ annZ $ Plus (Var "x")
    $ annZ (Constant 3)
  where
    annZ n = Ann n IntegerType
```

After two iterations of `constantFold` we get completely
simplified result:

```
*MutualTraversals> constantFold expr1
UnaryPlus 3 (Ann (Plus (Ann (Constant 2) IntegerType) (Var "x")) IntegerType)

*MutualTraversals> constantFold $ constantFold expr1
UnaryPlus 5 (Var "x")
```

*It works*.

---

To complete the challenge, we need to write
`chkSubterms'` and `synSubterms'` generically.
If we are allowed to use Template Haskell, that would be as straight forward
as writing Template Haskell is.
Nor I see any immediate problems generalizing `GPlated` definitions to generate bitraversals.

EDIT: Later today I added `GPlated2` in appendix.
It is straight forward generalization of `GPlate` implementation in `lens`.

```haskell
chkSubterms2' :: Applicative f => BilensLike' f Chk Syn Chk
synSubterms2' :: Applicative f => BilensLike' f Syn Syn Chk

chkSubterms2' f g = gplate2 g f
synSubterms2' f g = gplate2 f g

chkSubterms2  :: Traversal' Chk Chk
chkSubterms2 f = chkSubterms2' aux f where aux = synSubterms2' aux f

constantFold2 :: Chk -> Chk
constantFold2 = transformOf chkSubterms2 cfChk
```

*It works.*

```
*MutualTraversals> constantFold2 expr1
UnaryPlus 3 (Ann (Plus (Ann (Constant 2) IntegerType) (Var "x")) IntegerType)
*MutualTraversals> constantFold2 $ constantFold2 expr1
UnaryPlus 5 (Var "x")
```

So you don't even need to define boilerplate by hand.

---

We can define a `Plate` type like `multiplate` library advises.

```haskell
data Plate f = Plate
    { chkPlate :: Star f Chk Chk
    , synPlate :: Star f Syn Syn
    }
```

and define a value

```haskell
synChkPlate :: Applicative f => Plate f -> Plate f
synChkPlate p = Plate
    { chkPlate = chkSubterms' (synPlate p) (chkPlate p)
    , synPlate = synSubterms' (synPlate p) (chkPlate p)
    }
```

Now it's easy to see how you would add `Type` traversals to the mix.

--- 

I could also refute Michael's comment

<blockquote>
recursion-schemes does badly with mutually recursive types.
If this is a problem for you, you’ll realize pretty quickly.
</blockquote>

The `recursion-schemes` itself cannot deal with mutually recursive
types, but the approach can be generalized.
In this post we used stuff beyond `lens` as well.

I'll leave that for a future post.
(Or you can look into https://hackage.haskell.org/package/multirec and
 paper which explains it).

---

Note, that Michael slightly *cheats* in counting nodes with `Folds`:

```
    -- ... plus the number of nodes in all the subterms 
    <> foldMapOf termSubterms countTermNodes t
    -- ... plus the number of nodes in all the subtypes
    <> foldMapOf termSubtypes countTypeNodes t
```

here he examines the same `t` twice. First looking for subterms,
and then for subtypes.

With his definition of (unidirectional) `Term`,
he could use bitraversal to look for types and terms simultaneously!

---

Appendix: GPlated2

```haskell
-- | Implement 'plate' operation for a type using its 'Generic' instance.
gplate2
    :: (Generic a, GPlated2 a b (Rep a), Applicative f)
    => BilensLike' f a a b
gplate2 f g x = GHC.Generics.to <$> gplate2' f g (GHC.Generics.from x)

class GPlated2 a b g where
  gplate2' :: Applicative f => BilensLike' f (g p) a b

instance GPlated2 a b f => GPlated2 a b (M1 i c f) where
  gplate2' f g (M1 x) = M1 <$> gplate2' f g x

instance (GPlated2 a b f, GPlated2 a b g) => GPlated2 a b (f :+: g) where
  gplate2' f g (L1 x) = L1 <$> gplate2' f g x
  gplate2' f g (R1 x) = R1 <$> gplate2' f g x

instance (GPlated2 a b f, GPlated2 a b g) => GPlated2 a b (f :*: g) where
  gplate2' f g (x :*: y) = (:*:) <$> gplate2' f g x <*> gplate2' f g y

instance {-# OVERLAPPING #-} GPlated2 a b (K1 i a) where
  gplate2' f _ (K1 x) = K1 <$> f x

instance {-# OVERLAPPING #-} GPlated2 a b (K1 i b) where
  gplate2' _ g (K1 x) = K1 <$> g x

instance GPlated2 a b (K1 i c) where
  gplate2' _ _ = pure

instance GPlated2 a b U1 where
  gplate2' _ _ = pure

instance GPlated2 a b V1 where
  gplate2' _ _ v = v `seq` error "GPlated2/V1"
```
