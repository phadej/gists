---
title: Why there is no AGetter?
author: Oleg Grenrus
tags: lens
---

This question was recently asked on [`lens` issue tracker](https://github.com/ekmett/lens/issues/689). [The short answer](https://github.com/ekmett/lens/issues/689#issuecomment-262466054), by Edward Kmett is

<blockquote>Mainly because we have functions, which perfectly characterize getters.</blockquote>

Let's explore that in more detail:

This post is a literate Haskell file, and as we work with `lens`, we need to import them.
```haskell
{-# LANGUAGE RankNTypes, TypeOperators #-}
module NoAGetter where
import Control.Lens
import Data.Type.Equality  -- for a small proof
```

Existential answer
-------------

<blockquote>Keep in mind AGetter, if it existed, wouldn't work with views or any of the more general combinators other than view. So it'd be a thing that only existed so we could turn it into a function.</blockquote>

Suppose, there is `AGetter`:

```haskell
-- | When you see this as an argument to a function, it expects a Getter.
type AGetter s a = LensLike' (Retext a) s a
```

then we could put `AGetter`s into a container.
Similarly as we have
a version of [`(^.)`](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Getter.html#v:-94-.), the
[`(^#) :: s -> ALens s t a b -> a`](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Lens.html#v:-94--35-)
that works on ALens, we'll new combinator, say `(^-) :: s -> AGetter s a -> b`.

To put it differently:
The `ALens` is isomorphic to `Lens`, but we cannot use it directly as `Lens`:

```
Control.Lens> :t view (_1 :: ALens' (Int, Bool) Int)

<interactive>:... error:
    • Couldn't match type ‘Control.Lens.Internal.Context.Pretext
                             (->) Int Int (Int, Bool)’
                     with ‘Const Int (Int, Bool)’
      Expected type: Getting Int (Int, Bool) Int
        Actual type: ALens' (Int, Bool) Int
    • In the first argument of ‘view’, namely
        ‘(_1 :: ALens' (Int, Bool) Int)’
      In the expression: view (_1 :: ALens' (Int, Bool) Int)
```

we need to clone the `ALens`, or use specialized combinators:

```
Control.Lens> :t view (cloneLens (_1 :: ALens' (Int, Bool) Int))
view (cloneLens (_1 :: ALens' (Int, Bool) Int))
  :: Control.Monad.Reader.Class.MonadReader (Int, Bool) m => m Int
```

Constructive answer
-------------------

The question is, what's `Retext`? If you explore the `lens` documentation,
you'll notice that `ALens` uses `Pretext`, which is better `Context`, which
is isomorphic to `exists s. (s, Lens s t a b)`. By analogy, we need something
isomorphic to `exists s. (s, Getter s a)`. `Getter s a` is isomorphic to `s -> a`,
so
```
exists s. (s, Getter s a) ~ exists s. (s, s -> a) ~ a
```
As `Retext` needs to be a `Functor`, we'll just add a phantom type-variable.
So at the end, the `Retext` is an old friend of us:

```haskell
type Retext = Const
```

The `Context` has more type arguments, and we cannot define
a [`Sellable`](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Internal-Context.html#t:Sellable)
instance, but it would be `sell = Const`. Using this observation we can
define `cloneGetter` and `(^-)`, by adopting definitions of the
[`cloneLens`](http://hackage.haskell.org/package/lens-4.15.1/docs/src/Control.Lens.Lens.html#line-523)
and the
[`(^#)`](http://hackage.haskell.org/package/lens-3.15.1/docs/src/Control.Lens.Lens.html#line-1345):

```haskell
cloneGetter :: AGetter s a -> Getter s a
cloneGetter g = to $ \s -> getConst (g Const s)

infixl 8 ^-
(^-) :: s -> AGetter s a -> a
s ^- g = getConst (g Const s)
```

And we can use these definitions:

```
*Main> ('a', 'b') ^- _1
'a'

*Main> let getters = [_1, _2] :: [AGetter (Int, Int) Int]
*Main> map (\g -> (42, 7) ^- g) getters
[42,7]

*Main> map (\g -> (42, 7) ^. cloneGetter g) getters
[42,7]
```

Alternatives
------------

If we inline the type-aliases of `AGetter`, we'll get a `Getting a s a`:
```
AGetter s a
    = LensLike' (Const a) s a
    = LensLike (Const a) s s a a
    = (a -> Const a a) -> s -> Const a s
    = Getting a s a
```
If you really want to put `Getter`s into containers,
simplest solution seems to use `Getting a s a`: It's already in `Control.Lens`.

```haskell
type AGetter2 s a = Getting a s a

cloneGetter2 :: AGetter2 s a -> Getter s a
cloneGetter2 g = to $ \s -> s ^. g
```

One should note, tat the `AGetter` and the `AGetter2` are exactly the same:
```haskell
proof :: AGetter s a :~: AGetter2 s a
proof = Refl
```

The corollary of above, is that need `(^-)`, `(^.)` works well:
```
*Main> let getters2 = [_1, _2] :: [AGetter2 (Int, Int) Int]
*Main> map (\g -> (42, 7) ^. g) getters2
[42,7]
```
because of the type of `(^.) :: s -> Getting a s a -> a`.

Yet, we still need to clone the `AGetter` when the viewed type is changed (similarly as with `ALens`):
```
*Main> (42, True) ^. _1 . re _Show
"42"

*Main> (42, True) ^# (_1 :: ALens' (Int, Bool) Int) . re _Show
<interactive>:... error: ...
*Main> (42, True) ^. (_1 :: AGetter (Int, Bool) Int) . re _Show
<interactive>:... error:

*Main> (42, True) ^. cloneLens (_1 :: ALens' (Int, Bool) Int) . re _Show
"42"
*Main> (42, True) ^. cloneGetter (_1 :: AGetter (Int, Bool) Int) . re _Show
"42"
```

Plain old function
------------------

On the other hand, just using getter *functions* is even simpler, though then
we "escape" the `lens` eco-system:

```haskell
type AGetter3 s a = s -> a

cloneGetter3 :: AGetter3 s a -> Getter s a
cloneGetter3 = to
```

To put `AGetter3`s into container, we need explicitly `view` them:
```
*Main> let getters3 = [view _1, view _2] :: [AGetter3 (Int, Int) Int]
*Main> map (\g -> (42, 7) ^. cloneGetter3 g) getters3
[42,7]
```

Conclusion
----------

So, *why there is no AGetter?* In this post I tried to clarify, that having one
doesn't add any(?) benefit over plain *getter functions*. Yet, you are free to
play with the definitions here. Please tell me (e.g. [via
twitter](https://twitter.com/intent/tweet?text=AGetter%20is%20nice%20when%20...&via=phadej)), if you find some
applications where `AGetter` is more elegant then a plain old function.

---

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit' agetter.lhs
```
fetch the source from
[https://gist.github.com/phadej/fb02aea7de32fba94bc3de33de6ee2e6](https://gist.github.com/phadej/fb02aea7de32fba94bc3de33de6ee2e6)
