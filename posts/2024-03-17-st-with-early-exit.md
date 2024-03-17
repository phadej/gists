---
title: ST with an early exit
author: Oleg Grenrus
---

Implementation
--------------

I wish there were an early exit functionality in the `ST` monad.
This need comes time to time when writing imperative algorithms in Haskell.

It's very likely there is a functional version of an algorithm,
but it might be that `ST`-version is just simply faster, e.g. by avoiding
allocations (as allocating even short lived garbage is not free).

But there are no early exit in the `ST` monad.

Recent GHC added [*delimited continuations*](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim.html#g:24).
The TL;DR is that delimited continuations is somewhat like `goto`:

- `newPromptTag#` creates a label (tag)
- `prompt#` brackets the computation
- `control#` kind of jumps (goes to) the end of enclosing prompt bracket,
  and continues from there.

So let's use this functionality to implement a version of `ST` which
has an early exit. It turns out to be quite simple.

The `ST` monad is define like:

```haskell
newtype ST s a = ST (State# s -> (# State# s, a #)
```

and we change it by adding an additional prompt tag argument:

```haskell
newtype EST e s a = EST
    { unEST :: forall r. PromptTag# (Either e r)
            -> State# s -> (# State# s, a #) 
    }
```

(Why `forall r.`? We'll see soon).

It's easy to lift normal `ST` computations into `EST` ones:

```haskell
liftST :: ST s a -> EST e s a
liftST (ST f) = EST (\_ -> f)
```

so `EST` is a generalisation of `ST`, good.

Now we need a way to run `EST` computations,
and also a way to early exit in them.

The early exit is the simpler one.
Given that tag prompt brackets the whole computation, we simply jump to the end with `Left e`.
We ignore the captured continuation, we have no use for it.

```haskell
earlyExitEST :: e -> EST e s any
earlyExitEST e = EST (\tag -> control0## tag (\_k s -> (# s, Left e #)))
```

Now, the job for `runEST` is to create the tag and prompt the computation:

```haskell
runEST :: forall e a. (forall s. EST e s a) -> Either e a
runEST (EST f) = runRW#
    -- create tag
    (\s0 -> case newPromptTag# s0 of {
    -- prompt
    (# s1, tag #) -> case prompt# tag
         -- run the `f` inside prompt,
         -- and once we get to the end return `Right` value
         (\s2 -> case f tag s2 of (# s3, a #) -> (# s3, Right a #)) s1 of {
    (# _, a #) -> a }})
```

`runRW#` and forgetting the state at the end is the same as in `runST`, for comparison:

```haskell
runST :: (forall s. ST s a) -> a
runST (ST st_rep) = case runRW# st_rep of (# _, a #) -> a
-- See Note [runRW magic] in GHC.CoreToStg.Prep
```

With all the pieces in place, we can run few simple examples:

```haskell
-- | >>> ex1
-- Left 'x'
ex1 :: Either Char Bool
ex1 = runEST $ earlyExitEST 'x'

-- | >>> ex2
-- Right True
ex2 :: Either Char Bool
ex2 = runEST (return True)
```

Comments & wrinkles
--------

Early exit is one of the simplest "effect" you can implement with delimited continuations.
This is the throwing part of the exceptions, with only top-level exception handler.
It's a nice exercise (and a brain twister) to implement catch blocks.

One wrinkle in this implementation is the `control0##` (not `control0#`) function I used.
The delimited continuations primops are made to work only with `RealWorld`,
not arbitrary `State#` tokens.

I think this is unnecessary specialization [GHC issue #24165](https://gitlab.haskell.org/ghc/ghc/-/issues/24165),
I was advice to simply use `unsafeIOToST`, so I did:

```haskell
control0##
    :: PromptTag# a
    -> (((State# s -> (# State# s, b #)) -> State# s -> (# State# s, a #))
                                         -> State# s -> (# State# s, a #))
    -> State# s -> (# State# s, b #)
control0## = unsafeCoerce# control0#
```

This still feels silly, especially realizing that [the (only) example in the delimited continuations proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst#examples) goes like

```haskell
type role CC nominal representational
newtype CC ans a = CC (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad) via IO

runCC :: (forall ans. CC ans a) -> a
runCC (CC m) = case runRW# m of (# _, a #) -> a
```

but if you look at that, it's just a `ST` monad done weirdly:

```haskell
newtype ST s a = ST (State# RealWorld -> (# State# RealWorld, a #))
-- not using `s` argument !?
```

There might be a good reason why `CC` should be done like that (other than than primops are `RealWorld` specific), but the proposal doesn't explain that difference.
To me having phantom `ans` instead of using nominally it as in `ST` is suspicious.

Conclusion
----------

Delimited continutations are fun and could be very useful.

But surprisingly, at the moment of writing I cannot find any package on Hackage
using them for anything!
[Search for newPromptTag](https://hackage-search.serokell.io/?q=newPromptTag)
returns only false positives (`ghc-lib` etc) right now.
I wonder why they are unused?

Please try them out!
