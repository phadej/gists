---
title: Linear thoughts
author: Oleg Grenrus
---

I have been wondering about linear logic for a while, and after reading
[Edsko de Vries' blog post: Linearity, Uniqueness, and Haskell](http://edsko.net/2017/01/08/linearity-in-haskell/),
I finally got to dump my thoughts on the topic.

Edsko starts with two example functions, where the second one is following:

```hs
frugal :: a -> (a, a)
frugal x = (x, x)
```

In Haskell functions can reuse their arguments, as `frugal` does. Later he shows
that linear setting we can forbid such functions.

## Pairs

[A taste of linear logic by Philip Wadler](http://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf) is a very nice introduction to the intuitionistic linear logic.
One of the take aways, is that in linear logic, there are two kind of pairs, *"ands"*. And there's still single "or".

So to recap, Take `A` to be the proposition "I have ten zlotys", `B` to be the proposition "I have a pizza", and `C` to be the proposition "I have a cake".
We can say

- *A ⊢ B*: for ten zlotys I may buy a pizza
- *A ⊢ C*: for ten zlotys I may buy a cake

And there are three ways to "pair" the terms:

- *A, A ⊢ B ⊗ C*: for twenty zlotys I can buy both a pizza _and_ a cake,
- *A ⊢ B & C*: for ten zlotys I can buy whichever I choose from a cake _and_ a pizza,
- *A ⊢ B ⊕ C*: for ten zlotys I can buy either a pizza _or_ a cake, but I don't have a choice.

In natural language we'd might say: whichver I choose from a cake _or_ a pizza. That's confusing.

In the Edsko's post, the pairs are assumed to be of the first kind: ⊗, tensors. We cannot write `fst` or `snd` variants for the tensor, but

```hs
uncurry :: (a -o b -o c) -o a ⊗ b -o c
uncurry f p = case p of (x, y) -> f x y
```

is ok.

However, if we work with the second kind of the pair, "with", we cannot write `uncurry`, but can have `fst` and `snd`:

```hs
fst :: a & b -o a
fst (x, _) = a

snd :: a & b -o b
snd (_, y) = y
```

And with this pair we can write something like:

```hs
haveAndEat :: Vector Double -o Vector Double & Vector Double
heveAndEat arr = (write 0 2.3 arr, arr)
-- one should really have different constructor for tensor and with
```

There is no problem, as a callee cannot use both sides of the with, they have to choose
only (and exactly) one half.

I.e. we can write `frugal` as

```hs
frugal :: a -o a & a
frugal x = (x, x)
```

## With

IMHO `with` is a very interesting construction. It seems it cannot exist in a
strict language: only when we apply `fst` or `snd` will force it. In addition,
when we force one half, the thunk for the other can be released immediately.

I guess this can be useful in non-backtracking infinite solution-space traversals,
by using *with* we could be sure we don't introduce space leaks by accidentally
retaining other paths.

## Linear State

Recall a state monad

```hs
newtype State a = State { runState :: s -> (a, s) }
```

We can have linear version of it, and the `Monad` instance would type-check:

```hs
newtype LinearState a = LinearState { runLinearState :: s -o a ⊗ s }

instance Monad LinearState where
  return x = \s -> (x, s)
  m >>= k  = LinearState $ \s -> case runLinearState m s of
     (a, s') -> runLinearState (k a) s'
```

The interesting part, is that this type isn't an instance of `MonadState`.
We cannot write `get` or `put`. And we  cannot write `MonadReader` with `ask = get`
either. But we can have a different class:

```hs
class Monad m => MonadLinearState s m | m -> s where
  linearModify :: (s -o s) -o m ()

instance MonadLinearState s (LinearState s) where
  linearModify f = LinearState $ \s -> ((), f s)
```

Also `LinearWriter`, i.e. the one without `listen` or `pass`.

AFAICS, we could have `MonadLinearState RealWorld# IO`, without problems!

## do-notation

It seems, that if we had linearity, then we can use linearity to handle resources.
We can write `bracket` like functions, where we have to return the token at the end:

```hs
-- Not sure about the arrows, should they be lollipops?
withFile :: FilePath -> (Handle -o IO (Handle ⊗ a)) -> IO a
getFileContents :: Handle -o IO (Handle & String)

lineCount :: FilePath -> IO Int
lineCount fp = withFile fp $ \h -> do
  (h, contents) <- getFileContents h
  return (h, length $ lines contents)
```
  
That's something where `LinearStateT` will make code nicer. Or we can
change `getFileContents :: Handle OPENED -o IO (Handle CLOSED ⊗ String)`, or actually we'd need
to have

```hs
withFile
  :: FilePath
  -> (forall s. Handle s OPENED -o IO (Handle s CLOSED ⊗ a))
  -> IO a
```

The type variable `s` will index our `Handle`, so don't pass a wrong one when we have nested `withFile`s.

But then we couldn't use `do` notation, as state variable will change. We'd
need indexed monads, oh dear.

At this point, I have to mention, that linear types would give us great power,
but explaining this kind of IO (or any "magic" involcing them) to a beginner
won't be easy.

FWIW, `withMutableArray` from Edsko's post is simpler example of this idea of
handling resources.

## Point free

```hs
phadej    @pl \xs -> map f (filter g xs) 
lambdabot map f . filter g              -- looks linear
phadej    @pl \x -> x * x
lambdabot join (*)                      -- but join isn't (Reader)
phadej    @pl \p -> (fst p, snd p)
lambdabot id                            -- sometimes pl is smart
phadej    @pl \p -> (fst p, snd p + 2)
lambdabot liftM2 (,) fst ((2 +) . snd)  -- sometimes it isn't
phadej    @pl \p -> uncurry (\x y -> (x, y + 2)) p
lambdabot second (2 +)                  -- so you have to help it
```

TL;DR concatative languages are linear by default, as there are special
operators to contract and weaken!

## Traversals

It's probablty not obvious, but the type

```hs
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
```

is ok to linearise into:

```hs
linearTraverse
  :: (LinearApplicative f, LinearTraversable t)
  => (a -o f b) -> t a -o f (t b)
```

Or can we reuse `Applicative f`? Hard to say at this point. What arrows should
be there? Polymorphic in arrows, with some relations?

## Conclusion

In this post I used different syntax than in used by Edsko, the one I learned
from watching lectures and reading papers on linear logic. And I have to agree,
it's hard to evaluate how well type system will work in practice. Will we need
*with*, how higher order functions or polymorphism will fit etc. I'm also
looking forward how Haskell could support linearity, and what cool stuff we can
do with it!
