---
title: "Bitonic sort: an example of Overloaded:Categories and Staged programming"
author: Oleg Grenrus
tags: overloaded
---

Two weeks ago I wrote about [`Overloaded:Categories`](https://oleg.fi/gists/posts/2020-05-04-overloaded-categories.html),
and mentioned two brief examples. In this post I will walk through
a more complete example. The topic is 
[bitonic sorters](https://en.wikipedia.org/wiki/Bitonic_sorter).

Such domain "networks with N-inputs and M-outputs" are unfairly well suited for category representation.
But first let me show few pictures[^satisfaction].
We'll see how this pictures are drawn from the same code as computation.
The horizontal lines represent signals.
The vertical lines represent compare-and-swap operator. I.e. for this two
inputs we apply a function

```haskell
cas :: Ord a => (a,a) -> (a,a)
cas (x,y) = if x < y then (x,y) else (y,x)
```

[^satisfaction]: In fact most fun part for me was to see those fractal-like
pictures emerge. The fact that one can also compute with them is just a bonus.

For four inputs:

\begin{tikzpicture}
\foreach \a in {0,...,3}
  \draw[thick] (0,\a) -- ++(4.5,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,3);
\filldraw (1,2) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (2,0) -- (2,3);
\filldraw (2,0) circle (3pt);
\filldraw (2,3) circle (3pt);
\draw[thick] (2.5,1) -- (2.5,2);
\filldraw (2.5,1) circle (3pt);
\filldraw (2.5,2) circle (3pt);
\draw[thick] (3.5,0) -- (3.5,1);
\filldraw (3.5,0) circle (3pt);
\filldraw (3.5,1) circle (3pt);
\draw[thick] (3.5,2) -- (3.5,3);
\filldraw (3.5,2) circle (3pt);
\filldraw (3.5,3) circle (3pt);
\end{tikzpicture}


Eight inputs:

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(9.5,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,3);
\filldraw (1,2) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (2,0) -- (2,3);
\filldraw (2,0) circle (3pt);
\filldraw (2,3) circle (3pt);
\draw[thick] (2.5,1) -- (2.5,2);
\filldraw (2.5,1) circle (3pt);
\filldraw (2.5,2) circle (3pt);
\draw[thick] (3.5,0) -- (3.5,1);
\filldraw (3.5,0) circle (3pt);
\filldraw (3.5,1) circle (3pt);
\draw[thick] (3.5,2) -- (3.5,3);
\filldraw (3.5,2) circle (3pt);
\filldraw (3.5,3) circle (3pt);
\draw[thick] (1,4) -- (1,5);
\filldraw (1,4) circle (3pt);
\filldraw (1,5) circle (3pt);
\draw[thick] (1,6) -- (1,7);
\filldraw (1,6) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (2,4) -- (2,7);
\filldraw (2,4) circle (3pt);
\filldraw (2,7) circle (3pt);
\draw[thick] (2.5,5) -- (2.5,6);
\filldraw (2.5,5) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (3.5,4) -- (3.5,5);
\filldraw (3.5,4) circle (3pt);
\filldraw (3.5,5) circle (3pt);
\draw[thick] (3.5,6) -- (3.5,7);
\filldraw (3.5,6) circle (3pt);
\filldraw (3.5,7) circle (3pt);
\draw[thick] (4.5,0) -- (4.5,7);
\filldraw (4.5,0) circle (3pt);
\filldraw (4.5,7) circle (3pt);
\draw[thick] (5,1) -- (5,6);
\filldraw (5,1) circle (3pt);
\filldraw (5,6) circle (3pt);
\draw[thick] (5.5,2) -- (5.5,5);
\filldraw (5.5,2) circle (3pt);
\filldraw (5.5,5) circle (3pt);
\draw[thick] (6,3) -- (6,4);
\filldraw (6,3) circle (3pt);
\filldraw (6,4) circle (3pt);
\draw[thick] (7,0) -- (7,2);
\filldraw (7,0) circle (3pt);
\filldraw (7,2) circle (3pt);
\draw[thick] (7.5,1) -- (7.5,3);
\filldraw (7.5,1) circle (3pt);
\filldraw (7.5,3) circle (3pt);
\draw[thick] (8.5,0) -- (8.5,1);
\filldraw (8.5,0) circle (3pt);
\filldraw (8.5,1) circle (3pt);
\draw[thick] (8.5,2) -- (8.5,3);
\filldraw (8.5,2) circle (3pt);
\filldraw (8.5,3) circle (3pt);
\draw[thick] (7,4) -- (7,6);
\filldraw (7,4) circle (3pt);
\filldraw (7,6) circle (3pt);
\draw[thick] (7.5,5) -- (7.5,7);
\filldraw (7.5,5) circle (3pt);
\filldraw (7.5,7) circle (3pt);
\draw[thick] (8.5,4) -- (8.5,5);
\filldraw (8.5,4) circle (3pt);
\filldraw (8.5,5) circle (3pt);
\draw[thick] (8.5,6) -- (8.5,7);
\filldraw (8.5,6) circle (3pt);
\filldraw (8.5,7) circle (3pt);
\end{tikzpicture}


Or sixteen inputs:

\begin{tikzpicture}
\foreach \a in {0,...,15}
  \draw[thick] (0,\a) -- ++(19,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,3);
\filldraw (1,2) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (2,0) -- (2,3);
\filldraw (2,0) circle (3pt);
\filldraw (2,3) circle (3pt);
\draw[thick] (2.5,1) -- (2.5,2);
\filldraw (2.5,1) circle (3pt);
\filldraw (2.5,2) circle (3pt);
\draw[thick] (3.5,0) -- (3.5,1);
\filldraw (3.5,0) circle (3pt);
\filldraw (3.5,1) circle (3pt);
\draw[thick] (3.5,2) -- (3.5,3);
\filldraw (3.5,2) circle (3pt);
\filldraw (3.5,3) circle (3pt);
\draw[thick] (1,4) -- (1,5);
\filldraw (1,4) circle (3pt);
\filldraw (1,5) circle (3pt);
\draw[thick] (1,6) -- (1,7);
\filldraw (1,6) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (2,4) -- (2,7);
\filldraw (2,4) circle (3pt);
\filldraw (2,7) circle (3pt);
\draw[thick] (2.5,5) -- (2.5,6);
\filldraw (2.5,5) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (3.5,4) -- (3.5,5);
\filldraw (3.5,4) circle (3pt);
\filldraw (3.5,5) circle (3pt);
\draw[thick] (3.5,6) -- (3.5,7);
\filldraw (3.5,6) circle (3pt);
\filldraw (3.5,7) circle (3pt);
\draw[thick] (4.5,0) -- (4.5,7);
\filldraw (4.5,0) circle (3pt);
\filldraw (4.5,7) circle (3pt);
\draw[thick] (5,1) -- (5,6);
\filldraw (5,1) circle (3pt);
\filldraw (5,6) circle (3pt);
\draw[thick] (5.5,2) -- (5.5,5);
\filldraw (5.5,2) circle (3pt);
\filldraw (5.5,5) circle (3pt);
\draw[thick] (6,3) -- (6,4);
\filldraw (6,3) circle (3pt);
\filldraw (6,4) circle (3pt);
\draw[thick] (7,0) -- (7,2);
\filldraw (7,0) circle (3pt);
\filldraw (7,2) circle (3pt);
\draw[thick] (7.5,1) -- (7.5,3);
\filldraw (7.5,1) circle (3pt);
\filldraw (7.5,3) circle (3pt);
\draw[thick] (8.5,0) -- (8.5,1);
\filldraw (8.5,0) circle (3pt);
\filldraw (8.5,1) circle (3pt);
\draw[thick] (8.5,2) -- (8.5,3);
\filldraw (8.5,2) circle (3pt);
\filldraw (8.5,3) circle (3pt);
\draw[thick] (7,4) -- (7,6);
\filldraw (7,4) circle (3pt);
\filldraw (7,6) circle (3pt);
\draw[thick] (7.5,5) -- (7.5,7);
\filldraw (7.5,5) circle (3pt);
\filldraw (7.5,7) circle (3pt);
\draw[thick] (8.5,4) -- (8.5,5);
\filldraw (8.5,4) circle (3pt);
\filldraw (8.5,5) circle (3pt);
\draw[thick] (8.5,6) -- (8.5,7);
\filldraw (8.5,6) circle (3pt);
\filldraw (8.5,7) circle (3pt);
\draw[thick] (1,8) -- (1,9);
\filldraw (1,8) circle (3pt);
\filldraw (1,9) circle (3pt);
\draw[thick] (1,10) -- (1,11);
\filldraw (1,10) circle (3pt);
\filldraw (1,11) circle (3pt);
\draw[thick] (2,8) -- (2,11);
\filldraw (2,8) circle (3pt);
\filldraw (2,11) circle (3pt);
\draw[thick] (2.5,9) -- (2.5,10);
\filldraw (2.5,9) circle (3pt);
\filldraw (2.5,10) circle (3pt);
\draw[thick] (3.5,8) -- (3.5,9);
\filldraw (3.5,8) circle (3pt);
\filldraw (3.5,9) circle (3pt);
\draw[thick] (3.5,10) -- (3.5,11);
\filldraw (3.5,10) circle (3pt);
\filldraw (3.5,11) circle (3pt);
\draw[thick] (1,12) -- (1,13);
\filldraw (1,12) circle (3pt);
\filldraw (1,13) circle (3pt);
\draw[thick] (1,14) -- (1,15);
\filldraw (1,14) circle (3pt);
\filldraw (1,15) circle (3pt);
\draw[thick] (2,12) -- (2,15);
\filldraw (2,12) circle (3pt);
\filldraw (2,15) circle (3pt);
\draw[thick] (2.5,13) -- (2.5,14);
\filldraw (2.5,13) circle (3pt);
\filldraw (2.5,14) circle (3pt);
\draw[thick] (3.5,12) -- (3.5,13);
\filldraw (3.5,12) circle (3pt);
\filldraw (3.5,13) circle (3pt);
\draw[thick] (3.5,14) -- (3.5,15);
\filldraw (3.5,14) circle (3pt);
\filldraw (3.5,15) circle (3pt);
\draw[thick] (4.5,8) -- (4.5,15);
\filldraw (4.5,8) circle (3pt);
\filldraw (4.5,15) circle (3pt);
\draw[thick] (5,9) -- (5,14);
\filldraw (5,9) circle (3pt);
\filldraw (5,14) circle (3pt);
\draw[thick] (5.5,10) -- (5.5,13);
\filldraw (5.5,10) circle (3pt);
\filldraw (5.5,13) circle (3pt);
\draw[thick] (6,11) -- (6,12);
\filldraw (6,11) circle (3pt);
\filldraw (6,12) circle (3pt);
\draw[thick] (7,8) -- (7,10);
\filldraw (7,8) circle (3pt);
\filldraw (7,10) circle (3pt);
\draw[thick] (7.5,9) -- (7.5,11);
\filldraw (7.5,9) circle (3pt);
\filldraw (7.5,11) circle (3pt);
\draw[thick] (8.5,8) -- (8.5,9);
\filldraw (8.5,8) circle (3pt);
\filldraw (8.5,9) circle (3pt);
\draw[thick] (8.5,10) -- (8.5,11);
\filldraw (8.5,10) circle (3pt);
\filldraw (8.5,11) circle (3pt);
\draw[thick] (7,12) -- (7,14);
\filldraw (7,12) circle (3pt);
\filldraw (7,14) circle (3pt);
\draw[thick] (7.5,13) -- (7.5,15);
\filldraw (7.5,13) circle (3pt);
\filldraw (7.5,15) circle (3pt);
\draw[thick] (8.5,12) -- (8.5,13);
\filldraw (8.5,12) circle (3pt);
\filldraw (8.5,13) circle (3pt);
\draw[thick] (8.5,14) -- (8.5,15);
\filldraw (8.5,14) circle (3pt);
\filldraw (8.5,15) circle (3pt);
\draw[thick] (9.5,0) -- (9.5,15);
\filldraw (9.5,0) circle (3pt);
\filldraw (9.5,15) circle (3pt);
\draw[thick] (10,1) -- (10,14);
\filldraw (10,1) circle (3pt);
\filldraw (10,14) circle (3pt);
\draw[thick] (10.5,2) -- (10.5,13);
\filldraw (10.5,2) circle (3pt);
\filldraw (10.5,13) circle (3pt);
\draw[thick] (11,3) -- (11,12);
\filldraw (11,3) circle (3pt);
\filldraw (11,12) circle (3pt);
\draw[thick] (11.5,4) -- (11.5,11);
\filldraw (11.5,4) circle (3pt);
\filldraw (11.5,11) circle (3pt);
\draw[thick] (12,5) -- (12,10);
\filldraw (12,5) circle (3pt);
\filldraw (12,10) circle (3pt);
\draw[thick] (12.5,6) -- (12.5,9);
\filldraw (12.5,6) circle (3pt);
\filldraw (12.5,9) circle (3pt);
\draw[thick] (13,7) -- (13,8);
\filldraw (13,7) circle (3pt);
\filldraw (13,8) circle (3pt);
\draw[thick] (14,0) -- (14,4);
\filldraw (14,0) circle (3pt);
\filldraw (14,4) circle (3pt);
\draw[thick] (14.5,1) -- (14.5,5);
\filldraw (14.5,1) circle (3pt);
\filldraw (14.5,5) circle (3pt);
\draw[thick] (15,2) -- (15,6);
\filldraw (15,2) circle (3pt);
\filldraw (15,6) circle (3pt);
\draw[thick] (15.5,3) -- (15.5,7);
\filldraw (15.5,3) circle (3pt);
\filldraw (15.5,7) circle (3pt);
\draw[thick] (16.5,0) -- (16.5,2);
\filldraw (16.5,0) circle (3pt);
\filldraw (16.5,2) circle (3pt);
\draw[thick] (17,1) -- (17,3);
\filldraw (17,1) circle (3pt);
\filldraw (17,3) circle (3pt);
\draw[thick] (18,0) -- (18,1);
\filldraw (18,0) circle (3pt);
\filldraw (18,1) circle (3pt);
\draw[thick] (18,2) -- (18,3);
\filldraw (18,2) circle (3pt);
\filldraw (18,3) circle (3pt);
\draw[thick] (16.5,4) -- (16.5,6);
\filldraw (16.5,4) circle (3pt);
\filldraw (16.5,6) circle (3pt);
\draw[thick] (17,5) -- (17,7);
\filldraw (17,5) circle (3pt);
\filldraw (17,7) circle (3pt);
\draw[thick] (18,4) -- (18,5);
\filldraw (18,4) circle (3pt);
\filldraw (18,5) circle (3pt);
\draw[thick] (18,6) -- (18,7);
\filldraw (18,6) circle (3pt);
\filldraw (18,7) circle (3pt);
\draw[thick] (14,8) -- (14,12);
\filldraw (14,8) circle (3pt);
\filldraw (14,12) circle (3pt);
\draw[thick] (14.5,9) -- (14.5,13);
\filldraw (14.5,9) circle (3pt);
\filldraw (14.5,13) circle (3pt);
\draw[thick] (15,10) -- (15,14);
\filldraw (15,10) circle (3pt);
\filldraw (15,14) circle (3pt);
\draw[thick] (15.5,11) -- (15.5,15);
\filldraw (15.5,11) circle (3pt);
\filldraw (15.5,15) circle (3pt);
\draw[thick] (16.5,8) -- (16.5,10);
\filldraw (16.5,8) circle (3pt);
\filldraw (16.5,10) circle (3pt);
\draw[thick] (17,9) -- (17,11);
\filldraw (17,9) circle (3pt);
\filldraw (17,11) circle (3pt);
\draw[thick] (18,8) -- (18,9);
\filldraw (18,8) circle (3pt);
\filldraw (18,9) circle (3pt);
\draw[thick] (18,10) -- (18,11);
\filldraw (18,10) circle (3pt);
\filldraw (18,11) circle (3pt);
\draw[thick] (16.5,12) -- (16.5,14);
\filldraw (16.5,12) circle (3pt);
\filldraw (16.5,14) circle (3pt);
\draw[thick] (17,13) -- (17,15);
\filldraw (17,13) circle (3pt);
\filldraw (17,15) circle (3pt);
\draw[thick] (18,12) -- (18,13);
\filldraw (18,12) circle (3pt);
\filldraw (18,13) circle (3pt);
\draw[thick] (18,14) -- (18,15);
\filldraw (18,14) circle (3pt);
\filldraw (18,15) circle (3pt);
\end{tikzpicture}

In the last network, there are 80 comparators. The network is not optimal,
there is known network using just 60 comparators,
you can find its diagram in *The Art Of Computer Programming*.
Knuth also explains how bitonic sort networks are constructed
i.e. why they work.

Categorical setup
-----------------

I should immediately point out that this wouldn't be doable
with arrows, as the categories have `Functor`s as objects
i.e. type constructors of kind `Type -> Type`, not just `Type`s.

Lets define a super-concise names for well-known functors.
We will see soon why this is a good idea.

```haskell
newtype I         a = I a          -- Identity
data    U         a = U            -- Proxy
data    (f :*: g) a = f a :*: g a  -- Product
```

Next we define one useful example category type:

```haskell
newtype A x f g = A { runA :: f x -> g x }
```

Note, this is not a natural transformation, as the `x` argument is not closed over (no `RankNTypes`).
We'll need it "outside", so we can require `Ord x` needed for sorting.
The type argument is unorthodox, but this way it fits into `Category` class.
The `A x` instances resemble the `(->)` instances:

```haskell
instance Category (A x) where
    id        = A id
    A f . A g = A (f . g)

instance CategoryWith1 (A x) where
    type Terminal (A x) = U

    terminal = A (\_ -> U)

instance CartesianCategory (A x) where
    type Product (A x) = (:*:)

    proj1 = A (\(x :*: _) -> x)
    proj2 = A (\(_ :*: y) -> y)

    fanout (A f) (A g) = A (\x -> f x :*: g x)
```

We also define a new ad-hoc type-class, `SortingNetwork`.
It basically says that sorting networks are cartesian categories,
where the categorical product is the functor product `:*:`.
The `block` operator is used to place wires on the diagrams, otherwise it does nothing.

```haskell
class (CartesianCategory cat, Product cat ~ (:*:))
    => SortingNetwork cat
  where
    block :: cat a b -> cat a b
    block = id
```


Bitonic sorter
--------------

Bitonic sorter networks are defined recursively.
We'll use Haskell's type-class machinery to synthesize them for us.
Before we can dive into actual sorter, we need two helper primitives:

```haskell
class Pairwise f g where
    pairwise :: SortingNetwork cat
             => cat (I :*: I) (I :*: I)
             -> cat (f :*: g) (f :*: g)

class Opposite f g where
    opposite :: SortingNetwork cat
             => cat (I :*: I) (I :*: I)
             -> cat (f :*: g) (f :*: g)
```

These primitives are quite similar. Given some primitive two-wire function (like compare-and-swap),
the `pairwise` connects wires in two blocks in order:

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(3.5,0);
\draw[thick] (1,0) -- (1,4);
\filldraw (1,0) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (1.5,1) -- (1.5,5);
\filldraw (1.5,1) circle (3pt);
\filldraw (1.5,5) circle (3pt);
\draw[thick] (2,2) -- (2,6);
\filldraw (2,2) circle (3pt);
\filldraw (2,6) circle (3pt);
\draw[thick] (2.5,3) -- (2.5,7);
\filldraw (2.5,3) circle (3pt);
\filldraw (2.5,7) circle (3pt);
\end{tikzpicture}

and `opposite` connects them in opposite order:

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(3.5,0);
\draw[thick] (1,0) -- (1,7);
\filldraw (1,0) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (1.5,1) -- (1.5,6);
\filldraw (1.5,1) circle (3pt);
\filldraw (1.5,6) circle (3pt);
\draw[thick] (2,2) -- (2,5);
\filldraw (2,2) circle (3pt);
\filldraw (2,5) circle (3pt);
\draw[thick] (2.5,3) -- (2.5,4);
\filldraw (2.5,3) circle (3pt);
\filldraw (2.5,4) circle (3pt);
\end{tikzpicture}


Both operations are parallel if the "hardware" supports it.
(Unfortunately we cannot take advantage of that in Haskell, at least easily).

The instances are simple to define. The base case is `I I`.
Then we can return the primitive function directly:

```haskell
instance Pairwise I I where
    pairwise = id
```

The induction case is for product `:*:`. We require
that halves of product can be connected pairwise.
Because of this definition we essentially can make only networks
with size $2^N$. That is not a real issue, as we will see later.

The instance definition is *easy* using `Overloaded:Categories`.
Manual definition wouldn't be as pretty:

```haskell
instance (Pairwise x u, Pairwise y v) => Pairwise (x :*: y) (u :*: v) where
    pairwise f = proc ((x,y), (u,v)) -> do
        (x',u') <- pairwise f -< (x,u)
        (y',v') <- pairwise f -< (y,v)
        identity -< ((x',y'), (u',v'))
```

The `Opposite` instances are defined similarly.

If you have already examined the bitonic sorter networks, you noticed
that they are composed from smaller `Pairwise` and `Opposite` blocks.
So this is all we need to define bitonic sorter networks.

Bitonic sorter is kind-of merge-sort. First we sort the two halves,
and then we merge them. Thus the type-class for them has two
methods:

```haskell
class BitonicSort f where
    bitonicSort :: SortingNetwork cat
                => cat (I :*: I) (I :*: I)
                -> cat f f

    bitonicMerge :: SortingNetwork cat
                 => cat (I :*: I) (I :*: I)
                 -> cat f f
```

The base case for `I` is easy. With only one wire there is nothing to sort:

```haskell
instance BitonicSort I where
    bitonicSort  _ = identity
    bitonicMerge _ = identity
```

The induction step shows the structure.

The sort is recursive:
1. sort the halves
2. cas the opposites
3. merge the halves.

The merge is also recursive:
1. cas pairwise
2. merge the halves.

```haskell
instance (Pairwise f g, Opposite f g, BitonicSort f, BitonicSort g)
    => BitonicSort (f :*: g)
  where
    bitonicSort cas =
        block (bitonicSort cas *** bitonicSort cas) >>>
        block (opposite cas) >>>
        block (bitonicMerge cas *** bitonicMerge cas)

    bitonicMerge cas =
        block (pairwise cas) >>>
        block (bitonicMerge cas *** bitonicMerge cas)

(***) :: CartesianCategory cat
      => cat a b -> cat c d -> cat (Product cat a c) (Product cat b d)
f *** g = fanout (proj1 >>> f) (proj2 >>> g)
```

Here I haven't used `Overloaded:Categories`, as it doesn't offer much help.
Here *point-free* approach is clearer.

Computing with networks
-----------------------

As we have have networks defined for all sufficient categories,
we can use the already define concrete instance `A x`.
We simply need to instantiate `bitonicSort`,
with a primitive compare-and-swap element:

```haskell
cas :: A a (I :*: I) (I :*: I)
cas = A $ \(I x :*: I  y) ->
    if x <= y
    then I x :*: I y
    else I y :*: I x
```

Then the sort is just plumbing of data constructors.
To avoid using tuples in the result I define function in continuation passing style.

```haskell
sort4 :: Ord a
      => a -> a -> a -> a
      -> (a -> a -> a -> a -> r)
      -> r
sort4 a b c d kont = 
    let input = (I a  :*: I b ) :*: (I c  :*: I d)
        (I a' :*: I b') :*: (I c' :*: I d') = runA (bitonicSort cas) input

    in kont a' b' c' d'
````

We can `QuickCheck` this function:

```haskell
quickCheck $ \a b c d ->
    sort4 a b c d $ \a' b' c' d' ->
        sort [a,b,c,d] === [a',b',c',d' :: Int]
```

It works perfectly.

Below is the Core for `sort4`.
There are no allocations, just `jump`s around (and six compare-and-swaps
as in the diagram).
There are few casts as types of some continuations are mentioning
`I`. The `:*:` constructors are however gone. (I was positively surprised how well GHC does job here).

```haskell
$workerSort4
$workerSort4
  = \ @ a
      @ r
      lessEqual
      x1 x2 x3 x4
      kont ->
      join {
        $kont1 y1 y2
          = join {
              $kont2 z1 z2 z3 z4
                = join {
                    $kont3 w1 w2 w3 w4
                      = join {
                          $kont4 q1 q2
                            = case lessEqual (w2 `cast` <Co:2>) (w4 `cast` <Co:2>)
                              of {
                                False ->
                                  kont
                                    (q1 `cast` <Co:2>)
                                    (q2 `cast` <Co:2>)
                                    (w4 `cast` <Co:2>)
                                    (w2 `cast` <Co:2>);
                                True ->
                                  kont
                                    (q1 `cast` <Co:2>)
                                    (q2 `cast` <Co:2>)
                                    (w2 `cast` <Co:2>)
                                    (w4 `cast` <Co:2>)
                              } } in
                        case lessEqual (w3 `cast` <Co:2>) (w1 `cast` <Co:2>) of {
                          False -> jump $kont4 w1 w3;
                          True  -> jump $kont4 w3 w1
                        } } in
                  case lessEqual (z1 `cast` <Co:2>) (z4 `cast` <Co:2>) of {
                    False ->
                      case lessEqual (z2 `cast` <Co:2>) (z3 `cast` <Co:2>) of {
                        False -> jump $kont3 z3 z2 z4 z1;
                        True  -> jump $kont3 z2 z3 z4 z1
                      };
                    True ->
                      case lessEqual (z2 `cast` <Co:2>) (z3 `cast` <Co:2>) of {
                        False -> jump $kont3 z3 z2 z1 z4;
                        True  -> jump $kont3 z2 z3 z1 z4
                      }
                  } } in
            case lessEqual x3 x4 of {
              False -> jump $kont2 y1 y2 (x4 `cast` <Co:3>) (x3 `cast` <Co:3>);
	      True  -> jump $kont2 y1 y2 (x3 `cast` <Co:3>) (x4 `cast` <Co:3>)
            } } in
      case lessEqual x1 x2 of {
        False -> jump $kont1 (x2 `cast` <Co:3>) (x1 `cast` <Co:3>);
        True  -> jump $kont1 (x1 `cast` <Co:3>) (x2 `cast` <Co:3>)
      }
```

Staged programming
------------------

It's great that GHC optimises the above example.
But will it do as great job for network of 16 inputs?
You don't know, untill you try and inspect the produced Core.

There is a way to be sure, by constructing good code: *Template Haskell*.
The commonly used Template Haskell is untyped and error prone,
but there is also *Typed Template Haskell* variant.
It is quite simple (to the first approximation).
Instead of single delimiters like `[| Just $x |] :: ExpQ`, we use
double delimiters:

```haskell
[|| Just $$x ||] :: TExpQ (Maybe Int) -- if x :: TExpQ Int
```

There is one trick to know. Instead of direct result returning as we had
with `A x` we need to use continuation-passing-style.

```haskell
type Code a = TExpQ a

newtype Staged x f g = St
  { runStaged :: forall r. f (Code x) -> (g (Code x) -> Code r) -> Code r }
```

Rewriting `Category` and `CartesianCategory` instances is an easy exercise.
But why we need CPS? That can be illustrated by an implementation of 
`cas`.

```haskell
cas :: Code (a -> a -> Bool) -> Staged a (I :*: I) (I :*: I)
cas le = St $ \(I x :*: I y) kont ->
    [|| let joinP a b = $$(kont (I [|| a ||] :*: I [|| b ||]))
        in
        if $$le $$x $$y
        then joinP $$x $$y
        else joinP $$y $$x
    ||]
```

The continuation provides the code "which will happen later", so we can
generate `let` (which is recognized as a join point).
If we used direct style, there wouldn't be a way to generate code
like that. 

Notice that we pass comparator as `Code (a -> a -> Bool)`.
This is because currently TTH doesn't work well with type-class methods.
This is a workaround.

With `cas` done and `Staged` having category instances we can
instantiate `bitonicSort`. This code is the same as
with non-TH version previously.

```haskell
sort4code
      :: Code (a -> a -> Bool)
      -> Code a -> Code a -> Code a -> Code a
      -> (Code a -> Code a -> Code a -> Code a -> Code r)
      -> Code r
sort4code le a b c d kont = 
    let input = (I a  :*: I b ) :*: (I c  :*: I d)
    in runStaged (bitonicSort $ cas le) input $
        \((I a' :*: I b') :*: (I c' :*: I d')) ->
            kont a' b' c' d'
```

And then we can use it. We *generate* the sorting code using local names
bound by `sort4` definition:

```haskell
sort4 :: (a -> a -> Bool)
      -> a -> a -> a -> a
      -> (a -> a -> a -> a -> r)
      -> r
sort4 le a b c d kont =
    $$(sort4code [|| le ||] [|| a ||] [|| b ||] [|| c ||] [|| d ||] $
        \a' b' c' d' -> [|| kont $$a' $$b' $$c' $$d' ||]
    ) 
```

We can examine first what we generate, using
[`-ddump-splices`](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/debugging.html?highlight=dump%20splices#ghc-flag--ddump-splices)
(The names are cleaned up):

```haskell
lessThan x1 x2 x3 x4 kont =
    let
      joinP0 y1 y2
        = let
            joinP1 y3 y4
              = let
                  joinP2 z1 z2
                    = let
                        joinP3 z3 z4
                          = let
                              joinP4 w1 w2
                                = let
                                    joinP5 w3 w4
                                      = kont w1 w2 w3 w4
                                  in
                                    if lessThan z4 z2 then
                                        joinP5 z4 z2
                                    else
                                        joinP5 z2 z4
                            in
                              if lessThan z1 z3 then
                                  joinP4 z1 z3
                              else
                                  joinP4 z3 z1
                      in
                        if lessThan y2 y3 then
                            joinP3 y2 y3
                        else
                            joinP3 y3 y2
                in
                  if lessThan y1 y4 then
                      joinP2 y1 y4
                  else
                      joinP2 y4 y1
          in
            if lessThan x3 x4 then
                joinP1 x3 x4
            else
                joinP1 x4 x3
    in
      if lessThan x1 x2 then
          joinP0 x1 x2
      else
          joinP0 x2 x1
```

It looks very nice. The GHC correctly
recognizes the jump points,
otherwise the generated Core
(with [`-ddump-simpl`](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/debugging.html?highlight=dump%20splices#ghc-flag--ddump-simpl))
is essentially the same.
There weren't left much for GHC to do.
The produced Core is similar to the variant GHC optimised from direct definition,
except this one is more regular (I'm not sure why the Core above is slightly irregular).
But most importantly, we know we will get this good result using staged programming / TTH
as it is constructed explicitly.
We are not relying on compiler being sufficiently smart, we tell it what to do.

It's good to have smart compiler,
but in some specific use-cases general compiler general heuristics might not
be well suited or enough.
In particular, one might want to use even exponential-time optimization
routine in some specific case, but no-one would like that used for everything.

```haskell
sort4
  = \ @ a_ad1P
      @ r_ad1Q
      lessThan
      x1 x2 x3 x4
      kont ->
      let {
        joinP0
          = \ y1 y2 ->
              let {
                joinP1
                  = \ y3 y4 ->
                      let {
                        joinP2
                          = \ z1 z2 ->
                              let {
                                joinP3
                                  = \ z3 z4 ->
                                      let {
                                        joinP4
                                          = \ w1 w2 ->
                                              let {
                                                joinP5
                                                  = \ w3 w4 ->
                                                      kont w1 w2 w3 w4
                                                   } in
                                              case lessThan z4 z2 of {
                                                False -> joinP5 z2 z4;
                                                True  -> joinP5 z4 z2
                                              } } in
                                      case lessThan z1 z3 of {
                                        False -> joinP4 z3 z1;
                                        True  -> joinP4 z1 z3
                                      } } in
                              case lessThan y2 y3 of {
                                False -> joinP3 y3 y2;
                                True  -> joinP3 y2 y3
                              } } in
                      case lessThan y1 y4 of {
                        False -> joinP2 y4 y1;
                        True  -> joinP2 y1 y4
                      } } in
              case lessThan x3 x4 of {
                False -> joinP1 x4 x3;
                True  -> joinP1 x3 x4
              } } in
      case lessThan x1 x2 of {
        False -> joinP0 x2 x1;
        True ->  joinP0 x1 x2
      }
```

Another important observation is that we didn't need to touch `bitonicSort` definition.
We only created a new instance of `Category` with a `cas` primitive.
Then the code was generated for us (truly at compile time).

Third instantiation: diagrams
-----------------------------

The diagram drawing code is yet another instantiation.
The type I used is quite simple:

```haskell
newtype Diagram f g =
    D { runD :: forall r. f Int -> (g Int -> [Piece] -> r) -> r  }

data Piece = CAS Int Int
           | Pieces [Piece]
```

We provide `f` with distinct integers for each wire,
and at the end we'll get which one are compared (and in the proper order).
The `block` operator of `SortingNetwork` will create `Pieces`,
so we get more nicely aligned diagrams.

There is also a neat trick for generating the initial wire numbers.
Recall that we used three `Functor`s. They are also `Traversable`
and `Applicative`. Thus we can generate initial `f Int` using
a traversal with `State`, or more conveniently using `mapAccumL`.

```haskell
input :: (Applicative f, Traversable f) => f Int
input = snd $ mapAccumL (\s () -> (s + 1 ,s)) 0 (pure ())
```


Why we need U?
--------------

So far we have been limited to networks of $2^N$ size.
We can get all other networks by simply omitting top or bottom wires,
treating them as *always smaller* or *always bigger* values.
This is the second reason why we bothered with Functor kind.
First one is that staging example wouldn't been possible either.[^pairs]

[^pairs]: Consider what is the difference between `(Code a, Code a)` and `Code (a,a)`.

For example if we need a sorting network of six inputs,
we can instantiate it for a functor

```haskell
type Six   = ((U :*: I) :*: (I :*: I)) :*: ((I :*: I) :*: (I :*: U)) 
```

The resulting network is a truncated version of the eight network:

\begin{tikzpicture}
\foreach \a in {0,...,5}
  \draw[thick] (0,\a) -- ++(8,0);
\draw[thick] (1,1) -- (1,2);
\filldraw (1,1) circle (3pt);
\filldraw (1,2) circle (3pt);
\draw[thick] (2,0) -- (2,1);
\filldraw (2,0) circle (3pt);
\filldraw (2,1) circle (3pt);
\draw[thick] (3,1) -- (3,2);
\filldraw (3,1) circle (3pt);
\filldraw (3,2) circle (3pt);
\draw[thick] (1,3) -- (1,4);
\filldraw (1,3) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (2,4) -- (2,5);
\filldraw (2,4) circle (3pt);
\filldraw (2,5) circle (3pt);
\draw[thick] (3,3) -- (3,4);
\filldraw (3,3) circle (3pt);
\filldraw (3,4) circle (3pt);
\draw[thick] (4,0) -- (4,5);
\filldraw (4,0) circle (3pt);
\filldraw (4,5) circle (3pt);
\draw[thick] (4.5,1) -- (4.5,4);
\filldraw (4.5,1) circle (3pt);
\filldraw (4.5,4) circle (3pt);
\draw[thick] (5,2) -- (5,3);
\filldraw (5,2) circle (3pt);
\filldraw (5,3) circle (3pt);
\draw[thick] (6,0) -- (6,2);
\filldraw (6,0) circle (3pt);
\filldraw (6,2) circle (3pt);
\draw[thick] (7,1) -- (7,2);
\filldraw (7,1) circle (3pt);
\filldraw (7,2) circle (3pt);
\draw[thick] (6,3) -- (6,5);
\filldraw (6,3) circle (3pt);
\filldraw (6,5) circle (3pt);
\draw[thick] (7,3) -- (7,4);
\filldraw (7,3) circle (3pt);
\filldraw (7,4) circle (3pt);
\end{tikzpicture}

This network has 13 comparators (optimal has 12). Not bad.


Conclusion
----------

Programming with categories is fun. They offer
inspectable static-shape computation as does `Applicative`s, but
they have quite different ergonomics.

Where `Applicative` concentrates on "the objects",
category (or `Arrow`) approach is concentrated on "the arrows".
Which one suits your particular domain depends.
Categories work very well for sorting networks, sorting network is "an arrow",
not the end result.
I have hard time imagining how this can be represented
with `Applicative`s.

We defined a *bitonic sort* using categories (with a little help of `Overloaded:Categories`),
which we than instantiated three times:
- We interpreted the code directly
- Used Typed Template Haskell to generate more optimal code
- And lastly we drew diagrams from the same definitions.

We can (and maybe should) to continue and add an optimization
layer - even the simple - one.
If we further remove a wire from six-network above
we will get a network with a redundant comparator.
I leave the implementation of an optimizer as a tough but fun and rewarding exercise for a reader ;)

\begin{tikzpicture}
\foreach \a in {0,...,4}
  \draw[thick] (0,\a) -- ++(7.5,0);
\draw[thick] (1,1) -- (1,2);
\filldraw (1,1) circle (3pt);
\filldraw (1,2) circle (3pt);
\draw[thick] (2,0) -- (2,1);
\filldraw (2,0) circle (3pt);
\filldraw (2,1) circle (3pt);
\draw[thick] (3,1) -- (3,2);
\filldraw (3,1) circle (3pt);
\filldraw (3,2) circle (3pt);
\draw[thick] (1,3) -- (1,4);
\filldraw (1,3) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (2,3) -- (2,4);
\filldraw (2,3) circle (3pt);
\filldraw (2,4) circle (3pt);
\draw[thick] (4,1) -- (4,4);
\filldraw (4,1) circle (3pt);
\filldraw (4,4) circle (3pt);
\draw[thick] (4.5,2) -- (4.5,3);
\filldraw (4.5,2) circle (3pt);
\filldraw (4.5,3) circle (3pt);
\draw[thick] (5.5,0) -- (5.5,2);
\filldraw (5.5,0) circle (3pt);
\filldraw (5.5,2) circle (3pt);
\draw[thick] (6.5,1) -- (6.5,2);
\filldraw (6.5,1) circle (3pt);
\filldraw (6.5,2) circle (3pt);
\draw[thick] (5.5,3) -- (5.5,4);
\filldraw (5.5,3) circle (3pt);
\filldraw (5.5,4) circle (3pt);
\end{tikzpicture}

*Hints:* The optimizer usage looks like:

```haskell
sort5optDiagram :: Diagram Five Five
sort5optDiagram = runOptimize $ 
    bitonicSort (optimizedCas cas) >>>
    bitonicSort (optimizedCas cas)
```

Also https://www.well-typed.com/blog/2014/12/simple-smt-solver/ and
and [zero-one principle](https://en.wikipedia.org/wiki/Sorting_network#Zero-one_principle)
may be useful.

So even we sort twice, we get only nine comparator network:

\begin{tikzpicture}
\foreach \a in {0,...,4}
  \draw[thick] (0,\a) -- ++(7.5,0);
\draw[thick] (1,1) -- (1,2);
\filldraw (1,1) circle (3pt);
\filldraw (1,2) circle (3pt);
\draw[thick] (2,0) -- (2,1);
\filldraw (2,0) circle (3pt);
\filldraw (2,1) circle (3pt);
\draw[thick] (3,1) -- (3,2);
\filldraw (3,1) circle (3pt);
\filldraw (3,2) circle (3pt);
\draw[thick] (1,3) -- (1,4);
\filldraw (1,3) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (4,1) -- (4,4);
\filldraw (4,1) circle (3pt);
\filldraw (4,4) circle (3pt);
\draw[thick] (4.5,2) -- (4.5,3);
\filldraw (4.5,2) circle (3pt);
\filldraw (4.5,3) circle (3pt);
\draw[thick] (5.5,0) -- (5.5,2);
\filldraw (5.5,0) circle (3pt);
\filldraw (5.5,2) circle (3pt);
\draw[thick] (6.5,1) -- (6.5,2);
\filldraw (6.5,1) circle (3pt);
\filldraw (6.5,2) circle (3pt);
\draw[thick] (5.5,3) -- (5.5,4);
\filldraw (5.5,3) circle (3pt);
\filldraw (5.5,4) circle (3pt);
\end{tikzpicture}

*Acknowledgments*
I want to thank Andres Löh for introducing me to Typed Template Haskell,
and Matthew Pickering for working on it in GHC.
