---
title: "overloaded-0.2.1: Overloaded:Categories"
author: Oleg Grenrus
---

The `Overloaded:Categories` is another of the new features of 
recent [overloaded 0.2.1](https://hackage.haskell.org/package/overloaded-0.2.1)
release. I wrote about 
[`Overloaded:Do`](https://oleg.fi/gists/posts/2020-04-27-overloaded-local-do.html),
last week
`overloaded` package uses source plugins to reinterpret syntax in different ways.

Alexis King (lexi-lambda) is working on various Arrows issues,
including
[Constrained based arrow notation -ghc-proposal](https://github.com/lexi-lambda/ghc-proposals/blob/constraint-based-arrow-notation/proposals/0000-constraint-based-arrow-notation.md).
`Overloaded:Categories` is however orthogonal.
We are not considered as much with *how* arrow notation is desugared,
but rather into what.

My claim is that `Arrow`-classes are maybe not what we want to desugar into.
Samuel Gélineau (gelisam) works on Template Haskell library
[`category-syntax`](https://github.com/gelisam/category-syntax)
to desugar `do` notation into category classes.
`Overloaded:Categories` is different as it desugars arrow notation
using source plugin functionality.

We'll discuss `Arrow` class and its alternative `CartesianCategory`,
how we can desugar into latter, and see few examples of 
`Overloaded:Categories` where GHC `Arrows` are not applicable.

This work and especially AD example are inspired by
Conal Elliott [Compiling to categories](http://conal.net/papers/compiling-to-categories/).

<div id="toc"></div>

Arrow type class
----------------

Let us revisit `Arrow` type-class. The most minimal definition is

```haskell
class Category a => Arrow a where
    arr :: (b -> c) -> a b c

    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
```

The first problem is the `arr` combinator. The arrow notation desugaring
doesn't need as powerful combinator. `arr` is used to do data-plumbing,
rearranging tuple fields, in other words:

```haskell
    proj1 :: a (b,c) b
    proj2 :: a (b,c) c

    (&&&) :: a b c -> a b c' -> a b (c,c')
    -- or
    dup   :: a b c -> a b (c,c)
```

would be enough! 

We *can* rather desugar into category theory inspired class

```haskell
class Category a => CartesianCategory a where
    proj1 :: a (b, c) b
    proj2 :: a (b, c) c
    (&&&) :: a b c -> a b c' -> a b (c, c')
```

A somewhat natural generalization is to not require **product**
object to be a pair. This allows `CartesianCategory` to be **polykinded**.

```haskell
class Category a => CartesianCategory (a :: k -> k -> Type) where
    type Product a :: k -> k -> k

    proj1 :: a (Product a b c) b
    proj2 :: a (Product a b c) c
    (&&&) :: a b c -> a b c' -> a b (Product a c c')
             -- ^ called 'fanout' in the Overloaded.Categories
```

`Arrow` is more powerful than `CartesianCategory`.
We can write `CartesianCategory` instance for `WrappedArrow`:

```haskell
newtype WrappedArrow arr b c = WrapArrow (arr b c)

instance A.Arrow arr => CartesianCategory (WrappedArrow arr) where
    type Product (WrappedArrow arr) = (,)
    proj1 = WrapArrow (A.arr fst) -- note, here we do
    proj2 = WrapArrow (A.arr snd) -- just plumbing
    WrapArrow f &&& WrapArrow g = WrapArrow (f A.&&& g)
```

but cannot do the opposite: there is no way to write `arr` using
`CartesianCategory` methods.

Arrow-notation desugaring
-------------------------

A simple example of arrow notation in use could be:

```haskell
assoc :: arr ((x,y),z) (x,(y,z))
assoc = proc (xy, z) -> do
    (x,  y) <- id -< xy
    yz      <- id -< (y, z)
    id -< (x, yz)
```

Note: one doesn't need `returnA = arr id`, the `id` is *better*.

How we could desugar the above statements into `CartesianCategory`
combinators? We'll use similar (the same?) approach as Arrow are desugared.
Each statement of the form

```haskell
out <- morphism -< in
```

*takes* `in` some bindings, and *adds* new `out` ones.
The beginning `proc vars -> ...` produces initial bindings
and the final `combinator -< ...` assembles final "value".

We can write what variables are in scope after each statement:

```haskell
assoc :: arr ((x,y),z) (x,(y,z))
assoc = proc (xy, z) -> do
    -- xy, z
    (x,  y) <- id -< xy
    -- xy, z, x, y
    yz      <- id -< (y, z)
    -- xy, z, x, y, yz
    id -< (x, yz)
```

The `Overloaded:Categories` desugared is very simple,
as it essentially maintains a context of `a input variable` values.
To construct tuples it uses `&&&` to tuple-them up,
and when there is tuple-pattern match, `overloaded` applies
`proj1` and `proj2`.

<h3>Desugaring example: first step</h3>

For specific example, let us zoom into

```haskell
    -- xy, z
    (x,  y) <- id -< xy
    -- xy, z, x, y
```

step. Our goal is to construct

\begin{tikzcd}
XY \times Z \arrow[r] & XY \times Z \times X \times Y
\end{tikzcd}

diagram. As we have `xy` and `z` variables in scope, we have
$\texttt{xy} : XY \times Z \to XY$ and $\texttt{z} : XY \times Z \to Z$ arrows.
We don't construct tuples in this step, but
deconstruct tuple
so we will use `proj1` ($\pi_1$) and `proj2` ($\pi_2$).
To carry the existing bindings over we use `id &&& f` (`second`, $\langle 1 , f\rangle$)
We can draw a diagram of what happens:

\begin{tikzcd}
                                                      &                             & XY \times Z \arrow[rd] \\
XY \times Z \arrow[dr, "\texttt{xy}"] \arrow[urr, "1"] &     & & (XY \times Z) \times (X \times Y) \\
                                                      & XY \arrow[r, "\texttt{id}"] & X \times Y \arrow[ru]
\end{tikzcd}

The context is transformed to be

```haskell
xy = xy . proj1
z  = z  . proj1
x  = proj1 . proj2
y  = proj2 . proj2
```

using `id &&& id . xy`.

<h3>Desugaring example: second step</h3>

The next statement tuples up variables:

```haskell
    -- xy, z, x, y
    yz      <- id -< (y, z)
    -- xy, z, x, y, yz
```

\begin{tikzcd}
&& (XY \times Z) \times (X \times Y) \arrow[rd] \\
(XY \times Z) \times (X \times Y)
  \arrow[urr, "1"]
  \arrow[r, "\pi_2 \circ \pi_2"]
  \arrow[rd, "\pi_2 \circ \pi_1"]
&Y \arrow[r] & YZ \arrow[r] &
((XY \times Z) \times (X \times Y)) \times YZ
\\
&Z \arrow[ur]
\end{tikzcd}

The context is transformed to be

```haskell
xy = xy . proj1 . proj1
z  = z  . proj1 . proj1
x  = proj1 . proj2 . proj1
y  = proj2 . proj2 . proj1
yz = proj2
```

by `id &&& ((proj2 . proj2) &&& (proj2 . proj1))`.

And the process continues similarly.

<h3>Observations about desugaring</h3>

- The actual arrow notation desugaring similarly have
  rules for each statements with input and output contexts.

- repetitive fan outs and projections are often redundant,
  `overloaded` applies simplifications like `proj1 . (f &&& g)` into `f`,
  but not all possible to avoid losing possible sharing.
  The "optimizer" is far from smart.

- We could be smarter and count which sharing are actually used
  afterwards, and only carry them over.
  In the example above `xy` is used once, so it doesn't need to be carried
  over. As far as I understand GHC desugaring does that.

  Ultimately, we could desugar using *commutative monoidal category*
  combinators (In the example above, only `assoc` is needed).

  ```haskell
  assoc :: a ((b,c),d) (b,(c,d))
  swap  :: a (b, c) (c, b)
  ```

  But this is not yet supported in `overloaded`, as all example
  categories I wanted to work with were Cartesian.

Example: STLC
-------------

The go to example for GADTs is well-typed simple typed lambda calculus
terms:

```haskell
-- types
data Ty
    = TyPair Ty Ty
    | TyFun Ty Ty
    | TyNat

-- indexing into context
data Elem :: [Ty] -> Ty -> Type where
    Here  :: Elem (x ': xs) x
    There :: Elem xs x -> Elem (y ': xs) x

-- terms
data Term :: [Ty] -> Ty -> Type where
    Var :: Elem ctx ty -> Term ctx ty

    Lam :: Term (a ': ctx) b -> Term ctx ('TyFun a b)
    App :: Term ctx ('TyFun a b) -> Term ctx a -> Term ctx b

    Fst :: Term ctx ('TyPair a b) -> Term ctx a
    Snd :: Term ctx ('TyPair a b) -> Term ctx b
    Pair :: Term ctx a -> Term ctx b -> Term ctx ('TyPair a b)

    Nat :: Natural -> Term ctx 'TyNat
```

The `Elem` value in `Var` indexes into context.
The term values are thus full of `Here`, `There Here` etc
de Bruijn indices. These are not fun to write.

We can define a helper type:

```haskell
newtype Mapping (ctx :: [Ty]) (a :: Ty) (b :: Ty) =
    M (Term ctx ('TyFun a b))
```

which is not only `Category`, but also `CartesianCategory`:

```haskell
instance CartesianCategory (Mapping ctx) where
    type Product (Mapping ctx) = 'TyPair

    proj1 = M $ Lam $ Fst var0
    proj2 = M $ Lam $ Snd var0
    fanout (M f) (M g) = M $ Lam $ Pair
        (app (weakenTerm f) var0)
        (app (weakenTerm g) var0)
```

The `assoc` value we defined above, with the help
of `Overloaded:Categories`, has the type
(and can be redefined more concisely):

```haskell
assoc
    :: CartesianCategory cat
    => cat (Product cat (Product cat a b) c)
           (Product cat a (Product cat b c))
assoc = proc ((x, y), z) -> identity -< (x, (y, z))
```

which when specialized to `Mapping` produces term (after few reductions):

```haskell
Lam (Pair (Fst (Fst (Var Here)))
          (Pair (Snd (Fst (Var Here)))
                (Snd (Var Here))))
```

The arrow-notation is not as concise as Haskell expression
`\((x,y),z) -> (x,(y,z))`, but is nicer than counting de Bruijn indices
by hand.

Example: Automatic differentiation and machine learning
-------------------------------------------------------

Another example I have worked out is machinery which allows to
write automatic diffentiation programs.
Like the small xor neural network below:

\begin{tikzcd}[row sep=tiny]
X \arrow[r] \arrow[rdd] & U \arrow[rd]       \\
                        &               & Z  \\
Y \arrow[r] \arrow[ruu] & V \arrow[ru]       \\
\end{tikzcd}

```haskell
type Input   = Leaf2
type NeuronW = 'Plus ('Leaf N.Nat2) Leaf1
type Weights = 'Plus ('Plus NeuronW NeuronW) NeuronW

type WeightsN = Eval Weights

-- single neuron with two inputs
neuron :: AD ('Plus Input NeuronW) Leaf1
neuron = proc (i, (ws, bias)) -> do
    o <- dot -< (ws, i)
    tanhAD . plus -< (o, bias)

-- the 2-2-1 network
network :: AD ('Plus Input Weights) Leaf1
network = proc (xy, ((w1, w2), w3)) -> do
    u  <- neuron -< (xy, w1)
    v  <- neuron -< (xy, w2)
    uv <- rewrap -< (u, v)
    neuron -< (uv, w3)

-- error for xor.
-- This can be more "dynamic", taking values in.
-- We apply gradient descent to minimise this.
networkError :: AD Weights Leaf1
networkError = proc ws -> do
    -- xor!
    s1 <- ex (1 ::: 1 ::: VNil) 0 -< ws
    s2 <- ex (0 ::: 0 ::: VNil) 0 -< ws
    s3 <- ex (1 ::: 0 ::: VNil) 1 -< ws
    s4 <- ex (0 ::: 1 ::: VNil) 1 -< ws

    sumAD -< ((s1,s2), (s3, s4))
  where
    ex :: V N.Nat2 -> S -> AD Weights Leaf1
    ex i e = proc ws -> do
         i1 <- konst i               -< ()
         e1 <- konst (V.singleton e) -< ()
         a1 <- network               -< (i1, ws)
         r1 <- minus                 -< (e1, a1)
         dot -< (r1, r1)
```

Luckily GHC Haskell has (Typed) Template Haskell, so the generated code
is quite fast: training this toy-xor-neural network takes a millisecond
and evaluation 0.000s.

Conclusion
----------

It would be great if `Arrows` desugared into `CartesianCategory`
and other (basic) category theory inspired type-classes.
However, I haven't told all `arr` is not used not only
to do tuple mangling but also to mangle records into tuples and back
and sum-types into `Either`s (for `ArrowChoice` combinators).
With `Overloaded:Categories` we are limited to whatever
`Product` or `Coproduct` types are defined.
And what may be confusing, the syntax is always tuples and `Left`/`Right`
even the product and coproduct are something else. For example
Cartesian product i.e. pair is not only product but also a coproduct
in $\mathbf{Vect}$ - category of vector spaces and linear maps.

The usage of `Overloaded:Categories` with `proc` could also be made nicer
with heterogeneous lists, so one doesn't need to nest pairs, but use
n-tuples. It can be done, but I just had to stop somewhere.

For `Overloaded:Categories` to cover all of `Arrows` use cases, we'll need a
type-class with `arr`. In my opinion `arr` shouldn't be used for the bulk of
desugaring, and shouldn't be in the root class of the hierarchy, but rather in
a separate branch.

Similar situation is with having `fromInteger` in `Num`, the
value used for desugaring of integer literals is conflated with
kind-of class for ring structures (but with `abs` and `signum`),
thus if you don't have ring structure you don't use literals.
And if you cannot implement `arr` you don't use arrow-notation.
`overloaded` offers alternative designs.
