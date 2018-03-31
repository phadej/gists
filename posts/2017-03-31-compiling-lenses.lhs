---
title: Compiling lenses
author: Oleg Grenrus
tags: lens
---

To make my optics library for JavaScript &ndash;
[`optika`](https://github.com/phadej/optika),
faster, I need to avoid allocations. 
In Haskell we can rely on GHC to optimize code for us, fusing e.g. tuple creation in `lens`;
in JavaScript we have to do it ourselves.
We can prototype the solution in Haskell, as it's one of the best imperative languages.

This is the third post about lenses in this month,
where previous are [Affine Traversal](./2017-03-20-affine-traversal.html)
and [Why there is no AGetter?](./2017-03-13-why-there-is-no-agetter.html).
And once again this is a literate Haskell file, so there are some imports:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module CompilingLenses where
import Control.Applicative (Const (..))
import Control.Arrow ((&&&))
import Control.Monad.ST
import Data.Foldable (for_)
import Data.Monoid (Sum (..))
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.STRef
import Data.Tuple (swap)
```

Introduction
------------

[`optika`](https://github.com/phadej/optika) is a profunctor optics library,
so let's remind ourselves how profunctor optics work.

<div class="text-center">
<img title="optics diagram" src="../images/profunctor-optics.svg" />
</div>

The profunctor optic is a profunctor transformer: it takes "smaller" `p a b`, and
turns it into "bigger" `p s t`.

```haskell
type Optic p s t a b = p a b -> p s t
```

Usually `p a b` is a function. For example a `Forget` profunctor
used to defined `view` and `foldOf` operations is a `newtype Forget r a b =
Forget (a -> r)`.  The ordinary arrow `(->)` is also a profunctor, and used to
define `over` and `set` operations.  Also `Tagged a b` is a profunctor (if you
stare at it long enough), and is isomorphic to `() -> b` function. It's used to
implement `review`.

It's not easy to build intuition on how this works. For example, `Getter` is
essentially a function `s -> a`, but from above we get something like `s ~> t`.
The idea is that the curly arrow &ndash; the profunctor, is not exactly the function `s -> t`,
but may but something different. In `Getter` case, it's `a ~> b = a -> r`
for some `r` (`Forget r a b`).  As we start with `a ~> b`, we pick `r` to be `a`, i.e. `a ~> b =
a -> a`.  When the optic transforms `a ~> b` into `s ~> t`, it transforms
"small" getter `a -> a` into "big" `s -> a`!

We'll define a non-function profunctor to implement `sumOf`. It will be a *deep
embedding* of folds. Note, that we cannot perform arbitrary transformations
while creating lenses or traversals. For example `Lens` is:
```haskell
type Lens s t a b = forall p. Strong p => p a b -> p s t
```
so we can only `dimap` (from `Profunctor`) and `first'` (from `Strong`) the
arbitrary profunctor. We can create a GADT where these operations are
represented explicitly; it will sacrifice a bit of elegance (no more functions
everywhere) for the sake of performance.

We'll define a small example data type and a value; to make
following development more concrete. We'll show how `sumOf` is implemented
using `Forget r a b`. After that we'll define a deep-embedding Fold-AST: `Neglect`,
implement `neglectSumOf` using it; and discuss how it's useful for the JavaScript
implementation. At the end, we'll inspect benchmarks of various implementations
of `sumOf` in JavaScript: `optika`'s old and new, as well as implementations
from other optics libraries.

Example
-------

To make the development more concrete, Let's define two record types, where the
second one is used in the first one.

```haskell
data Foo = Foo { _fooX :: Bool, _fooYS :: [Bar] } deriving Show
data Bar = Bar { _barX :: Bool, _barZ  :: Int }   deriving Show
```

The example value we'll work with is:

```haskell
foo :: Foo
foo = Foo True
    [ Bar True 3
    , Bar False 4
    , Bar False 5
    , Bar True 2
    ]
```

Next we'll need lenses to work with these types. First the `lens` helper,
defined a bit differently than in
[`purescript-profunctor-lenses`](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/2.3.0/docs/Data.Lens.Lens#v:lens)
```haskell
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = dimap (id &&& getter) (uncurry setter) . second'
```
and the actual lenses
```haskell
fooYS :: Lens Foo Foo [Bar] [Bar]
fooYS = lens _fooYS (\s b -> s { _fooYS = b })

barZ :: Lens Bar Bar Int Int
barZ = lens _barZ (\s b -> s { _barZ = b })
```

If we want to calculate the sum of `barZ` ints inside `Foo`, we'll need to fold:
```haskell
sumOf :: Optic (Forget (Sum a)) s s a a -> s -> a
sumOf l = getSum . runForget (l (Forget Sum))
```

And it works: `3 + 4 + 5 + 2 = 14`:
```
λ> sumOf (fooYS . traverse' . barZ) foo
14
```

Neglect
-------

The first step in a solution to the allocation problem is to not transform
functions (recall, `newtype Forget r a b = Forget (a -> r)`), but make a more
concrete AST. For the lack of the imagination, we'll call it `Neglect`.

```haskell
data Neglect r a b where
    End    ::                                   Neglect r r            x
    LMap   :: (c -> a) ->      Neglect r a b -> Neglect r c            x
    Second ::                  Neglect r a b -> Neglect r (c, a)       x
    Right' ::                  Neglect r a b -> Neglect r (Either c a) x 
    Wander :: Traversable f => Neglect r a b -> Neglect r (f a)        x
```

*Note*: We let the second argument change freely in the AST, this just highlights
that `b` type parameter is a phantom.

We'll need a function to inspect the structure of `Neglect`, and change the last type parameter
```haskell
debugNeglect :: Neglect r a b -> [String]
debugNeglect End        = ["End"]
debugNeglect (LMap _ x) = "LMap" : debugNeglect x
debugNeglect (Second x) = "Second" : debugNeglect x
debugNeglect (Right' x) = "Right'" : debugNeglect x
debugNeglect (Wander x) = "Wander" : debugNeglect x

retagNeglect :: Neglect r a b -> Neglect r a c
retagNeglect p = case p of 
    End      -> End
    LMap f x -> LMap f x
    Second x -> Second x
    Right' x -> Right' x
    Wander x -> Wander x
```

The `Neglect` is an instance of `Traversing` and all of the super-classes,
at least with a lousy interpretation of the laws:
```haskell
instance Profunctor (Neglect r) where
    lmap = LMap
    rmap _ = retagNeglect

instance Strong (Neglect r) where
    second' = Second

instance Choice (Neglect r) where
    right' = Right'

instance Traversing (Neglect r) where
    traverse' = Wander
```

The set-up is ready, and we can write imperative variant of `sumOf`.
We allocate a mutable cell where we accumulate the result. The helper `go`,

```haskell
neglectSumOf :: Num a => Optic (Neglect a) s s a a -> s -> a
neglectSumOf l s = runST $ do
    ref <- newSTRef 0
    go ref (l End) s
    readSTRef ref
  where
    go :: Num r => STRef s r -> Neglect r a b -> a -> ST s ()
    go  ref End        x         = modifySTRef' ref (x +)
    go _ref (Right' _) (Left _)  = pure ()
    go  ref (Right' p) (Right x) = go ref p x
    go  ref (LMap f p) x         = go ref p (f x)
    go  ref (Second p) (_, x)    = go ref p x
    go  ref (Wander p) xs        = for_ xs $ go ref p
```

And this variant works too!
```
λ> neglectSumOf (fooYS . traverse' . barZ) foo
14
```

Next step is to manually fuse `LMap` and `Second`. As you can see they 
almost always come together, because of the way `lens` is defined.
```
λ> debugNeglect ((fooYS . traverse' . barZ) End)
["LMap","Second","Wander","LMap","Second","End"]
```

As in optics library setting the classes are implementation details,
let's add a method to `Strong` type-class:
```haskell
class Profunctor p => StrongE p where
    firstE :: p a b  -> p (a, c) (b, c)
    firstE = dimap swap swap . secondE

    secondE :: p a b -> p (c, a) (c, b)
    secondE = dimap swap swap . firstE

    lensE :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
    lensE getter setter = dimap (id &&& getter) (uncurry setter) . secondE

    {-# MINIMAL firstE | secondE #-}
```

Now we can redefine the optics to use the extended class
```haskell
type LensE s t a b = forall p. StrongE p => p a b -> p s t

fooYSE :: LensE Foo Foo [Bar] [Bar]
fooYSE = lensE _fooYS (\s b -> s { _fooYS = b })

barZE :: LensE Bar Bar Int Int
barZE = lensE _barZ (\s b -> s { _barZ = b })
```

`Forget` is trivially instance of `StrongE`:
```haskell
instance StrongE (Forget r) where
    firstE = first'
    secondE = second'
```
but with `Neglect` we can make a slight optimization:
```haskell
instance StrongE (Neglect r) where
    firstE = first'
    secondE = second'
    lensE getter _ = LMap getter -- no tuple creation!
```

With those definition `sumOf` and `neglectSumOf` are still working:
```
λ> sumOf (fooYSE . traverse' . barZE) foo
14
λ> neglectSumOf (fooYSE . traverse' . barZE) foo
14
```
but in the latter case there are less commands to interpret
```
λ> debugNeglect ((fooYSE . traverse' . barZE) End)
["LMap","Wander","LMap","End"]
```

In `optika` and *JavaScript* in general, the `key` (accessing field of an
object) and `idx` optics are needed often, so we can add special cases for them
to `StrongE` class.
(`idx` is  an affine traversal, where `key` can be thought as `Lens' Record (Maybe Field)`.)
There we'll end with a optic description like:
```
["fooYS", ["wander"], "barZ"]
```
where "wander" in array representing the `Wander` operation, and non-wrapped
string a `Key` command. 
Note, we got a long way to represent an argument to functions like `updateIn`
of [immutable.js](https://facebook.github.io/immutable-js/).
If there are no wandering or prisms, we could use it directly to execute
`over`! Otherwise, we can write interpreter which would something along
```javascript
function (s) {
    let acc = 0;
    const s0 = s["fooYS"];      // "fooYS"
    for (const s1 of s0) {      // ["wander"]
        const s2 = s1["barZ"];  // "barZ"
        acc += s2;              // end
    }
    return acc;
}
```
Unfortunately there are no quasiquoter in JavaScript, so we cannot `eval` such
code, and plug-in the getter functions which aren't keys or indices.  This is
something where Lisps shine, as there we could. Yet, interpreting the commands
directly seems to be fast enough anyway, especially with small data-sets,
where different constant factors matter.

Prism
-----

For completeness, let's also consider Prisms. So far the only prism
I needed in JavaScript land is `filtering`.

```haskell
type Prism s t a b = forall p. Choice p => p a b -> p s t

filtering :: (a -> Bool) -> Prism a a a a
filtering p = dimap getter (either id id) . right' 
  where
    getter x | p x       = Right x
             | otherwise = Left x
```

This doesn't cause any problems for our current definition:
```
λ> sumOf (fooYSE . traverse' . filtering _barX . barZ) foo
5
λ> neglectSumOf (fooYSE . traverse' . filtering _barX . barZ) foo
5
```
Yet, we can notice a similar pattern of commands in `Neglect`:
`Right'` is followed by `LMap`, because of the way `filtering` is defined.
```
*Main> debugNeglect ((fooYSE . traverse' . filtering _barX . barZ) End)
["LMap","Wander","LMap","Right'","LMap","Second","End"]
```
We can add `filtering` (and `prism`) methods to our variants of `Choice`, so
the `Neglect` program would be smaller (but using more different commands).
This reminds me of *RISC vs CISC* comparison!

Benchmarks
----------

There is no reason to benchmark Haskell version, as it exists only as a proof-of-concept version.
The [JavaScript version (WIP at the moment of writing)](https://github.com/phadej/optika/pull/8) is easy to benchmark,
thanks to the existing benchmark suite in [`partial.lenses`](https://github.com/calmm-js/partial.lenses).

```
   4,797,435/s     208.445ns     1.00x   L.sum(L.elems, xs100)
     866,526/s       1.154μs     5.54x   K.traversed().cheatSumOf(xs100)
     812,924/s       1.230μs     5.90x   K.traversed().compileSumOf(xs100)
     742,065/s       1.348μs     6.46x   K.traversed().sumOf(xs100)
     612,765/s       1.632μs     7.83x   L.concat(Sum, L.elems, xs100)
     582,574/s       1.717μs     8.23x   K.traversed().forgetSumOf(xs100)
     177,087/s       5.647μs    27.09x   xs100.reduce((a, b) => a + b, 0)
     163,278/s       6.125μs    29.38x   R.sum(xs100)
      23,082/s      43.324μs   207.84x   P.sumOf(P.traversed, xs100)
       3,597/s     278.027μs  1333.81x   O.Fold.sumOf(O.Traversal.traversed, xs100)

     181,058/s       5.523μs     1.00x   K.traversed().traversed().traversed().compileSumOf(xsss100)
     168,839/s       5.923μs     1.07x   L.sum([L.elems, L.elems, L.elems], xsss100)
     158,853/s       6.295μs     1.14x   L.concat(Sum, [L.elems, L.elems, L.elems], xsss100)
     133,148/s       7.510μs     1.36x   k_traversed3.sumOf(xsss100)
      97,361/s      10.271μs     1.86x   k_traversed3.forgetSumOf(xsss100)
      75,170/s      13.303μs     2.41x   xsss100.reduce((a0, xss) => a0 + xss.reduce((a1, xs) => a1 + xs.reduce((a2, x) => a2 + x, 0), 0), 0)
      74,948/s      13.343μs     2.42x   K.traversed().traversed().traversed().sumOf(xsss100)
      65,481/s      15.272μs     2.77x   K.traversed().traversed().traversed().forgetSumOf(xsss100)
      14,389/s      69.496μs    12.58x   k_traversed3.cheatSumOf(xsss100)
      10,196/s      98.080μs    17.76x   K.traversed().traversed().traversed().cheatSumOf(xsss100)
       4,159/s     240.416μs    43.53x   P.sumOf(R.compose(P.traversed, P.traversed, P.traversed), xsss100)
         682/s       1.466ms   265.48x   O.Fold.sumOf(R.compose(O.Traversal.traversed, O.Traversal.traversed, O.Traversal.traversed), xsss100)
```

- `K` with `forgetSumOf` is old `optika` version
- `K` with `sumOf` is "new" `optika` version
- `L` is [`partial.lenses`](https://github.com/calmm-js/partial.lenses)
- `P` is [`ramda-lens`](https://github.com/ramda/ramda-lens)
- `O` is [`flunc/optics`](https://github.com/flunc/optics)

And other constants are:
```javascript
const xs100 = Array(100).fill(1);
const xs10 = Array(10).fill(1);
const xsss100 = Array(100).fill([[1]]);
const xsss100b = Array(100).fill([xs10]);
const xsss100c = Array(10).fill([xs100]);

const k_traversed3 = K.traversed().traversed().traversed();
```

I expected the new version (`K...sumOf`) to be much faster than older one (`K...forgetSumOf`).
We are still slower than `partial.lenses` in the first benchmark (array of 100 numbers), and
about the same in the second one (array of 100 of `[[1]]`, i.e. 100-1-1 3-layer-array).

One reason is due more expensive compilation. `partial.lenses` doesn't have compilation step,
but in `optika` each operation is in two-step: *compilation* and *execution*
The first one is where GHC helps you in Haskell land, if you don't use dynamic lenses.
The `K...compileSumOf` measures the time spend "compiling" the lens in `optika`.
```javascript
Optic.prototype.compileSumOf = function (/* unused */) {
  var commands = this.run(profunctor.dictNeglectSum, []).reverse();
  return commands.length; // incorrect!
}
```
I.e. we construct the commands array, but do not *execute* it.

Two more benchmarks are using `xsss100b` and `xsss100c`, 100-1-10 and 10-1-100 elements 3-layer-arrays:
```
      59,728/s      16.743μs     1.00x   k_traversed3.forgetSumOf(xsss100b)
      53,630/s      18.646μs     1.11x   k_traversed3.sumOf(xsss100b)
      51,232/s      19.519μs     1.17x   L.sum([L.elems, L.elems, L.elems], xsss100b)
      48,596/s      20.578μs     1.23x   K.traversed().traversed().traversed().forgetSumOf(xsss100b)
      47,026/s      21.265μs     1.27x   L.concat(Sum, [L.elems, L.elems, L.elems], xsss100b)
      41,254/s      24.240μs     1.45x   K.traversed().traversed().traversed().sumOf(xsss100b)
      12,820/s      78.004μs     4.66x   xsss100b.reduce((a0, xss) => a0 + xss.reduce((a1, xs) => a1 + xs.reduce((a2, x) => a2 + x, 0), 0), 0)
      12,530/s      79.810μs     4.77x   k_traversed3.cheatSumOf(xsss100b)
      11,303/s      88.472μs     5.28x   K.traversed().traversed().traversed().cheatSumOf(xsss100b)
       1,616/s     618.755μs    36.96x   P.sumOf(R.compose(P.traversed, P.traversed, P.traversed), xsss100b)
         242/s       4.129ms   246.60x   O.Fold.sumOf(R.compose(O.Traversal.traversed, O.Traversal.traversed, O.Traversal.traversed), xsss100b)

     103,170/s       9.693μs     1.00x   k_traversed3.forgetSumOf(xsss100c)
      87,922/s      11.374μs     1.17x   K.traversed().traversed().traversed().forgetSumOf(xsss100c)
      79,017/s      12.655μs     1.31x   k_traversed3.sumOf(xsss100c)
      67,469/s      14.822μs     1.53x   L.sum([L.elems, L.elems, L.elems], xsss100c)
      61,644/s      16.222μs     1.67x   L.concat(Sum, [L.elems, L.elems, L.elems], xsss100c)
      57,878/s      17.278μs     1.78x   K.traversed().traversed().traversed().sumOf(xsss100c)
      54,430/s      18.372μs     1.90x   k_traversed3.cheatSumOf(xsss100c)
      39,805/s      25.123μs     2.59x   K.traversed().traversed().traversed().cheatSumOf(xsss100c)
      15,588/s      64.151μs     6.62x   xsss100c.reduce((a0, xss) => a0 + xss.reduce((a1, xs) => a1 + xs.reduce((a2, x) => a2 + x, 0), 0), 0)
       2,429/s     411.670μs    42.47x   P.sumOf(R.compose(P.traversed, P.traversed, P.traversed), xsss100c)
         343/s       2.917ms   300.95x   O.Fold.sumOf(R.compose(O.Traversal.traversed, O.Traversal.traversed, O.Traversal.traversed), xsss100c)
```

With slightly bigger inputs, the difference between `optika` and `partial.lenses` diminishes.
Looks like we go as fast as is reasonably possible with JavaScript.

One of [performance design choices of `partial-lenses`](https://github.com/calmm-js/partial.lenses#performance) is
<blockquote>
Avoid optimizations that require large amounts of code
</blockquote>
It's worth mentioning, that one could count current `sumOf` of optika as *large amounts of code*,
the approach generalizes to every fold, hopefully without noticeable performance degradation.

The forth `optika` variant &ndash; `K...cheatSumOf` is cheating, as a name says, and compiles
the `optika` lens to `partial.lenses` and runs it!  This is the benefit of more
concrete, `Neglect` based profunctor.  Not all lenses can be converted, but
some could. This let us compare the compilation overhead in one more way.

Another trick used in these benchmarks is `k_traversed3`, a lens
created outside of the benchmark suite. As the lens object is now persistent,
we can use it to cache compiled commands:
```javascript
Optic.prototype.sumOf = function (v) {
  var commands;
  if (!this.commandsSumOf) {
    commands = this.run(profunctor.dictNeglectSum, []).reverse();
    this.commandsSumOf = commands;
  } else {
    commands = this.commandsSumOf;
  }

  // ...
}
```
This is questionable, even *dirty*, *hack*, but it doesn't cause
memory-leaks. Each lens is now more memory heavy, but not significantly.

Conclusion
----------

Mapping Haskell ideas to untyped and unoptimised language requires making
not-so idiomatic choices; but Haskell can still help there.  As `optika` is
based on solid ideas of profunctor optics, adding new way to `sumOf` was
relatively small and isolated patch.  We have seen an example how to convert
optics them to other library formats (one may try to convert them to and from
[`purescript-profunctor-lenses`](https://github.com/purescript-contrib/purescript-profunctor-lenses)!)
These ideas are hopefully useful to someone who decides to make (yet another)
optimized Clojure optics library, macros would let you do even cooler stuff.

---

This instance is not yet in any released version of `profunctors`, but
[it will be](https://github.com/ekmett/profunctors/commit/a424ee6d08247218d91a1dab930d2f2228578c59)
thanks to [Phil Freeman](https://twitter.com/paf31).

```haskell ignore
instance Monoid m => Traversing (Forget m) where
    traverse' (Forget h) = Forget (foldMap h)
    wander f (Forget h) = Forget (getConst . f (Const . h))
```

---

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
λ> :l compiling-lenses.lhs
```
fetch the source from
[]()
