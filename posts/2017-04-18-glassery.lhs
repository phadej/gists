---
title: Glassery
author: Oleg Grenrus
tags: lens
---

After I have
[improved the raw performance](./2017-03-31-compiling-lenses.html)
of [`optika`](https://github.com/phadej/optika) &ndash; a JavaScript optics library,
it's time to make the library (feature-)complete and sound.
Gathering and classifying all possible optic types, gives us a reference point
to guide the implementation.
In this post I systematically introduce various optic types,
using programming language Haskell.
In fact, this literal Haskell file could be turned into a library with some work.
The primary goal of this post is to clarify my own thoughts; but I hope
it may be useful for others too.

As some of the previous posts (
[Compiling lenses](./2017-03-31-compiling-lenses.html),
[Affine Traversal](./2017-03-20-affine-traversal.html),
[Why there is no AGetter?](./2017-03-13-why-there-is-no-agetter.html)
), this is a literate Haskell file, but this time we don't depend on `profunctors` (but as it seems, on everything else).

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Glassery where
import Control.Monad.Trans.State (State, evalState, state)
import Data.Distributive (Distributive (..), cotraverse)
import Data.Foldable (traverse_)
import Data.Functor.Apply (Apply (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep hiding (tabulated)
import Data.Monoid (Endo (..))
import Data.Pointed (Pointed (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.Semigroup.Foldable (Foldable1 (..), traverse1_)
import Data.Semigroup.Traversable (Traversable1 (..))
import Data.Tuple (swap)
import GHC.Proof ((===), (=/=))
import Linear (V2 (..))
```

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)

Introduction
------------

Profunctor formulation of optics is elegant:
```
type Optic c s t a b = forall p. c p => p a b -> p s t
```
Depending of the constraint `c`, we get different kind of optics, e.g. with `c
= Strong` we get a `Lens`. There are not many type classes for
`* -> * -> *` kinded types: `Profunctor`, `Strong`, `Choice`, `Bifunctor` and few
less known ones: `Traversing1`, `Traversing`, `Mapping` and `Bicontravariant` (and `Closed`).
The subset lattice of class sets exposes an optics hierarchy:

<div class="text-center">
<img title="optics diagram" src="../images/optics-hierarchy.svg" />
</div>

Each color represents different class added into the mix. `Iso` is restricted only by `Profunctor`;
imposing additional `Strong` (<span style="color:#000080">blue</span>) constraint we get `Lens`;
adding `Traversing1` (<span style="color:#008000">green</span>) turns a `Lens` into a `Traversal1`.
Other colors are for `Choice` (<span style="color:#800000">red</span>),
`Bicontravariant` (<span style="color:#ff8000">orange</span>),
`Bifunctor` (<span style="color:#8000ff">purple</span>), and
`Mapping` (<span style="color:#808080">gray</span>). There is no color
for `Traversing` as it's (almost) the combination of `Traversing1` and `Choice`.

If the implementation is concrete-representation based, this graph
is an inheritance graph of optic classes (good example of multiple inheritance!).
With the van Laarhoven encoding of lenses, you get the same hierarchy;
but it's not as easy to see, as the `type Optic` is more complicated.
See [the documentation for `Control.Lens.Type` module](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Type.html).

The `Strong` part of the graph can also be indexed: `IndexedLens`,
`IndexedTraversal` etc. Indexed optics provide also the index of a smaller value
inside the bigger one.

Major part of the content of this post is based on the haddock documentation of Edward Kmett's
[`lens`](http://hackage.haskell.org/package/lens) and
[`profunctors`](http://hackage.haskell.org/package/profunctors) packages.
The ["Profunctor Optics: Modular Data Accessors"](https://arxiv.org/abs/1703.10857)
by Matthew Pickering et al is also one reference, even it doesn't discuss
all the possible (known) optics.
The [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) has influenced
the format.

*Edit:* There are older posts about profunctor optics:
[in r6research: Mainline Profunctor Heirarchy for Optics](http://r6research.livejournal.com/27476.html)
and [bennofs' lpaste](http://lpaste.net/103359).

Contents
--------

We can summarize the contents of this post as a table (of contents).
For each optic type there is a constructor and characterizing operations (analogous to
introduction and elimination rules in logic!), as well as closely related type classes and profunctors.
Some operations occur more than once in the table.
This is because I try to make table complete, for example lens-operations
`view` and `set` are operations of `Lens` sub-classes: `Getter` and `Setter`,
but they are enough to describe a `Lens` `l`:
```
lens (view l) (set l) ≡ l
      view (lens g s) ≡ g
       set (lens g s) ≡ s
```
Compare that to the local soundness and completeness of conjunction:
```
pair (fst p) (snd p) ≡ p  -- complete
      fst (pair x y) ≡ x  -- sound 1
      snd (pair x y) ≡ y  -- sound 2
```

| Optic | Constructor | Operations | Type class | Profunctor |
| --- | --- | --- | --- | --- |
| [Equality](#equality)                 | `id`, `simple`  |                      |                     | `Identical` |
| [Iso](#iso)                           | `iso`           | `view`, `review`     | `Profunctor`        |             |
| [Prism](#prism)                       | `prism`         | `previewE`, `review` | `Choice`            | `ForgetE`   |
| [Review](#review)                     | `upto`          | `review`             | `Bifunctor`         | `Tagged`    |
| [Getter](#getter)                     | `to`            | `view`               | `Bicontravariant`   |             |
| [Lens](#lens)                         | `lens`          | `view`, `set`        | `Strong`            |             |
| [Affine Traversal](#affine-traversal) | `affine`        | `previewE`, `set`    | `Choice`, `Strong`  |             |
| [Traversal1](#traversal1)             | `traversing1`   | `traverse1Of`        | `Traversing1`       | `Star`      |
| [Traversal](#traversal)               | `traversing`    | `traverseOf`         | `Traversing`        | `Star`      |
| [Affine Fold](#fold-fold1-and-affine-fold) | `afolding` | `preview`            |                     | `ForgetM`   |
| [Fold1](#fold-fold1-and-affine-fold)  | `folding1`      | `foldMap1Of`         |                     | `Forget`    |
| [Fold](#fold-fold1-and-affine-fold)   | `folding`       | `foldMapOf`          |                     | `Forget`    |
| [Setter](#setter)                     | `setting`       | `over`               | `Mapping`           | `(->)`      |

Each optic section follows the same internal structure:

- I define the optic using profunctor framework
- then informally describe it,
- state its laws,
- introduce related type class (if any),
- show how its constructor and operations can be defined
- state the completeness and soundness properties of them, and
- define a data type used to implement the operations.

After the bulk of the text, there are sections about [Indexed optics](#indexed-optics),
[concrete optics](#a-concrete-optic), [`re`-operation](#operation-re),
[Closed type class](#type-class-closed) and the optic it induces: `Grate`.

<div id="toc"></div>

Practical Optic
---------------

Though formulation presented in the introduction would work, it's not practical
in Haskell as we'd need to enable `UndecidableSuperClasses` to write

```
class CTop1 a
instance CTop1 a

class (f a, g a) => (f :/\: g) a
instance (f a, g a) => (f :/\: g) a
```

So instead we'll define different type alias:

```haskell
type Optic p s t a b = p a b -> p s t
```

and universally qualify over `p` in the particular optic type definition.
After this small technicality we can continue to the first concrete optic: `Equality`.

Equality
--------

The root of the optics hierarchy is an `Equality`.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-identity.svg" />
</div>

```haskell
type Equality s t a b = forall p. Optic p s t a b
```

It's a witness that both pairs of types: `(a ~ s)` and `(b ~ t)` are equal.
I "borrowed" the diagrams from the paper by [Matthew Pickering et al.](http://programming-journal.org/2017/1/7/).
They nicely illustrate what various optics do. In the `Equality` case we can go
back and forth between `s` and `a` as well as between `t` and `b`.

Types like `Equality` are used to witness equality in Haskell without GADTs ([`eq` package](http://hackage.haskell.org/package/eq)),
in PureScript [`purescript-leibniz`](https://pursuit.purescript.org/packages/purescript-leibniz/2.0.0/docs/Data.Leibniz#t:Leibniz),
or in Scala [`scalaz: Leibniz`](https://oss.sonatype.org/service/local/repositories/releases/archive/org/scalaz/scalaz_2.12/7.2.10/scalaz_2.12-7.2.10-javadoc.jar/!/scalaz/Leibniz.html).
[`PrimGetter`](#getter) and
[`PrimReview`](#review) have similar "two-in-one" property, as we will see in respective sections.

<h3 id="t:Identical">data type: Identical</h3>

The
[`:~:`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Type-Equality.html#t::-126-:)
type from [`Data.Type.Equality`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Type-Equality.html)
is another type for propositional equality, it arguably more directly encodes the equality:
```haskell
data a :~: b where
    Refl :: a :~: a -- or a ~ b => a :~: b
```
It however encodes only single equality. Encoding two at the same time isn't difficult
```haskell
data Identical a b s t where
    Identical :: Identical a b a b

```

We can convert freely between pair of `:~:` and `Equality`, using `Identical`.
In fact, the `id` is the only (non-bottom) constructor for `Equality`:
```haskell
toEquality :: a :~: s -> b :~: t -> Equality s t a b
toEquality Refl Refl = id

fromEquality :: Equality s t a b -> (a :~: s, b :~: t)
fromEquality l = case (l Identical) of
    Identical -> (Refl, Refl)
```

<h3 id="v:simple">constructor: simple</h3>

The `simple` is occasionally useful to constraint excessive polymorphism,
e.g turn `Optic` into simple `Optic'`.
```haskell
type Simple o s a = o s s a a
type Optic' p s a = Optic p s s a a -- Simple (Optic p) s a
type As a = Simple Equality a a

-- | @foo . (simple :: As Int) . bar@.
simple :: As a
simple = id
```


Iso
---

The relaxed version of equality is an *isomorphism*, `Iso`.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-iso.svg" />
</div>

```haskell
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
```

We restrict `Equality` so we can go only from `s` to `a` and from `b` to `t`.
In simple, monomorphic case we get a bijection between `s` and `a`. The type
system doesn't prevent us from encoding two arbitrary functions (which aren't
inverses of each other) as an Iso, but then that value won't be a lawful `Iso`.

<h3>laws</h3>

Since every `Iso` is both a valid `Lens` and a valid `Prism`
the laws for those types imply the following laws for an `Iso` `o`:

```
viewP o (reviewP o b) ≡ b
reviewP o (viewP o s) ≡ s
```

Or even more powerfully using `re`:

```
o . re o ≡ id
re o . o ≡ id
```

Intuitively `re` "rotates" the optic diagram 180 degrees, turning `Iso` `s t a b` into
`Iso` `b a t s`, `Review` `t b` into `Getter` `b t` and back.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-iso.svg" />
<img title="optics diagram" src="../images/optic-iso-flipped-1.svg" />
<img title="optics diagram" src="../images/optic-iso-flipped-2.svg" />
</div>

*Note:* `re` in the Haskell [`lens`](http://hackage.haskell.org/package/lens) only turns a `Review` into a `Getter`, as
the asymmetry of van Laarhoven encoding prevents making more general `re`. Therefore we have `from` to invert `Iso`.

*Note:* `re` doesn't turn `Lens` into `Prism` (or vice versa), that's discussed later.

<h3 id="t:Profunctor">type class: Profunctor</h3>

<blockquote>
Intuitively
Profunctor
is a bifunctor where the first argument is contravariant
and the second argument is covariant.
</blockquote>

Other way to see it is a generalization
of function, which can be pre- and post-composed with other functions but not
with itself.

```haskell
class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

    {-# MINIMAL dimap | (lmap , rmap) #-}
```

*Note:* here and later we won't state the laws of the type classes. Also note that
optics and Profunctor laws aren't connected, we can construct and use incorrect
optics using lawful Profunctors. The next (after `5.2`) release of
`profunctors` library will contain laws in the haddock documentation. This post
will be later updated to contain links to the Hackage documentation.

<h3 id="v:iso">constructor: iso</h3>

`iso` builds an isomorphism from a pair of inverse functions.

```haskell
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap
```

<h3>soundness and completeness</h3>

The describing operations of Iso are
[`viewP`](#v:view) (of `Getter`) and
[`reviewP`](#v:review) (of `Review`). Using those we can state
completeness equation for `Iso`:

```haskell
iso_complete :: Iso s t a b -> Iso s t a b
iso_complete o = iso (viewP o) (reviewP o)
```

If compiler would be smart enough, it can simplify that definition to
`iso_complete p = p`. Unfortunately it isn't, so we cannot use
[`ghc-proofs`](https://github.com/nomeata/ghc-proofs)
([blog post](https://www.joachim-breitner.de/blog/717-Why_prove_programs_equivalent_when_your_compiler_can_do_that_for_you_))
by [Joachim Breitner](https://www.joachim-breitner.de) for completeness proofs.
Luckily, soundness proofs are simpler, so we can *prove* them, we only have to
eta-expand the functions (which would make HLint unhappy).

```haskell
iso_sound1_proof :: ()
iso_sound1_proof =
    (\getter setter s -> viewP (iso getter setter) s)
    ===
    (\getter _setter s -> getter s)

iso_sound2_proof :: ()
iso_sound2_proof =
    (\getter setter b -> reviewP (iso getter setter) b)
    ===
    (\_getter setter b -> setter b)
```


Prism
-----

A prism is a first-class pattern.
Prisms are to sum data types as lenses are to product data types.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-prism.svg" />
</div>

```haskell
type Prism s t a b = forall p. Choice p => Optic p s t a b
```

<h3>laws</h3>


1. First, if I `review` a value with a `Prism` and then `preview`, I will get it back:
```
preview l (review l b) ≡ Just b
```
2. If you can extract a value a using a `Prism` `l` from a value `s`, then the value `s` is completely described by `l` and `a`:
```
preview l s ≡ Just a ⇒ review l a ≡ s
```

<h3 id="t:Choice">type class: Choice</h3>

<blockquote>
The generalization of Costar of Functor that is strong
with respect to Either.
</blockquote>

```haskell
class Profunctor p => Choice p where
    left' :: p a b -> p (Either a c) (Either b c)
    left' =  dimap swapE swapE . right'

    right' :: p a b -> p (Either c a) (Either c b)
    right' = dimap swapE swapE. left'

    {-# MINIMAL left' | right' #-}

swapE :: Either a b -> Either b a
swapE = either Right Left
```

*Note:* that `left'` and `right'` are `Prism`s, called `_Left` and `_Right`
in Haskell's [`lens`](http://hackage.haskell.org/package/lens).

```
λ> :t left' :: Prism (Either a c) (Either b c) a b
left' :: Prism (Either a c) (Either b c) a b
  :: Choice p => Optic p (Either a c) (Either b c) a b

λ> :t right' :: Prism (Either c a) (Either c b) a b
right' :: Prism (Either c a) (Either c b) a b
  :: Choice p => Optic p (Either c a) (Either c b) a b
```


<h3 id="v:prism">constructor: prism</h3>

`prism` builds a `Prism` from `build` (which is a setter) and `match` (which is a getter)
functions:

```haskell
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism setter getter = dimap getter (either id setter) . right'
```

*Note:* That's how they will be defined in [purescript-profunctor-lenses](https://github.com/purescript-contrib/purescript-profunctor-lenses/issues/57),
and are in [`mezzolens`](http://hackage.haskell.org/package/mezzolens-0.0.0/docs/src/Mezzolens-Unchecked.html#prism).

There the `right'` is a foundational operation letting us to focus on the
smaller part of the sum (`Either`). `dimap` preprocesses the input, recall it
witnesses an isomorphism, in this case `Iso s t (Either t a) (Either t b)`.
But that's not an isomorphism! The more theoretically correct definition would
be to use some (existential) type:

```haskell
eprism :: (Either e b -> t) -> (s -> Either e a) -> Prism s t a b
eprism build match = dimap match build . right'
```

The existential definition is an insight I learned from Edward Kmett;
similar approach applies to `Lens`. In practice (think about any bigger sum type),
this is not good way to do things, as we don't have way to say
"`SumOfThree` which isn't constructed with `FirstOfThree`".

<h3 id="v:previewE">operation: previewE</h3>

Describing operations are
`preview` (see [Affine Fold](#fold-fold1-and-affine-fold)) and
`review` (see [Review](#review)). Yet, careful reader may notice
that type of `preview` is `s -> Maybe a`, which would only allow us to define
a monomorphic constructor:

```haskell
prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' setter getter = prism setter (\s -> maybe (Left s) Right (getter s))
```

```haskell
-- previewE :: Prism s t a b -> s -> Either t a
previewE :: Optic (ForgetE a) s t a b -> s -> Either t a
previewE o = runForgetE (o (ForgetE Right))
```

<h3>soundness and completeness</h3>

`previewE` together with `reviewP` completely describe `Prism`:

```haskell
prism_complete :: Prism s t a b -> Prism s t a b
prism_complete p = prism (reviewP p) (previewE p)
```

The soundness properties are obvious:

```haskell
prism_sound1_proof :: ()
prism_sound1_proof =
    (\getter setter s -> previewE (prism setter getter) s)
    ===
    (\getter _setter s -> getter s)

prism_sound2_proof :: ()
prism_sound2_proof =
    (\getter setter b -> reviewP (prism setter getter) b)
    ===
    (\_getter setter b -> setter b)
```

<h3 id="t:ForgetE">data type: ForgetE</h3>

`ForgetE` is the first profunctor we see from family of `Forget` profunctors.
It's the one which isn't an instance of `Traversing1`.

```haskell
newtype ForgetE r a b = ForgetE { runForgetE :: a -> Either b r }
```

[Instance definitions are in the appendix](#instances-forgete).

Review
------

A `Review` describes how to construct a single value. It's a dual of `Getter`.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-prim-review.svg" />
<img title="optics diagram" src="../images/optic-review.svg" />
</div>

```haskell
type PrimReview s t a b = forall p. Bifunctor p => Optic p s t a b
type Review t b = forall p. (Bifunctor p, Choice p) => Optic' p t b
```

Unlike a `Prism` a `Review` is write-only. Since a `Review` cannot be used
to read back there are no laws that can be applied to it. In fact, it is
isomorphic to an arbitrary function from `(b -> t)` as witnessed by `upto` and `review`.
Similarly, there are no laws for `Getter` either.

*Note* that `Review` isn't `Simple PrimReview`, by using both `Bifunctor` and
`Profunctor` constraint we say that the first argument of a bifunctor is *phantom*.
That how we remove the first rail from the diagram.

In practice we don't need two-in-one `PrimReview` (or `PrimGetter`),
so there is little reason to define it in the libraries.

<h3 id="t:Bifunctor">type class: Bifunctor</h3>

`Bifunctor` class is a bifunctor where both arguments are covariant, unlike
`Profunctor` which is contravariant in the first argument.
`Bifunctor` is in the `base` library since version `4.8.0.0`.
Many Prelude types are `Bifunctor`s, e.g. `Either` and `(,)`.

```haskell
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

    {-# MINIMAL bimap | (first, second) #-}
```

<h3 id="v:upto">constructor: upto</h3>

You can generate a `Review` by using `unto`. You can also use any `Prism` or `Iso` directly as a `Review`.

```haskell
upto :: (b -> t) -> Review t b
upto f = bimap f f

uptoP :: (a -> s) -> (b -> t) -> PrimReview s t a b
uptoP = bimap
```

There is another way to define `upto`, only using the function argument once.
As the first argument of a profunctor is phantom, we can freely change it:
```haskell
firstPhantom :: (Bifunctor p, Profunctor p) => p a c -> p b c
firstPhantom p = lmap (const ()) (first (const ()) p)

upto' :: (b -> t) -> Review t b
upto' f = firstPhantom . rmap f
```

<h3 id="v:review">operation: review</h3>

> This can be used to turn an `Iso` or `Prism` around and view a value through it
the other way.

```haskell
review :: Optic' Tagged t b -> b -> t
review o b = unTagged (o (Tagged b))

reviewP :: Optic Tagged s t a b -> b -> t
reviewP o b = unTagged (o (Tagged b))
```

<h3>soundness and completeness</h3>

Soundness and completeness of `review` and `upto` is trivial to show:

```haskell
review_complete :: Review t b -> Review t b
review_complete o = upto (review o)
```

```haskell
review_sound_proof :: ()
review_sound_proof =
    (\build b -> review (upto build) b)
    ===
    (\build b -> build b)
```

<h3 id="t:Tagged">data type: Tagged</h3>

<blockquote>
A <code>Tagged a b</code> value is a value <code>b</code> with an attached phantom type <code>a</code>.
</blockquote>
Thus it's `Profunctor` and `Bifunctor`.

```haskell
newtype Tagged a b = Tagged { unTagged :: b }
```

[Instance definitions are in the appendix](#instances-tagged).

Getter
------

A Getter describes how to retrieve a single value in a way that can be composed
with other optics. It's a dual of [Review](#review).

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-prim-getter.svg" />
<img title="optics diagram" src="../images/optic-getter.svg" />
</div>

```haskell
type PrimGetter s t a b = forall p. Bicontravariant p => Optic p s t a b
type Getter s a = forall p. (Bicontravariant p, Strong p) => Optic' p s a
```

Unlike a `Lens` a `Getter` is read-only. Since a `Getter` cannot be used
to write back there are no Lens laws that can be applied to it. In fact, it is
isomorphic to an arbitrary function from `(s -> a)`.

<h3 id="t:Bicontravariant">type class: Bicontravariant</h3>

`Bicontravariant` as a bifunctor contravariant in both arguments. It's a dual
of `Bifunctor`. AFAIK none widely used package defines
this type class.

```haskell
class Bicontravariant p where
    cimap :: (b -> a) -> (d -> c) -> p a c -> p b d
    cimap f g = cofirst f . cosecond g

    cofirst :: (b -> a) -> p a c -> p b c
    cofirst f = cimap f id

    cosecond :: (c -> b) -> p a b -> p a c
    cosecond = cimap id

    {-# MINIMAL cimap | (cofirst, cosecond) #-}
```

<h3 id="v:to">constructor: to</h3>

We can use `to` construct a `Getter`. Unsurprisingly, it's definition is
similar to the definition of `upto`:

```haskell
to :: (s -> a) -> Getter s a
to f = cimap f f

toP :: (s -> a) -> (t -> b) -> PrimGetter s t a b
toP = cimap
```

<h3 id="v:view">operation: view</h3>

Definition of `view` is more complicated than for `review`.  We start with a
function `a -> a` (wrapped as `Forget` `a a a`), where the result type is fixed; and
transform it into `s -> a` using the optic.

```haskell
view :: Optic' (Forget a) s a -> s -> a
view o = runForget (o (Forget id))

viewP :: Optic (Forget a) s t a b -> s -> a
viewP o = runForget (o (Forget id))
```

*Note:* `view`, as defined above, accepts also other optics than `Lens`, but
in any case it returns only single `a`.

<h3>soundness and completeness</h3>

Soundness and completeness of `to` and `view` is trivial to show,
similarly to `Review.

```haskell
getter_complete :: Getter s a -> Getter s a
getter_complete o = to (view o)
```

```haskell
getter_sound_proof :: ()
getter_sound_proof =
    (\getter b -> view (to getter) b)
    ===
    (\getter b -> getter b)
```

<h3 id="t:Forget">data type: Forget</h3>

`Forget` is a bifunctor contravariant in the first argument,
and phantom in the second. It's used to implement operations on all optics
containing `Bicontravariant` constraint, i.e. `Getter` and the folds.

```haskell
newtype Forget r a b = Forget { runForget :: a -> r }
```

*Note:* `Forget r` is isomorphic to `Star` `(Const r)`.

[Instance definitions are in the appendix](#instances-forget).

Lens
----

<blockquote>
A <code>Lens s t a b</code> is a purely functional reference.
</blockquote>

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-lens.svg" />
</div>

```haskell
type Lens s t a b = forall p. Strong p => Optic p s t a b
```

<h3 id="laws:lens">laws</h3>

1. You get back what you put in:
```
view l (set l v s)  ≡ v
```
2. Putting back what you got doesn't change anything:
```
set l (view l s) s  ≡ s
```
3. Setting twice is the same as setting once:
```
set l v' (set l v s) ≡ set l v' s
```

<h3 id="t:Strong">type class: Strong</h3>

<blockquote>
Generalizing Star of a strong Functor.
</blockquote>

```haskell
class Profunctor p => Strong p where
  first' :: p a b  -> p (a, c) (b, c)
  first' = dimap swap swap . second'

  second' :: p a b -> p (c, a) (c, b)
  second' = dimap swap swap . first'

  {-# MINIMAL first' | second' #-}
```

*Note:*: `first'` and `second`' are lenses, however they aren't `_1` and `_2`
as latter have more general type in Kmett's [`lens`](http://hackage.haskell.org/package/lens).

<h3 id="v:lens">constructor: lens</h3>

`lens` builds a `Lens` from `getter` and `setter`.

```haskell
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter pab = dimap
     (\s -> (getter s, s))
     (\(b, s) -> setter s b)
     (first' pab)
```

*Note:* There is [an issue in `profunctors`](https://github.com/ekmett/profunctors/issues/47)
whether this method should be in `Strong` (similarly as `prism` in `Choice`).

<h3>soundness and completeness</h3>

`viewP` (from `Getter) and `set` (from `Setter`) completely describe a `Lens`:

```haskell
lens_complete :: Lens s t a b -> Lens s t a b
lens_complete o = lens (viewP o) (set o)
```

And the operations are obviously sound:

```haskell
lens_sound1_proof :: ()
lens_sound1_proof =
    (\getter setter s -> viewP (lens getter setter) s)
    ===
    (\getter _setter s -> getter s)

lens_sound2_proof :: ()
lens_sound2_proof =
    (\getter setter s b -> set (lens getter setter) s b)
    ===
    (\_getter setter s b -> setter s b)
```

Affine Traversal
----------------

Affine Traversal is an optic that has 0 or 1 target. A bit like `Prism`, but unlike
it (and like `Lens`) you cannot use it to construct the value, only change the inner part.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-affine-traversal.svg" />
</div>

```haskell
type AffineTraversal s t a b =
    forall p. (Strong p, Choice p) => Optic p s t a b
```

If `Iso` combines the both good properties of `Lens` and `Prism`, `AffineTraversal` combines
both bad ones: it what you get when you compose `Lens` with `Prism` (or `Prism` with `Lens`):

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-affine-composition.svg" />
</div>

<h3 id="laws:affine-traversal">laws</h3>

As with `Iso` we can deduce Affine Traversal laws from `Lens` and `Prism` laws:

1. You get back what you put in:
```
preview l (set l v s)  ≡ v <$ preview l s
```

2. If you can extract a value, and put it back, that doesn't change anything.
```
preview l s ≡ Just a => set l s a ≡ s
```

3. If you get nothing when extracting a value, then whatever you put in, the operation is no-op.
```
preview l s ≡ Nothing => set l s a ≡ s
```

4. Setting twice is the same as setting once:
```
set l v' (set l v s) ≡ set l v' s
```

<h3 id="v:affine">constructor: affine</h3>

```haskell
affineTraversal :: (s -> Either t a)
                -> (s -> b -> t)
                -> AffineTraversal s t a b
affineTraversal getter setter pab = dimap
    (\s -> (getter s, s))
    (\(bt, s) -> either id (setter s) bt)
    (first' (right' pab))
```

<h3>soundness and completeness</h3>

The describing operations of `AffineTraversal` are
`previewE` and
`set`. Using those we can state
completeness equation for Affine Traversal:

```haskell
affine_traversal_complete ::
    AffineTraversal s t a b -> AffineTraversal s t a b
affine_traversal_complete o = affineTraversal (previewE o) (set o)
```

The soundness proofs:

```haskell
affine_traversal_sound1_proof :: ()
affine_traversal_sound1_proof =
    (\getter setter s -> previewE (affineTraversal getter setter) s)
    ===
    (\getter _setter s -> getter s)

affine_traversal_sound2_proof :: ()
affine_traversal_sound2_proof =
    (\getter setter s b -> set (affineTraversal getter setter) s b)
    =/=
    (\_getter setter s b -> setter s b)
```

`ghc-proofs` fails to prove the second one when trying to unify:
```
\getter setter s b ->
    case getter s of
        Left x -> x
        Right y -> setter s b
\getter setter s b ->
    setter s b
```

Here we need to use the third law of Affine Traversal to proceed.
In the `Left x` clause, `x` and `setter s b` are equivalent;
as we get nothing from the getter (the `x` is `s`), setting the value
is a no-op.

Traversal1
----------

<blockquote>
A <code>Traversal s t a b</code> (<code>Traversal1 s t a b</code>) is a generalization of
<code>traverse</code> (<code>traverse1</code>) from <code>Traversable</code> (<code>Traversable1</code>). It allows you to
traverse over a structure and change out its contents with
monadic or <code>Applicative</code> (<code>Apply</code>) side-effects.
</blockquote>

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-traversal1.svg" />
</div>

```haskell
type Traversal1 s t a b = forall p. Traversing1 p => Optic p s t a b
```

<h3>laws</h3>

The laws of `Traversal` can be stated for `Traversal1` too.

1. Identity
```
traverse1Of t (Id . f) ≡  Id (fmap f)
```
2. Composition
```
Compose . fmap (traverse1Of t f) . traverse1Of t g ≡
    traverse1Of t (Compose . fmap f . g)
```

<h3 id="t:Traversing1">type class: Traversing1</h3>

```haskell
class (Strong p) => Traversing1 p where
    traverse1' :: Traversable1 f => p a b -> p (f a) (f b)
    traverse1' = wander1 traverse1

    wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t)
            -> p a b -> p s t
```

`traverse1'` can be defined using `wander1`, but the implemenation
is not very insightful, the `Traversable` `Baz1` type is the key:

```haskell
newtype Baz1 t b a
    = Baz1 { runBaz :: forall f. Apply f => (a -> f b) -> f t }
```

See [`profunctors` source](https://github.com/ekmett/profunctors/blob/7c2d0b7778951269f27fb5a2343abc1b80f00d4b/src/Data/Profunctor/Traversing.hs#L118)
for the details.

*Note:* `Traversing1` determines `Strong`:

```haskell
firstTraversing1 :: Traversing1 p => p a b -> p (a, c) (b, c)
firstTraversing1 = dimap swap swap . traverse1'

secondTraversing1 :: Traversing1 p => p a b -> p (c, a) (c, b)
secondTraversing1 = traverse1'
```

<h3 id="v:traversing1">constructor: traversing1</h3>

As `Traversing1` is cleverly defined class, the constructor of a `Traversal1`
is just a new name for `wander1`.

```haskell
traversing1 :: (forall f. Apply f => (a -> f b) -> s -> f t)
            -> Traversal1 s t a b
traversing1 = wander1
```

<h3 id="v:traverse1Of">operation: traverse1Of</h3>

> Map each element of a structure targeted by a `Lens` or
`Traversal1`, evaluate these actions from left to right, and
collect the results.

```haskell
-- traverse1Of :: Traversal1 s t a b
--             -> (forall f. Apply f => (a -> f b) -> s -> f t)
traverse1Of :: Apply f
            => Optic (Star f) s t a b
            -> (a -> f b) -> s -> f t
traverse1Of o f = runStar (o (Star f))
```

*Note:* the `Apply f` constraint is redundant, but it will be needed
to satisfy `Traversing1 (Star f)`! It makes sense to define:

```haskell
withStar :: Optic (Star f) s t a b
         -> (a -> f b) -> s -> f t
withStar o f  = runStar (o (Star f))
```

*Note:* In van Laarhoven encoding, implementation of `traverse1Of` (and `traverseOf`)
is simply an `id` (with less general type). In profunctor encoding
the `over` operation is `id`!

<h3 id="t:Star">data type: Star</h3>

`Star` is isomorphic to [`Kleisli` from `base`](http://hackage.haskell.org/package/base-4.9.1.0/docs/src/Control.Arrow.html#t:Kleisli).
It lifts a `Functor` into a `Profunctor` (forwards).

```haskell
newtype Star f a b = Star { runStar :: a -> f b }
```

[Instance definitions are in the appendix](#instances-star).

Traversal
---------

<blockquote>
A <code>Traversal s t a b</code> (<code>Traversal1 s t a b</code>) is a generalization of
<code>traverse</code> (<code>traverse1</code>) from <code>Traversable</code> (<code>Traversable1</code>). It allows you to
traverse over a structure and change out its contents with
monadic or <code>Applicative</code> (<code>Apply</code>) side-effects.
</blockquote>

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-traversal.svg" />
</div>

```haskell
type Traversal s t a b = forall p. Traversing p => Optic p s t a b
```

<h3>laws</h3>

The laws for a `Traversal` `t` follow from the laws for `Traversable` as stated in
["The Essence of the Iterator Pattern"](http://dl.acm.org/citation.cfm?id=1561023).

1. Identity
```
traverseOf t (Id . f) ≡  Id (fmap f)
```
2. Composition
```
Compose . fmap (traverseOf t f) . traverseOf t g ≡
    traverseOf t (Compose . fmap f . g)
```

<h3 id="t:Traversing">type class: Traversing</h3>

Note: Definitions in terms of 'wander' are much more efficient!
Note: traverse' can be defined in terms of wander.

```haskell
class (Choice p, Traversing1 p) => Traversing p where
  traverse' :: Traversable f => p a b -> p (f a) (f b)
  traverse' = wander traverse

  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
         -> p a b -> p s t
```

`Traversing` fully determines `Choice`:

```haskell
leftTraversing :: Traversing p => p a b -> p (Either a c) (Either b c)
leftTraversing = dimap swapE swapE . traverse'

rightTraversing :: Traversing p => p a b -> p (Either c a) (Either c b)
rightTraversing = traverse'
```

The `Traversing` class is needed as `Choice` and `Traversing1` doesn't
necessarily agree with `Traversing`. Intuitively `Choice` can be used
to check whether a container is empty or not, and if not then we could
use `Traversing1`.
If the classes are internal to the optics library, `Traversing` is still useful to provide more efficient `wander`.
The situation similar to `Applicative` ⊂ `Apply` and `Pointed` situation,
superset because there are extra interaction laws.

<h3 id="v:traversing">constructor: traversing</h3>

Similarly as `traversing1` is an alias to `wander1`,
`traversing` is an alias to `wander`.

```haskell
traversing :: (forall f. Applicative f => (a -> f b) -> s -> f t)
           -> Traversal s t a b
traversing = wander
```

<h3 id="v:traverseOf">operation: traverseOf</h3>

> Map each element of a structure targeted by a `Lens`,
`Traversal1` or `Traversal`, evaluate these actions from left to right, and
collect the results.

```haskell
-- traverseOf :: Traversal1 s t a b
--            -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf :: Applicative f
           => Optic (Star f) s t a b
           -> (a -> f b) -> s -> f t
traverseOf = withStar
```

*Note* the definition is same as of `traverse1Of`, the context differs.  `Star`
is `Traversing` only if `f` is `Applicative`; and `Traversing1` if `f` is
`Apply`.

<h3 id="v:partsOf">operation: partsOf</h3>

You may ask: How `Traversal s t a b` is different from `Lens s t [a] [b]`?
The difference is the same as between `AffineTraversal` and `Lens`.
`Traversal` don't let you to remove the elements. We can however write a conversion
function

```haskell
partsOf :: Traversal s s a a -> Lens s s [a] [a]
partsOf o = lens getter setter
  where
    getter s = foldMapOf o (:[]) s
    setter s xs = evalState (traverseOf o (state . fill) s) xs
    fill a []     = (a, [])
    fill _ (a:as) = (a, as)
```

The note in `partsOf` documentation in `lens` package says:
<blockquote>
You should really try to maintain the invariant of the number of children in the list.
</blockquote>

The implementation will do it for us too, we cannot remove or add elements using `partsOf`:
it will either use old if not given enough or drop excess ones.
That is the reason why we can work only with a type-preserving `Traversal` (in fact we can
write a version for `Traveral s t a a`, but there are still `a a`, not `a b`).

```
λ> view (partsOf traverse') [1,2,3]
[1,2,3]
λ> set (partsOf traverse') [1,2,3] [4,5]
[4,5,3]
λ> set (partsOf traverse') [1,2,3] [4,5,6,7]
[4,5,6]
```

Fold, Fold1, and Affine Fold
----------------------------

> A `Fold s a` is a generalization of something `Foldable`. It allows you to
extract multiple results from a container.

`Fold1` extract at least one result, `AffineFold` at most one. (`Getter`
exactly one.) Since folds are read-only, there are now laws.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-affine-fold.svg" />
<img title="optics diagram" src="../images/optic-fold1.svg" />
<img title="optics diagram" src="../images/optic-fold.svg" />
</div>

```haskell
type AffineFold s a
    = forall p. (Strong p, Choice p, Bicontravariant p) => Optic' p s a
type Fold1 s a
    = forall p. (Traversing1 p, Bicontravariant p) => Optic' p s a
type Fold s a
    = forall p. (Traversing p, Bicontravariant p) => Optic' p s a
```

<h3 id="v:foldMapOf">operation: foldMapOf</h3>

```haskell
foldMapOf :: Optic' (Forget r) s a -> (a -> r) -> s -> r
foldMapOf o f = runForget (o (Forget f))
```

<h3 id="v:toListOf">operation: toListOf</h3>

Using `foldMapOf` is easy to define various folds, `toListOf` is useful
at least for examples:

```haskell
toListOf :: Optic' (Forget (Endo [a])) s a -> s ->  [a]
toListOf o s = appEndo (foldMapOf o (Endo . (:)) s) []
```

*Note:* we use `Endo` as a difference list, `[a]` would do it too, but less
efficiently.

<h3 id="v:foldMap1Of">operation: foldMap1Of</h3>

The definition of `foldMap1Of` is exactly the same as of `foldMapOf`.
If passed in optic uses only `Traversing1` constraint, then
only the `Semigroup r` would be required on `r` in `Forget`.

<h3 id="v:preview">operation: preview</h3>

```haskell
preview :: Optic' (ForgetM a) s a -> s -> Maybe a
preview o = runForgetM (o (ForgetM Just))
```

<h3 id="v:folding">constructor: folding</h3>

```haskell
folding :: Foldable f => (s -> f a) -> Fold s a
folding f = cimap f (const ()) . wander traverse_
```

<h3>soundness and completeness</h3>

Completeness is not as obvious as previously,
as here we have to pick *some* `Foldable`. List is a safe choice (`DList` would be more efficient).

```haskell
fold_complete :: Fold s a -> Fold s a
fold_complete o = folding (toListOf o)
```

The soundness proof is also complicated. First GHC wants us to annotate
the types to resolve ambiguous `Foldable` and `Monoid`.

```haskell
fold_sound_proof :: ()
fold_sound_proof =
    (\(f :: Int -> [Int]) (s :: [Int]) -> foldMapOf (folding id) f s)
    =/=
    (\f s -> foldMap f s)
```

The proof failed with the following obligation left:

```
    Simplified LHS
        (\ (f :: Int -> [Int]) (s :: [Int]) ->
           foldr
             @ Int
             @ (Const [Int] ())
             ((\ (x :: Int) -> ++ @ Int (f x)) `cast` ...)
             (([] @ Int) `cast` ...)
             s)
        `cast` ...
    Simplified RHS:
        \ (f :: Int -> [Int]) (s :: [Int]) ->
          foldr
            @ Int
            @ [Int]
            (\ (x :: Int) -> ++ @ Int (f x))
            ([] @ Int)
            s
```

It's easy to see that the expressions are same, except for the casts.
Possibly `ghc-proof` could erase types (and casts) and compare expressions
after that.

<h3 id="v:folding1">constructor: folding1</h3>

Similar to `folding`:

```haskell
folding1 :: Foldable1 f => (s -> f a) -> Fold1 s a
folding1 f = cimap f (const ()) . wander1 traverse1_
```

<h3 id="v:afolding">constructor: afolding</h3>

Because there isn't a `Foldable` variant for *at most one element* containers,
we'll use `Maybe`:

```haskell
afolding :: (s -> Maybe a) -> AffineFold s a
afolding f = cimap (\s -> maybe (Left s) Right (f s)) Left . right'
```

*Note*: we don't use `Strong` constraint here.

We can freely convert between `AffineFold s a` and `Getter s (Maybe a)`:

```haskell
getterToAF, getterToAF' :: Getter s (Maybe a) -> AffineFold s a
getterToAF o = afolding (view o)
getterToAF' o = o . _Just

afToGetter :: AffineFold s a -> Getter s (Maybe a)
afToGetter o = to (preview o)
```

<h3 id="t:ForgetM">data type: ForgetM</h3>

`ForgetM` is a variant of `Forget` with a value wrapped in `Maybe`. It's used
to implement `preview`.

```haskell
newtype ForgetM r a b = ForgetM { runForgetM :: a -> Maybe r }
```

[Instance definitions are in the appendix](#instances-forget).

Setter
------

Another top class of an optics hierarchy is a `Setter`. It's a generalization
of `fmap` from `Functor`.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-setter.svg" />
</div>

```haskell
type Setter s t a b = forall p. Mapping p => Optic p s t a b
```

<h3>laws</h3>

The only `Lens` law that can apply to a `Setter` `l` is that

```
set l y (set l x a) ≡ set l y a
```
You can't `view` a `Setter` in general, so the other two lens laws are irrelevant.

However, two `Functor` laws apply to a `Setter`:

```
over l id ≡ id
over l f . over l g ≡ over l (f . g)
```

<h3 id="t:Mapping">type class: Mapping</h3>

The third in series of `Traversable1`, `Traversable`, `Functor` type classes:
is `Mapping`.

```haskell
class (Traversing p, Closed p) => Mapping p where
    map' :: Functor f => p a b -> p (f a) (f b)
    map' = roam collect

    roam :: (forall f. (Applicative f,  Distributive f)
         => (a -> f b) -> s -> f t)
         -> p a b -> p s t
    roam = roamMap' map'

    {-# MINIMAL map' | roam #-}
```

Originally I defined `roam` using `Representable` constraint,
because I didn't know how to implement `setting` without it. Turns out, it's possible
by using `map'` as shown in [a post in r6research blog](http://r6research.livejournal.com/27476.html).

`Applicative` constraint isn't strictly necessary, but is required
to implement `wanderMapping` as mentioned in [`profunctors` #50 pull request](https://github.com/ekmett/profunctors/pull/50).
Also `roamMap'` implementation is from that pull request.
On the other hand, there is [a `distributive` issue](https://github.com/ekmett/distributive/issues/12)
to add `Applicative` constraint to `Distributive` anyway.

<h3 id="v:setting">constructor: setting</h3>

`setting` builds a `Setter` from a `map` like function.
In my original try, I had to use `Representable` constraint, as I used `index` and `tabulate`.
However, [r6research](http://r6research.livejournal.com/27476.html) shows
how to define `setting` using `Context` (`PStore` in the post), without relying on the `Representable`.

```haskell
setting :: ((a -> b) -> s -> t) -> Setter s t a b
setting f = dimap (Context id) (\(Context g s) -> f g s) . map'

data Context a b t = Context (b -> t) a deriving Functor
```

```
λ> over (setting fmap) (+1) [1,2,3]
[2,3,4]
```

`Context` is the indexed store can be used to characterize a `Lens`, and seems
the `Setter`.

The definition using `Representable`:
```
setting :: ((a -> b) -> s -> t) -> Setter s t a b
setting f = roam $ \g s -> tabulate $ \idx ->
    f (flip index idx . g) s
```

The useful `Setter` is `mapped`:

```haskell
mapped :: Functor f => Setter (f a) (f b) a b
mapped = setting fmap
```

<h3 id="v:collecting">constructor: collecting</h3>

`collecting` is another name for `roam`. Name would
suggest we'd require only `Distributive`, but that won't be enough,
because of the way we defined `Mapping`.

```haskell
collecting
    :: (forall f. (Applicative f, Distributive f) => (a -> f b) -> s -> f t)
    -> Setter s t a b
collecting = roam
```

<h3 id="v:over">operation: over</h3>

Modify all targets of the optic with a function. `over` is specific version
of `id`.

```haskell
over :: Optic (->) s t a b -> (a -> b) -> s -> t
over = id
```

<h3 id="v:set">operation: set</h3>

```haskell
set :: Optic (->) s t a b -> s -> b -> t
set o s b = over o (const b) s
```

<h3 id="v:collectOf">operation: collectOf</h3>

We can also make multiple copies at once. We should be OK with just
`Distributive`, but it's simpler to require `Representable`.

```haskell
collectOf :: (Applicative f, Distributive f)
          => Optic (Star (WrappedApplicative f)) s t a b
          -> (a -> f b) -> s -> f t
collectOf o f = unwrapApplicative . runStar (o (Star (WrapApplicative . f)))
```

<h3>soundness and completeness</h3>

```haskell
setter_complete :: Setter s t a b -> Setter s t a b
setter_complete = setting . over
```

In soundness proof, we have to eta-expand `g`:

```haskell
setter_sound_proof :: ()
setter_sound_proof =
    (\f g s -> over (setting f) g s)
    ===
    (\f g s -> f (\x -> g x) s)
```

Another way is to say that `collectOf` and `collecting` are the basic operations:

```haskell
setter_complete_2 :: Setter s t a b -> Setter s t a b
setter_complete_2 o = collecting (collectOf o)
```

Stating soundness property is quite complicated (as with traversals), so we omit it.

Indexed optics
--------------

Indexed optics are possible in the profunctor encoding.
The first step is to notice is that the index is an additional information
we extract from the bigger value,
so we can encode indexed optics as `Optic p s t (i, a) b`.

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-indexed.svg" />
</div>

`purescript-profunctor-lenses` uses a newtype, so will we too:
```haskell
newtype Indexed p i a b = Indexed { runIndexed :: p (i, a) b }

type IndexedOptic p i s t a b = Indexed p i a b -> p s t
type IndexedOptic' p i s a = IndexedOptic p i s s a a
```

[Instances](#instances-indexed) and simple operations are easy
to define:

```haskell
itraversing :: Traversing p
            => (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
            -> IndexedOptic p i s t a b
itraversing itr (Indexed pab) = wander (\f s -> itr (curry f) s) pab
```

```haskell
ifoldMapOf :: IndexedOptic' (Forget r) i s a -> (i -> a -> r) -> s -> r
ifoldMapOf o f = runForget (o (Indexed (Forget (uncurry f))))
```

The problematic part is the indexed optic composition: `icompose` (`<.>` clashes with the `Apply` operation).
Conceptually the operation is simple, second optic should just pass the first
index through:

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-icompose.svg" />
</div>

Writing the actual definition is simple (only?) if you know the right type:

```haskell
icompose :: Profunctor p
         => (i -> j -> k)
         -> (Indexed p i u v -> p s t)
         -> (Indexed (Indexed p i) j a b -> Indexed p i u v)
         -> (Indexed p k a b -> p s t)
icompose ijk stuv uvab ab = icompose' ijk
    (stuv . Indexed)
    (runIndexed . uvab . Indexed . Indexed)
    (runIndexed ab)

icompose' :: Profunctor p
          => (i -> j -> k)
          -> (p (i, u) v -> p s t)
          -> (p (i, (j, a)) b -> p (i, u) v)
          -> (p (k, a) b -> p s t)
icompose' ijk stuv uvab ab = stuv (uvab (lmap f ab))
  where
    f (i, (j, a)) = (ijk i j, a)
```

I conclude this section by showing an example, with an indexed
list traversal:

```haskell
itraverseList :: Applicative f => (Int -> a -> f b) -> [a] -> f [b]
itraverseList f = go 0
  where
    go _ []     = pure []
    go i (a:as) = (:) <$> f i a <*> go (i + 1) as

itraversedList :: Traversing p => IndexedOptic p Int [a] [b] a b
itraversedList = itraversing itraverseList
```
we can extract indexes in nested lists too:
```
λ> let xss = [[1,2],[3,4,5]]
λ> let o = icompose (,) itraversedList itraversedList
λ> ifoldMapOf o (\ij a -> [(ij, a)]) xss
[((0,0),1),((0,1),2),((1,0),3),((1,1),4),((1,2),5)]
```

A concrete optic
----------------

The operations presented here take optics applied to a concrete
`Profunctor`. For example the `Lens` operations:

```
viewP :: Optic (Forget a) s t a b -> s -> a
set   :: Optic (->)       s t a b -> s -> b -> t
```

`(->)` is a concrete profunctor for `Setter`, and `Forget` for the `Getter`.
What does the concrete `Lens` looks like? Let's take the lens operations,
and put them into the record:

```haskell
data Shop a b s t = Shop
    { shopGetter :: s -> a
    , shopSetter :: s -> b -> t
    }
```

Note that the argument pairs are flipped.
`Shop` is a `Strong` profunctor
([instances](#instances-shop)).
Recall that profunctor
optics transform profunctors: `p a b -> p s t`. If `p` is `Shop a b`,
then an optic will transform `Shop a b a b` into `Shop a b s t`,  from
which we can extract lens operations!

```haskell
type ALens s t a b = Optic (Shop a b) s t a b

cloneLens :: ALens s t a b -> Lens s t a b
cloneLens o = lens getter setter where
    Shop getter setter = o (Shop id (\_ -> id))
```

*Note:* `cloneLens` is a `Rank1Type` variant of `lens_complete` function.
Here we get both `Lens` operations in one go.

But why is `ALens` useful? In Haskell we cannot put `Lens` directly
into a container (we'd need impredicative types); we'll need either wrap
`Lens` into a newtype or alternatively we can use `ALens` variant.
Neither variant is composable, but `ALens` approach is simple,
as we don't need to explicitly wrap the optic:

```haskell
afirst :: ALens (a, c) (b, c) a b
afirst = first'
```

As JavaScript is dynamic language, there is no need for *an optic*,
as we can put whatever values in whatever container.
However, in a static typed language they are often needed.

<h2 id="v:re">operation: re</h2>

The `re` operation turns or rotates optics.

```haskell
re :: Optic (Re p a b) s t a b -> Optic p b a t s
re o = runRe (o (Re id))
```

The `Re` type, and its instances witness the symmetry of
`Profunctor` and the relation between `Bifunctor` and `Bicontravariant`:

```haskell
newtype Re p s t a b = Re { runRe :: p b a -> p t s }

instance Profunctor p => Profunctor (Re p s t) where
    dimap f g (Re p) = Re (p . dimap g f)

instance Bifunctor p => Bicontravariant (Re p s t) where
    cimap f g (Re p) = Re (p . bimap g f)

instance Bicontravariant p => Bifunctor (Re p s t) where
    bimap f g (Re p) = Re (p . cimap g f)
```

However there `Choice` and `Strong` aren't related in the same
way as `Bifunctor` and `Bicontravariant`. If you rotate `Prism`,
you don't get `Lens`:

<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-prism.svg" />
<img title="optics diagram" src="../images/optic-prism-flipped.svg" />
<img title="optics diagram" src="../images/optic-lens.svg" />
</div>

To make `Review` and `Getter` invertible (not only `Prim` variants),
we need two new type classes:

```haskell
class Profunctor p => Costrong p where
    unfirst :: p (a, d) (b, d) -> p a b
    unfirst = unsecond . dimap swap swap

    unsecond :: p (d, a) (d, b) -> p a b
    unsecond = unfirst . dimap swap swap

class Profunctor p => Cochoice p where
    unleft :: p (Either a d) (Either b d) -> p a b
    unleft = unright . dimap swapE swapE

    unright :: p (Either d a) (Either d b) -> p a b
    unright = unleft . dimap swapE swapE
```

The instances are in the [appendix](#instances-re).

We can prove per optic type and operation that `re . re` = `id`.

```haskell
rere_id_lens_set_proof :: ()
rere_id_lens_set_proof =
    (\getter setter s b -> set (lens getter setter) s b)
    ===
    (\getter setter s b -> set (re (re (lens getter setter))) s b)
```

Interesting question arises: what is `Cotraversing`? I don't know.

<h3 id="inverting-prism">Inverting Prism</h3>

After noting the `reset` in the [`bennofs' lpaste`](http://lpaste.net/103359),
I realised we *can try to* rotate `Prism` into `Lens`:

```haskell
rePrism :: Prism s t a b -> Lens b a t s
rePrism o = lens (reviewP o) (reset o)

reset :: Optic (Re (->) a b) s t a b -> b -> s -> a
reset = set . re

reover :: Optic (Re (->) a b) s t a b -> (t -> s) -> (b -> a)
reover = over . re
```

but there's a gotcha:

```
λ> :t set (rePrism right')
set (rePrism right') :: s -> Either c t -> t
```

if we try to `set` `rePrism right'` with `Left` value, it will loop.
Converting `Lens` to `Prism` is similarly problematic,

<h2 id="t:Closed">type class: Closed</h2>

The `profunctor` library defines `Closed` type class.
And it seems to be useful in the optics context too:
[http://r6research.livejournal.com/28050.html](http://r6research.livejournal.com/28050.html).
The support for `Grate` was recently added to [`purescript-profunctor-optics`](https://github.com/purescript-contrib/purescript-profunctor-lenses/pull/60).

<blockquote>
<p>A strong profunctor allows the monoidal structure to pass through.</p>

<p>A closed profunctor allows the closed structure to pass through.</p>
</blockquote>

```haskell
class Profunctor p => Closed p where
    closed :: p a b -> p (x -> a) (x -> b)
    closed = grate f
      where
        f :: (((x -> a) -> a) -> b) -> x -> b
        f g x = g ($ x)

    grate :: (((s -> a) -> b) -> t) -> p a b -> p s t
    grate f = dimap (flip ($)) f . closed

    {-# MINIMAL closed | grate #-}
```

Using `Closed` we can define a new optic, `Grate`:

```haskell
type Grate s t a b = forall p. Closed p => Optic p s t a b
```

On way to understand where it's useful, is through associated container
class: `Distributive`.

<blockquote>
To be distributive a container will need to have a way to consistently zip a
potentially infinite number of copies of itself. This effectively means that
the holes in all values of that type, must have the same cardinality, fixed
sized vectors, infinite streams, functions, etc. and no extra information to
try to merge together.
</blockquote>

The actual constructor are either `grate` or `closed`,
yet we can define some helpful constructors:

```haskell
cotraversed :: Distributive f => Grate (f a) (f b) a b
cotraversed = grate $ \f -> cotraverse f id

represented :: Representable f => Grate (f a) (f b) a b
represented = dimap index tabulate . closed
```

One way to use `Grate` is to `review` them, for example:

```haskell
_V2 :: Grate (V2 a) (V2 b) a b
_V2 = represented
```

```
λ> review (_V2 . right' . _V2) 1 :: V2 (Either Bool (V2 Int))
V2 (Right (V2 1 1)) (Right (V2 1 1))

λ> over _V2 (+1) (V2 1 2) :: V2 Int
V2 2 3
```

<h3 id="t:Zipping">data type: Zipping</h3>

Another use is to `zip` containers, using `Zipping`
(which is `Costar V2`, except we can define more [instances](#instances-zipping)).

```haskell
newtype Zipping a b = Zipping { runZipping :: a -> a -> b }
```

<h3 id="v:zipWithOf">operation: zipWithOf</h3>

```
zipWithOf :: Optic Zipping s t a b -> (a -> a -> b) -> s -> s -> t
zipWithOf o f = runZipping (o (Zipping f))
```

`Zipping` is also `Choice` and `Strong`, so we can zip inside
structures:

```
λ> let as = V2 (Left ())     (Right (1,2))
λ> let bs = V2 (Right (3,4)) (Right (5,6))
λ> zipWithOf (_V2 . right' . first') (,) as bs
V2 (Left ()) (Right ((1,5),2))
```

With `Prism`, non-matching elements are taking from the first argument.
In `Lens` case "left-over" part is also taken from the first argument.

*Note:* we can `zipWithOf` only containers with the same element type. And
of the same size, so it's combination of `zipWith` and `alignWith` (from [`these` package](http://hackage.haskell.org/package/these)).

*Note:* I think it's possible to implement both variants: common `zipWithOf`
and `alignWithOf` using `Traversal`, using `State` trick as in `partsOf`: Pick
the shorter or longer container and zip/align into it. That won't work with
infinite structures.

<h2 id="t:Monoidal">type class: Monoidal</h2>

There is at least one more, not well know profunctor class `Monoidal`.
It's used in [`opaleye`](https://hackage.haskell.org/package/opaleye)
(as [`ProductProfunctor`](https://hackage.haskell.org/package/product-profunctors),
author seems want to remove `empty` and `***!`, yet I do want to use them).

```haskell
class Profunctor p => Semigroupal p where
    mult :: p a b -> p c d -> p (a, c) (b, d)

class Semigroupal p => Monoidal p where
    unit :: p () ()
```

We can define a variant of `_V2` using `Semigroupal` (I have a strong tempation
to call an optic using `Monoidal`, a *Monocle*; yet the *Stereographic (glasses)*
is conceptually more correct).

```haskell
v2 :: Semigroupal p => Optic p (V2 a) (V2 b) a b
v2 p = dimap (\(V2 x y) -> (x, y)) (\(x, y) -> V2 x y) (mult p p)
```

With new `v2` the examples in `Closed` section work, both `review`

```
λ> review (v2 . right' . _V2) 1 :: V2 (Either Bool (V2 Int))
V2 (Right (V2 1 1)) (Right (V2 1 1))
```

and `zipWithOf`:

```
λ> let as = V2 (Left ())     (Right (1,2))
λ> let bs = V2 (Right (3,4)) (Right (5,6))
λ> zipWithOf (v2 . right' . first') (,) as bs
V2 (Left ()) (Right ((1,5),2))
```

But also `traverseOf`:

```
λ> let f x = state (\s -> (x + s, s +1))
λ> evalState (traverseOf v2 f (V2 5 7)) 1
V2 6 9
```

and `toListOf`:

```
λ> toListOf (v2 . v2) (V2 (V2 1 2) (V2 3 4))
[1,2,3,4]
```

In the ["Modular Data Access"](https://arxiv.org/abs/1703.10857) `Monoidal` is
used to implement `Traversal`; yet it's not enough alone: you have to add
`Choice` (which is called `Cocartesian` there) to deal with not `Representable`
containers (we have to check whether it's empty or not).  as well as `Strong`
to let extra content information pass through. This is OK for `Traversal`, but
not ok for `Traversal1`, which is `Choice`less optic.

As I don't understood `Grate` or `Monoidal`, I don't know where to put them in
the graph. If we look at the instances:

One way to see the subtle difference between `Monoidal` + `Strong` + `Choice` and
`Traversing` is try to define a `traversedList` (without relying on `Choice`):

```
traversedList :: (Strong p, Monoidal p)
              => Optic p [a] [b] a b
traversedList pab = _
```
It should be possible, but is far from straight forward:
`Strong` *is* required there, hint:
<div style="padding-left: 1em">
<img title="optics diagram" src="../images/optic-lens-monoidal.svg" />
</div>
Yet, I don't see any practical benefit: every `Choice` in the table below is
also a `Monoidal`, we don't get anything by not requiring `Choice` for
`Traversing` like operation.

| | `Forget` | `Star` | `Tagged` | `Zipping` |
| --- | --- | --- | --- | --- |
| `Bifunctor`       | no      | no      | **yes** | no      |
| `Bicontravariant` | **yes** | no      | no      | no      |
| `Choice`          | **yes** | **yes** | **yes** | **yes** |
| `Strong`          | **yes** | **yes** | no      | **yes** |
| `Closed`          | **yes** | no      | **yes** | **yes** |
| `Monoidal`        | **yes** | **yes** | **yes** | **yes** |

van Laarhoven encoding
----------------------

There're another ways to encode optics using type-classes,
other than profunctor one presented in this post.
[van Laarhoven style lenses](https://www.twanvl.nl/blog/haskell/cps-functional-references)
give rise to two ways to encode optics:
`Profunctor + Functor` = `OpticEK` used in Edward Kmett's [`lens`](http://hackage.haskell.org/package/lens)
and
`Functor` + `Functor` = `OpticVL` as showed in [r6reseach blog post](http://r6research.livejournal.com/28432.html):

```
type OpticP  c   s t a b  =
    forall p.    c p        => p a b        -> p s t
type OpticEK c c' s t a b =
    forall p f. (c p, c' f) => p a (f b)    -> p s (f t)
type OpticVL c c' s t a b =
    forall f g. (c f, c' g) => (g a -> f b) -> g s -> f t
```

The `OpticEK` variant is expressive and well understood, `lens`
library is an emperical evidence. The encoding is practical: in the `Strong` part of
the hierarchy `p` is a function arrow `(->)`.
Therefore `Strong` optics are of the form `forall f. c f => (a - > f b) -> s -> f t`,
as a consequence lenses (`c = Functor`) and traversals (`c = Applicative`)
can be defined in the libraries without dependency on the `lens` library.
In additon, indexed optics are there as well, in more ergonomic way (Profunctor variant needs
[`Indexable`](http://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Indexed.html#t:Indexable)
analogue to be able to talk about `p` and `Indexed p i` in an uniform way, when you don't care).
See [Edwards reply for more info](https://www.reddit.com/r/haskell/comments/6627nn/glassery_an_optics_zoo/dggdnvr/).

The another variant, `OpticVL` looks suspicious (and I'm not familiar with it at all),
but it seems you can do about anything with it as well. Let's explore the
`IsoVL` and `PrismVL`. First we define a bit different (but practical) alias:

```haskell
type OpticVL g f s t a b = (g a -> f b) -> g s -> f t
```

<h3 id="t:IsoVL">IsoVL</h3>

To form an isomorphism we require that `f` and `g` to be `Functor`s:

```haskell
type IsoVL s t a b =
    forall f g. (Functor g, Functor f) => OpticVL g f s t a b
```

The constructor definition is guided by the types:

```haskell
isoVL :: (s -> a) -> (b -> t) -> IsoVL s t a b
isoVL getter setter gafb gs = setter <$> gafb (getter <$> gs)
```

To `viewVL` and `reviewVL` we instantiate the functor arguments with
`Identity` and `Const` (and vice versa).
The definitions are elegantly symmetric:

```haskell
viewVL :: OpticVL Identity (Const a) s t a b -> s -> a
viewVL o s = getConst (o (Const . runIdentity) (Identity s))

reviewVL :: OpticVL (Const b) Identity s t a b -> b -> t
reviewVL o b = runIdentity (o (Identity . getConst) (Const b))
```

As before, we can state completeness and soundness equations for these operations:

```haskell
isoVL_complete :: IsoVL s t a b -> IsoVL s t a b
isoVL_complete o = isoVL (viewVL o) (reviewVL o)
```

and GHC proves some of them for us:

```haskell
isoVL_sound1_proof :: ()
isoVL_sound1_proof =
    (\getter setter s -> viewVL (isoVL getter setter) s)
    ===
    (\getter _setter s -> getter s)

isoVL_sound2_proof :: ()
isoVL_sound2_proof =
    (\getter setter b -> reviewVL (isoVL getter setter) b)
    ===
    (\_getter setter b -> setter b)
```

<h3 id="t:PrismVL">PrismVL</h3>

The encoding of `Prism` in two `Functor` form is not trivial:

```haskell
type PrismVL s t a b =
    forall f g. (CostrongSum g, Functor f, Pointed f) => OpticVL g f s t a b
```

`Pointed f` is not a surprise (if you read [my post about affine traversal](http://oleg.fi/gists/posts/2017-03-20-affine-traversal.html));
the `CostrongSum` might be. It's a type inspired by another [r6research blog post](http://r6research.livejournal.com/28338.html)
([haskell-cafe post](https://mail.haskell.org/pipermail/haskell-cafe/2015-November/122357.html), and [Gershom B. reply](https://mail.haskell.org/pipermail/haskell-cafe/2015-December/122378.html)).
which let's us to undo `StrongSum`'s `distRight` (i.e. `point` from `Pointed`).

```haskell
class Functor f => CostrongSum f where
    codistRight :: f (Either a b) -> Either a (f b)

instance CostrongSum (Const r) where
    codistRight (Const r) = Right (Const r)

instance CostrongSum Identity where
    codistRight = either Left (Right . Identity) . runIdentity
```

Now we have the right tools to define the `prismVL` constructor:

```haskell
prismVL :: (b -> t) -> (s -> Either t a) -> PrismVL s t a b
prismVL setter getter gafb gs =
     either point (fmap setter . gafb) (codistRight (getter <$> gs))
```

The second of `PrismVL` operations is `previewVL`:

```haskell
previewEVL :: OpticVL Identity (Either a) s t a b -> s -> Either t a
previewEVL o s = swapE (o (Left . runIdentity) (Identity s))
```

And again we can write completeness and soundness expressions:

```haskell
prismVL_complete :: PrismVL s t a b -> PrismVL s t a b
prismVL_complete p = prismVL (reviewVL p) (previewEVL p)
```

```haskell
prismVL_sound1_proof :: ()
prismVL_sound1_proof =
    (\getter setter s -> previewEVL (prismVL setter getter) s)
    ===
    (\getter _setter s -> getter s)

prismVL_sound2_proof :: ()
prismVL_sound2_proof =
    (\getter setter b -> reviewVL (prismVL setter getter) b)
    ===
    (\_getter setter b -> setter b)
```

I can conclude with a bare definition of `LensVL`, leaving
implementing `lensVL` as an exercise for the reader:

```haskell
type LensVL s t a b =
    forall f g. (CostrongProduct g, Functor f) => OpticVL g f s t a b
```

where yet another new non-standard class is:

```haskell
class Functor f => CostrongProduct f where
    distPair :: f (a, b) -> (a, f b)
```

or you can just use `g ~ Identity`.

Appendix: Some optics
---------------------

```haskell
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = dimap (maybe (Left ()) Right) (either (const Nothing) Just) . right'
```

Appendix: Instances
-------------------

<h3 id="instances-tagged">Tagged</h3>

```haskell
instance Profunctor Tagged where
    dimap _ g (Tagged b) = Tagged (g b)

instance Choice Tagged where
    right' (Tagged b) = Tagged (Right b)

instance Bifunctor Tagged where
    bimap _ g (Tagged b) = Tagged (g b)

instance Closed Tagged where
    closed (Tagged b) = Tagged (const b)

instance Semigroupal Tagged where
    mult (Tagged a) (Tagged b) = Tagged (a, b)

instance Monoidal Tagged where
    unit = Tagged ()
```

<h3>Function, (->)</h3>

```haskell
instance Profunctor (->) where
    dimap f g p = g . p . f

instance Choice (->) where
    right' f = either Left (Right . f)

instance Strong (->) where
    first' f (a, c) = (f a, c)

instance Closed (->) where
    closed f xa x = f (xa x)

instance Traversing1 (->) where
    wander1 f g s = runIdentity (f (Identity . g) s)

instance Traversing (->) where
    wander f g s = runIdentity (f (Identity . g) s)

instance Mapping (->) where
    roam f g s = runIdentity (f (Identity . g) s)

instance Semigroupal (->) where
    mult = bimap

instance Monoidal (->) where
    unit = id

instance Cochoice (->) where
    unright f = go . Right where go = either (go . Left) id . f

instance Costrong (->) where
    unfirst f a = b where (b, d) = f (a, d)
```

<h3 id="instances-star">Star</h3>

```haskell
instance Functor f => Profunctor (Star f) where
    dimap f g (Star p) = Star (fmap g . p . f)

-- | definition using firstTraversing would require Apply constraint
instance Functor f => Strong (Star f) where
    first' (Star p) = Star $ (\(a,c) -> fmap (,c) (p a))

instance (Functor f, Pointed f) => Choice (Star f) where
    right' (Star p) = Star (either (point . Left) (fmap Right . p))

instance Apply f => Traversing1 (Star f) where
    wander1 f (Star p) = Star (f p)

instance (Applicative f, Apply f, Pointed f) => Traversing (Star f) where
    wander f (Star p) = Star (f p)

instance Distributive f => Closed (Star f) where
    closed (Star afb) = Star (\xa -> distribute (\x -> afb (xa x)))

-- | We /could/ define `StarCo f a b = StarCo (a -> Co f b)`
-- to use only `Representable` constraint
instance (Apply f, Pointed f, Applicative f, Distributive f) => Mapping (Star f) where
    roam f (Star p) = Star (f p)

instance Apply f => Semigroupal (Star f) where
    mult (Star f) (Star g) = Star (\(x, y) -> (,) <$> f x <.> g y)

instance (Apply f, Applicative f) => Monoidal (Star f) where
    unit = Star (\_ -> pure ())
```

<h3 id="instances-forget">Forget</h3>

```haskell
instance Profunctor (Forget r) where
    dimap f _ (Forget p) = Forget (p . f)

instance Strong (Forget r) where
    first' (Forget p) = Forget (p . fst)

instance Bicontravariant (Forget r) where
    cimap f _ (Forget p) = Forget (p . f)

-- | We could use `Default` with `def` here
-- Then we should require that if `r` is `Monoid`, then `def = mempty`.
instance Monoid r => Choice (Forget r) where
    right' (Forget p) =  Forget (either (const mempty) p)

instance Semigroup r => Traversing1 (Forget r) where
    wander1 f (Forget p) = Forget (getConst . f (Const . p))

instance (Semigroup r, Monoid r) => Traversing (Forget r) where
    wander f (Forget p) = Forget (getConst . f (Const . p))

instance Semigroup r => Semigroupal (Forget r) where
    mult (Forget p) (Forget q) = Forget (\(x, y) -> p x <> q y)

instance (Semigroup r, Monoid r) => Monoidal (Forget r) where
    unit = Forget (\_ -> mempty)
```

<h3 id="instances-forgetm">ForgetM</h3>

```haskell
instance Profunctor (ForgetM r) where
    dimap f _ (ForgetM p) = ForgetM (p . f)

instance Bicontravariant (ForgetM r) where
    cimap f _ (ForgetM p) = ForgetM (p . f)

instance Choice (ForgetM r) where
    right' (ForgetM p) = ForgetM (either (const Nothing) p)

instance Strong (ForgetM r) where
    first' (ForgetM p) = ForgetM (p . fst)
```

<h3 id="instances-forgete">ForgetE</h3>

is a `Strong` `Choice`.

```haskell
instance Profunctor (ForgetE r) where
    dimap f g (ForgetE p) = ForgetE (first g . p . f)

instance Choice (ForgetE r) where
    right' (ForgetE p) = ForgetE (unassocE . fmap p)

unassocE :: Either a (Either b c) -> Either (Either a b) c
unassocE (Left a)          = Left (Left a)
unassocE (Right (Left b))  = Left (Right b)
unassocE (Right (Right c)) = Right c

instance Strong (ForgetE r) where
    first' (ForgetE p) = ForgetE (\(a,c) -> first (,c) (p a))

instance Costrong (ForgetE r) where
    unfirst (ForgetE f) =
       ForgetE (first fst . f . (, error "Costrong ForgetE"))
```

<h3>Either and pair</h3>

```haskell
instance Bifunctor Either where
    bimap f _ (Left x)  = Left (f x)
    bimap _ g (Right y) = Right (g y)

instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)
```

<h3 id="instances-re">Re</h3>

```haskell
instance Cochoice p => Choice (Re p s t) where
    right' (Re p) = Re (p . unright)

instance Costrong p => Strong (Re p s t) where
    first' (Re p) = Re (p . unfirst)

instance Choice p => Cochoice (Re p s t) where
    unright (Re p) = Re (p . right')

instance Strong p => Costrong (Re p s t) where
    unfirst (Re p) = Re (p . first')
```

<h3 id="instances-indexed">Indexed</h3>

```haskell
instance Profunctor p => Profunctor (Indexed p i) where
    dimap f g (Indexed p) = Indexed (dimap (fmap f) g p)

instance Strong p => Strong (Indexed p i) where
    first' (Indexed p) = Indexed (lmap unassoc (first' p))

unassoc :: (a,(b,c)) -> ((a,b),c)
unassoc (a,(b,c)) = ((a,b),c)

instance Choice p => Choice (Indexed p i) where
    left' (Indexed p) = Indexed $
        lmap (\(i, e) -> first (i,) e) (left' p)

instance Traversing1 p => Traversing1 (Indexed p i) where
    wander1 f (Indexed p) = Indexed $
         wander1 (\g (i, s) -> f (curry g i) s) p

instance Traversing p => Traversing (Indexed p i) where
    wander f (Indexed p) = Indexed $
         wander (\g (i, s) -> f (curry g i) s) p
```

<h3 id="instances-zipping">Zipping</h3>

```haskell
instance Profunctor Zipping where
    dimap f g (Zipping p) = Zipping (\x y -> g (p (f x) (f y)))

instance Closed Zipping where
    closed (Zipping p) = Zipping (\f g x -> p (f x) (g x))

instance Choice Zipping where
    right' (Zipping p) = Zipping (\x y -> p <$> x <*> y)

instance Strong Zipping where
    first' (Zipping p) = Zipping (\(x, c) (y, _) -> (p x y, c))

instance Semigroupal Zipping where
    mult (Zipping p) (Zipping q) = Zipping (\(a,b) (c,d) -> (p a c, q b d))

instance Monoidal Zipping where
    unit = Zipping (\_ _ -> ())
```

<h3 id="instances-shop">Shop</h3>

```haskell
instance Profunctor (Shop x y) where
    dimap f g (Shop getter setter) = Shop
        { shopGetter = getter . f
        , shopSetter = \a y -> g (setter (f a) y)
        }

instance Strong (Shop x y) where
    first' (Shop getter setter) = Shop
        { shopGetter = getter . fst
        , shopSetter = \(a, c) y -> (setter a y, c)
        }
```

<h3 id="instances-orphans">Orphans</h3>

```haskell
instance Pointed V2 where
    point = pure

instance Representable f => Pointed (Co f) where
    point x = Co (tabulate (const x))
```

Appendix: Auxiliary functions and types
---------------------------------------

<h3 id="v:roamMap'">roamMap'</h3>

Implementation by David Feuer from [`profunctors` #50](https://github.com/ekmett/profunctors/pull/50).

```haskell
roamMap' :: Profunctor p
         => (forall f. Functor f => p a b -> p (f a) (f b))  -- ^ map'
         -> (forall f. (Distributive f, Applicative f)
                     => (a -> f b) -> s -> f t)
         -> p a b -> p s t
roamMap' m f = dimap (\s -> Bar $ \afb -> f afb s) lent . m
  where
    lent :: Bar t a a -> t
    lent m = runIdentity (runBar m Identity)

newtype Bar t b a = Bar
   { runBar :: forall f. (Distributive f, Applicative f)
            => (a -> f b) -> f t
   }
   deriving Functor
```

<h3 id="t:WrappedApplicative">WrappedApplicative</h3>

Used to implement `collectOf` without `Pointed` and `Apply` constraints.

```haskell
newtype WrappedApplicative f a =
    WrapApplicative { unwrapApplicative :: f a }
    deriving Functor

instance Applicative f => Pointed (WrappedApplicative f) where
    point = WrapApplicative . pure

instance Applicative f => Apply (WrappedApplicative f) where
    WrapApplicative f <.> WrapApplicative x = WrapApplicative (f <*> x)

instance Applicative f => Applicative (WrappedApplicative f) where
    pure = point
    (<*>) = (<.>)

instance Distributive f => Distributive (WrappedApplicative f) where
    collect f = WrapApplicative . collect (unwrapApplicative . f)
```

Appendix: Changes
-----------------

- *2017-04-19:* Reimplemented `setting` using a version from [Mainline Profunctor Heirarchy for Optics](http://r6research.livejournal.com/27476.html).
- *2017-04-19:* Added [van Laarhoven encoding](#van-laarhoven-encoding) section.
- *2017-04-19:* Added [Inverting Prism](#inverting-prism) section.

---

Leave comments in [`/r/haskell` thread](https://www.reddit.com/r/haskell/comments/6627nn/glassery_an_optics_zoo/)

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
λ> :l glassery.lhs
```
fetch the source from
[https://gist.github.com/phadej/c32503efd3274e83196d549eaae28a1a](https://gist.github.com/phadej/c32503efd3274e83196d549eaae28a1a)
