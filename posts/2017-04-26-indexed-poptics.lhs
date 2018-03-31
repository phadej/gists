---
title: Indexed Profunctor optics
author: Oleg Grenrus
tags: lens
---

This post is a response to the [Edward's tweet](https://twitter.com/kmett/status/854352458855976960):

<blockquote>
Now try to fit all of the indexed and index-preserving variants. ;)
<cite>Edward Kmett</cite>
</blockquote>

Which in turn is a reply to my previous post: [Glassery](./2017-04-18-glassery.html).

First I'll show how we can implement indexed optics using a `newtype Indexed`,
as [`purescricpt-profunctor-lenses`](https://github.com/purescript-contrib/purescript-profunctor-lenses)
(version 3.2.0) does. This approach is already mentioned in *Glassery*,
but I'll also compare it to the `lens` encoding.

The rest of the post is novel, at least I haven't seen such tehnicque applied
to lenses before.
By indexing a profunctor itself (`p i a b`), we regain the flexibility of `lens`
approach ([Section: Indexed profunctor](#indexed-profunctor)).
This approach also scales to so called "coindexing",
so it's possible to extract "coindexes", e.g. reason why or where `Prism` failed
([Section: Coindexed](#coindexed)).

This blog post introduces a type alias with **9** (nine) variables:
```
type IndexedOpticJ p i j k l s t a b =
    p i j a b -> p k l s t
```

---

```haskell
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module IndexedPoptics where

import Control.Applicative (Const (..))
import Control.Lens (TraversableWithIndex (itraverse))
import Data.Bifunctor
import Data.Constraint
import Data.Functor (void)
import Data.Monoid (Endo (..))
import Data.Semigroup (Semigroup (..))
import Data.Pointed
import Data.Profunctor
import Data.Profunctor.Traversing
import Test.HUnit

import qualified Control.Lens as L
```

Contents
--------

<div id="toc"></div>

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)

Newtype
-------

The current way to do indexed optics in profunctor encoding is to use a newtype

```haskell
newtype Indexed p i a b = Indexed { runIndexed :: p (i, a) b }
type Optic        p   s t a b = p a b           -> p s t
type IndexedOptic p i s t a b = Indexed p i a b -> p s t
```

That's the approach taken by
[`purescricpt-profunctor-lenses`](https://github.com/purescript-contrib/purescript-profunctor-lenses)
(version 3.2.0).

Definition of indexed traversal if not complicated, we don't need
new type-classes: the old friend `Traversing` is enough:

```haskell
itraversed :: (TraversableWithIndex i t, Traversing p)
            => IndexedOptic p i (t a) (t b) a b
itraversed (Indexed piab) = wander (itraverse . curry) piab
```

We'll use `itoListOf` in the examples.

```haskell
itoListOf :: IndexedOptic (Forget [(i, a)]) i s s a a
          -> s -> [(i, a)]
itoListOf o = runForget (o (Indexed (Forget (:[]))))
```

The definition is similar to regular `toListOf`

```haskell
toListOf :: Optic (Forget [a]) s s a a
          -> s -> [a]
toListOf o = runForget (o (Forget (:[])))
```

We can combine indexed and regular optics, without problems, using
function composition:

```haskell
newtype_ex2 =
    itoListOf (itraversed . traverse') ["foo", "bar"]
    ~=?
    [(0,'f'),(0,'o'),(0,'o'),(1,'b'),(1,'a'),(1,'r')]

newtype_ex3 =
    itoListOf (traverse' . itraversed) ["foo", "bar"]
    ~=?
    [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]

newtype_ex4 =
    itoListOf itraversed "foobar"
    ~=?
    [(0,'f'),(1,'o'),(2,'o'),(3,'b'),(4,'a'),(5,'r')]
```

Combining two indexed optics directly won't work.
```
itoListOf (itraversed . itraversed) ["foo", "bar"]

<interactive>:77:10: error:
    • Couldn't match type ‘p’ with ‘Indexed p Int’
```

That's because the type of the result optics has two `Indexed`,
but definition of `itoListOf` requires only single one!

```haskell
newtype_ex1 :: ( Traversing p
               , TraversableWithIndex i1 t1
               , TraversableWithIndex i2 t2
               )
            => Indexed (Indexed p i1) i2 a b
            -> p (t1 (t2 a)) (t1 (t2 b))
newtype_ex1 = itraversed . itraversed
```

So we need to define a special combinator for the optic composition:

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

newtype_ex5 =
    itoListOf (icompose (,) itraversed itraversed) ["foo", "bar"]
    ~=?
    [((0,0),'f'),((0,1),'o'),((0,2),'o')
    ,((1,0),'b'),((1,1),'a'),((1,2),'r')
    ]
```

Alternatively we can use an `OpticLike`, which let us combine the indices
in the optic building "pipeline":

```haskell
flattenIndices
    :: Profunctor p
    => (i -> j -> k)
    -> Indexed p k a b
    -> Indexed (Indexed p i) j a b
flattenIndices ijk (Indexed kab) = Indexed (Indexed (lmap f kab))
  where
    f (i, (j, a)) = (ijk i j, a)

newtype_ex6 =
    itoListOf (itraversed . itraversed . flattenIndices (,)) ["foo", "bar"]
    ~=?
    [((0,0),'f'),((0,1),'o'),((0,2),'o')
    ,((1,0),'b'),((1,1),'a'),((1,2),'r')
    ]
```

<h3>Comparison to lens</h3>

In `lens`, if we combine two indexed optics we'll get the latter index:
(profunctor: type-error).

```haskell
lens_ex1 =
    L.itoListOf (L.itraversed . L.itraversed) ["foo", "bar"]
    ~?=
    [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]
```

We should use `icompose` (or `<.>`) to combine indices:

```haskell
lens_ex2 =
    L.itoListOf (L.icompose (,) L.itraversed L.itraversed) ["foo", "bar"]
    ~?=
    [((0,0),'f'),((0,1),'o'),((0,2),'o')
    ,((1,0),'b'),((1,1),'a'),((1,2),'r')]
```

On the other hand, in `lens` indexed optics degrade to regular one
when used by a regular operation:

```haskell
lens_ex3 =
    L.toListOf L.itraversed [1, 2, 3]
    ~=?
    [1,2,3]
```

Profunctor version fails with a type error:

```
toListOf itraversed [1,2,3]

<interactive>:274:25: error:
    • Couldn't match type ‘Forget [a] a a’
                     with ‘Indexed (Forget [a]) Int Integer Integer’
```

We must remove the index explicitly:

```haskell
unindexed :: Profunctor p
          => p a b
          -> Indexed p i a b
unindexed = Indexed . lmap snd

newtype_ex7 =
    toListOf (itraversed . unindexed) "foo"
    ~=?
    "foo"
```

So the Profunctor encoding using `newtype Indexed` is more rigid.
We have to be explicit about the index (or indices). Whether this
is good or bad: depends.

Some might think that being explicit is good. On the other hand, the
implicitness of `lens` is not a problem. If we have type redundancy, forcing a
type of the result; you'll get a type-error if `lens`es are combined with wrong
combinator. A bit later than when building up the optic (and not annotating it
with a type-signature) though.

The `print (itoListOf o s)` examples doesn't have such
redundancy, so the issue may seem bigger than it is.

Indexed profunctor
------------------

<blockquote>
In <code>lens</code>, we can get away with having indexed optics because we have two type
parameters, and when you go to compose two optics with <code>(.)</code>, the profunctor
part automatically selects <code>p = (->)</code> by unification, for the optics that
supply indices, as composing a couple things of the form
<cite>Edward Kmett</cite>
</blockquote>

Or as Matthew Pickering put it: there are two parameters in lens encoding,
`p` and `f`. By varying the `f`, we can change the lens type (`Lens`, `Getter`, `Fold` or `Traversal`)
and by varying `p` we can vary between indexed and regular variants.

That's a good insight. We can add an additional degree of freedom to the profunctor
encoding:

```haskell
type IndexedOpticI p i o s t a b = p i a b -> p o s t
type IndexedOpticI' p i o s a = p i a a -> p o s s
```

The idea is to have index variable on both side of the arrow! Instead
of stacking up `Indexed`, we'll stack up something on top of `o`. The example
will make this more concrete. Let's make an indexed traversal.

We have to introduce a new class `TraversingWithIndex`.

```haskell
class IndexedProfunctor p => TraversingWithIndex p where
    itraversedI :: TraversableWithIndex i t
                => IndexedOpticI p (i, o) o (t a) (t b) a b
    itraversedI = iwanderI itraverse

    iwanderI :: (forall f. Applicative f
                   => (i -> a -> f b)
                   -> (s -> f t))
            -> IndexedOpticI p (i, o) o s t a b
    -- It should be possible to write a default implementation
    -- using itraversedI

    -- Not strictly necessary.
    traversingDictI :: Dict (Traversing (p i))
    -- or we could use Forall from Data.Constraint.Forall
    default traversingDictI :: Traversing (p i) => Dict (Traversing (p i))
    traversingDictI = Dict

class IndexedProfunctor p where
    ilmap :: (i -> j) -> p j a b -> p i a b
```

Note that indexes of `IndexedOptics` are related: `(i, o)` and `o`.
Every `IndexedTraversal` in the chain would add an additional index.
There's an engineering problem: what kind relation would be the most convenient
for a practical usage (should we use `DataKinds` and type-level lists?).
For now we'll use tuples, and keep used language extensions to the minimum.

```haskell
indexed_ex1 :: ( TraversingWithIndex p
               , TraversableWithIndex i1 t1
               , TraversableWithIndex i2 t2
               )
            => p (i2, (i1, o)) a b
            -> p o (t1 (t2 a)) (t1 (t2 b))
indexed_ex1 = itraversedI . itraversedI
```

The next step is to define a profunctor implementing that class. The `Star`
is good template, we only need to add an index:

```haskell
newtype StarI f i a b
    = StarI { runStarI :: i -> a -> f b }
```


another one is a variant of `Forget` which forgets the index as well:

```haskell
newtype ForgetI r i a b = ForgetI { runForgetI :: a -> r }
```

and the one which doesn't:

```haskell
newtype IndexedForget r i a b =
    IndexedForget { runIndexedForget :: i -> a -> r }
```

Using `ForgetI` we can define normal operations,

```haskell
toListOfI :: IndexedOpticI' (ForgetI (Endo [a])) i o s a -> s -> [a]
toListOfI o s = appEndo (foldMapOfI o (Endo . (:)) s) []

foldMapOfI :: IndexedOpticI' (ForgetI r) i o s a -> (a -> r) -> s -> r
foldMapOfI o f = runForgetI (o (ForgetI f))
```

and using `IndexedForget` the indexed variants:

```haskell
itoListOfI :: IndexedOpticI' (IndexedForget (Endo [(i, a)])) i () s a
          -> s -> [(i, a)]
itoListOfI o s = appEndo (ifoldMapOfI o (\i a -> Endo ((i,a):)) s) []

ifoldMapOfI :: IndexedOpticI' (IndexedForget r) i () s a
           -> (i -> a -> r) -> s -> r
ifoldMapOfI o f = runIndexedForget (o (IndexedForget f)) ()
```

Now we can use `toListOfI` on the indexed optic!

```haskell
indexed_ex2 =
    toListOfI (itraversedI . itraversedI) [[1,2],[3,4,5]]
    ~=?
    [1,2,3,4,5]
```

or `itoListOf`, though we get very ugly indexes:

```haskell
indexed_ex3 =
    itoListOfI (itraversedI . itraversedI) [[1,2],[3,4,5]]
    ~=?
    [((0,(0,())),1),((1,(0,())),2),((0,(1,())),3)
    ,((1,(1,())),4),((2,(1,())),5)
    ]
```

It's possible to write a variant `itoListOf` with a type signature
requiring single index:

```haskell
itoListOfI' :: IndexedOpticI' (IndexedForget (Endo [(i, a)])) (i, ()) () s a
          -> s -> [(i, a)]
itoListOfI' o s =
    appEndo (ifoldMapOfI o (\(i, ()) a -> Endo ((i,a):)) s) []
```

As with the newtype variant, it's possible to flatten indices.
In fact `ilmap` is a general index mapping function.

```haskell
flattenIndicesI
    :: IndexedProfunctor p
    => (i -> j -> k)
    -> p (k, z) a b
    -> p (j, (i, z)) a b
flattenIndicesI f = ilmap g where
    g (j, (i, z)) = (f i j, z)

indexed_ex4 =
    itoListOfI' (itraversedI . itraversedI . flattenIndicesI (,))
    ["foo", "bar"]
    ~=?
    [((0,0),'f'),((0,1),'o'),((0,2),'o')
    ,((1,0),'b'),((1,1),'a'),((1,2),'r')
    ]
```

We can compose indexed and regular optics using function composition dot, `.`:

```haskell
indexed_ex5 =
    itoListOfI' (itraversedI . traverse') ["foo", "bar"]
    ~=?
    [(0,'f'),(0,'o'),(0,'o'),(1,'b'),(1,'a'),(1,'r')]

indexed_ex6 =
    itoListOfI' (traverse' . itraversedI) ["foo", "bar"]
    ~=?
    [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]
```

Graceful degradation from indexed to regular optics is possible with profunctor
optics too. The key idea is to make every optic indexed (kind of)!

The engineering challenge here, is to design an algebra for indices.  With
`DataKinds` and type-level lists and type-families to tuples, the API can be
made quite nice (I guess, hopefully there aren't some nastiness for
type-inference).

<h3>A CPS note</h3>

One option to make `IndexedOpticI` elegant, is instead of nesting tuples,
we could use (->) version:

```haskell
class IndexedProfunctor p => TraversingWithIndexC p where
    itraversedC :: (TraversableWithIndex i t)
                => IndexedOpticI p r (i -> r) (t a) (t b) a b
    itraversedC = iwanderC itraverse

    iwanderC :: (forall f. Applicative f
                 => (i -> a -> f b)
                 -> (s -> f t))
             -> IndexedOpticI p r (i -> r) s t a b
```

The combination has a nice type:

```haskell
cps_ex1 :: ( TraversingWithIndexC p
           , TraversableWithIndex i t, TraversableWithIndex i' t'
           )
        => IndexedOpticI p r (i -> i' -> r) (t (t' a)) (t (t' b)) a b
cps_ex1 =  itraversedC . itraversedC
```

Defining operations is not complicated, we have to be just a bit more
clever in the instantiation:

```haskell
itoListOfC :: IndexedOpticI' (IndexedForget [(i, a)]) i (i -> i) s a
           -> s -> [(i, a)]
itoListOfC o = ifoldMapOfC o (\i a -> [(i, a)])

ifoldMapOfC :: IndexedOpticI' (IndexedForget r) i (i -> i) s a
            -> (i -> a -> r) -> s -> r
ifoldMapOfC o f = runIndexedForget (o (IndexedForget f)) id
```

The double-index example may clarify better: the first index of the optic
is instantiated to the "joint" index type, and the latter to the function to
produce the joint index from the actual two.

```haskell
ifoldMapOfC2 :: IndexedOpticI' (IndexedForget r) k (i -> j -> k) s a
             -> (i -> j -> k) -> (k -> a -> r) -> s -> r
ifoldMapOfC2 o ijk f = runIndexedForget (o (IndexedForget f)) ijk
```

And we can fuse the two function arguments above;

```haskell
ifoldMapOfC2' :: IndexedOpticI' (IndexedForget r)
                 (a -> r) (i -> j -> a -> r) s a
              -> (i -> j -> a -> r) -> s -> r
ifoldMapOfC2' o f = runIndexedForget (o (IndexedForget id)) f
```

To write the examples will use a `flattenIndicesC`. *Note:* Now we have
to precompose with it.

```haskell
flattenIndicesC
    :: IndexedProfunctor p
    => (i -> j -> k)
    -> p (i -> j -> z) a b
    -> p (k -> z) a b
flattenIndicesC f = ilmap (\g i j -> g (f i j))
```

And the example:

```haskell
cps_ex2 =
    itoListOfC (flattenIndicesC (,) . itraversedC . itraversedC)
    [[1,2],[3,4,5]]
    ~=?
    [((0,0),1),((0,1),2),((1,0),3),((1,1),4),((1,2),5)]
```

or using `ifoldMapOfC2'`:

```haskell
cps_ex3 =
    ifoldMapOfC2' (itraversedC . itraversedC) (\i j a -> [(i,j,a)])
    [[1,2],[3,4,5]]
    ~=?
    [(0,0,1),(0,1,2),(1,0,3),(1,1,4),(1,2,5)]
```

Thanks to
Tom Ellis
[for mentioning this idea](https://www.reddit.com/r/haskell/comments/67ov21/indexed_profunctor_optics/dgsh5sl/).
and correcting me further.

Coindexed
---------

<blockquote>
Also, there is a notion we don't currently explore in <code>lens</code> (it is incompatible
with the notion of indexed optics) of what I call 'coindexed' optics. You can
think of it as allowing information back in the failing match case. e.g a prism
that returns an error message on failure. When you combine the two features the
problem gets even worse, as one wants to push information from the left side of
the <code>(.)</code> towards the right and the other wants to push information from the
right side of the </code>(.)</code> towards the left and they need to conspire to produce the
right types now with 2-3 sources of information about what it should be!
<cite>Edward Kmett</cite>
</blockquote>

*That's easy*.

<blockquote>
"We can solve any problem by introducing an extra level of indirection."
<cite>David J. Wheeler</cite>
</blockquote>

In our case: type variables. In this section we'll use a monstrous type
mentioned in the introduction:

```haskell
type IndexedOpticJ p i j k l s t a b =
    p i j a b -> p k l s t
```

one index pair for the contravariant argument (as previously), and one more
pair for covariant.

Writing operations using this encoding isn't different than previously.
We make some concrete profunctor, use optic to transform it, and then use the
result:

```haskell
ifoldMapOfJ :: IndexedOpticJ (IndexedForgetJ r) (i, ()) () () k s t a b
            -> (i -> a -> r) -> s -> Either k r
ifoldMapOfJ o f =
    runIndexedForgetJ (o (IndexedForgetJ $ \(i, ()) -> Right . f i)) ()

newtype IndexedForgetJ r i j a b =
    IndexedForgetJ { runIndexedForgetJ :: i -> a -> Either j r }
```

That's not exactly a `ifoldMapOf` variant, as it can fail with a description!

Let's define few constructors to play with examples. We will use bare `String`
for errors, in real library you probably want something more structured.

```haskell
type Err = String
```

To define prisms we need a variant of `Choice`, not that
we

```haskell
class IndexedProfunctorJ p => ChoiceWithIndexJ p where
    irightJ :: IndexedOpticJ p i j i (Either Err j)
                               (Either c a) (Either c b) a b

instance ChoiceWithIndexJ (IndexedForgetJ r) where
    irightJ (IndexedForgetJ p) =
        IndexedForgetJ $ \i eca -> case fmap (p i) eca of
            Right (Right r) -> Right r
            Right (Left j)  -> Left (Right j)
            Left _c         -> Left (Left "right' failed")

class IndexedProfunctorJ p where
    idimapJ :: (i -> j) -> (k -> l)
            -> IndexedOpticJ p j k i l a b a b

    ilmapJ :: (i -> j)
          -> IndexedOpticJ p j k i k a b a b
    ilmapJ f = idimapJ f id

instance IndexedProfunctorJ (IndexedForgetJ r) where
    idimapJ f g (IndexedForgetJ p)
        = IndexedForgetJ $ \i -> first g . p (f i)
```

And a `Traversing` variant. Let's make examples interesting
by making `IndexedFOrgetJ` instance "fail", if the `Traversal` is empty:

```haskell
class ChoiceWithIndexJ p => TraversingWithIndexJ p where
    itraversedJ :: TraversableWithIndex i t
                => IndexedOpticJ p (i, j) k j (Either Err k)
                                   (t a) (t b) a b
    itraversedJ = iwanderJ itraverse

    iwanderJ :: (forall f. Applicative f
                   => (i -> a -> f b)
                   -> (s -> f t))
             -> p (i, j) k a b -> p j (Either Err k) s t

instance Semigroup r => TraversingWithIndexJ (IndexedForgetJ r) where
    iwanderJ f (IndexedForgetJ p) =
        IndexedForgetJ $ \j s -> runE2 $ getConst $
            f (\i a -> Const $ E2 $ first Right $ p (i, j) a ) s
```

We have to define auxiliary type to select the right error when traversing:

```haskell
newtype E2 a b = E2 { runE2 :: Either (Either Err a) b }

instance Semigroup b => Monoid (E2 a b) where
    mempty = E2 (Left (Left "Empty Fold"))
    mappend (E2 (Right a)) (E2 (Right b)) = E2 (Right (a <> b))
    mappend x@(E2 Right{}) _ = x
    mappend _ x@(E2 Right{}) = x
    -- make inner errors more important!
    mappend x@(E2 (Left (Right _))) _ = x
    mappend _ x@(E2 (Left (Right _))) = x
    mappend x _ = x
```

<h3>Examples</h3>

It's time for examples. The "good" cases work as before:

```haskell
coindexed_ex1 =
    ifoldMapOfJ (irightJ . idimapJ ((),) id) (,) (Right 'a')
    ~=?
    Right ((), 'a')

coindexed_ex2 =
    ifoldMapOfJ itraversedJ (\i x -> [(i, x)]) "foobar"
    ~=?
    Right [(0,'f'),(1,'o'),(2,'o'),(3,'b'),(4,'a'),(5,'r')]
```

*Note* if `traverse'` zooms into empty `Traversable`, it won't be an error.
But we can make a variant which would make that erroneous as well.

```haskell
coindexed_ex3 =
    ifoldMapOfJ (itraversedJ . traverse') (\i x -> [(i, x)]) ["foo", "bar"]
    ~=?
    Right [(0,'f'),(0,'o'),(0,'o'),(1,'b'),(1,'a'),(1,'r')]

coindexed_ex4 =
    ifoldMapOfJ (traverse' . itraversedJ) (\i x -> [(i, x)]) ["foo", "bar"]
    ~=?
    Right [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]

coindexed_ex5 =
    ifoldMapOfJ (itraversedJ . itraversedJ . idimapJ unassoc id)
    (\i x -> [(i, x)])
    ["foo", "bar"]
    ~=?
    Right [((0,0),'f'),((1,0),'o'),((2,0),'o')
          ,((0,1),'b'),((1,1),'a'),((2,1),'r')]
```

The erroneous cases work as we want: looking at wrong value through
`Prism` gives `Prism` error,
Looking through `Traversal` at empty list gives an empty fold error:

```haskell
coindexed_ex6 =
    ifoldMapOfJ (irightJ . idimapJ ((),) id) (,) (Left True)
    ~=?
    (Left (Left "right' failed") :: Either (Either Err ()) ((), ()))
```

```haskell
coindexed_ex7 =
    ifoldMapOfJ itraversedJ (\i x -> [(i, x)]) ""
    ~=?
    Left (Left "Empty Fold")
```

If we combine a `Traversal` and a `Prism` we'll see how different
erroneous cases work. If all elements of the list are `Right`, we get them.
If some is `Left`, but there's at least one `Right`; we still get `Right`.
If all values are `Left` we get *prism* error; and if the
list is empty we get *an empty fold* error.
Here we could use `idimapJ` to flatten errors, but it's good to see
on which "level" error occurred.

```haskell
coindexed_ex8 =
    ifoldMapOfJ (itraversedJ . irightJ) (\i x -> [(i, x)])
    [Right 'a', Right 'b']
    ~=?
    Right [(0,'a'),(1,'b')]

coindexed_ex9 =
    ifoldMapOfJ (itraversedJ . irightJ) (\i x -> [(i, x)])
    [Right 'a', Left False]
    ~=?
    Right [(0,'a')]

coindexed_exA =
    ifoldMapOfJ (itraversedJ . irightJ) (\i x -> [(i, x)])
    [Left False]
    ~=?
    (Left (Right (Left "right' failed"))
        :: Either (Either Err (Either Err ())) [(Int, ())])

coindexed_exB =
    ifoldMapOfJ (itraversedJ . irightJ) (\i x -> [(i, x)]) []
    ~=?
    (Left (Left "Empty Fold")
        :: Either (Either Err (Either Err ())) [(Int, ())])
```

Conclusion
----------

In this blog post a presented some ideas for indexed profunctor optics,
there's still a lot to design and engineer. The current approach is quite good
IMHO. But *maybe* using indexed profunctor encoding we can make it even
better. The *Coindexed* example is made with a tongue-in-cheek, but maybe there
would be practical use cases for it too.  After all, it degrades into *indexed*
case nicely.

Appendix: Test runner
---------------------

```haskell
runIndexedPopticsExamples :: IO ()
runIndexedPopticsExamples = void $ runTestTT $ TestList $
    [ newtype_ex2, newtype_ex3, newtype_ex4, newtype_ex5, newtype_ex6
    , newtype_ex7
    , lens_ex1, lens_ex2, lens_ex3
    , indexed_ex2, indexed_ex3, indexed_ex4, indexed_ex5, indexed_ex6
    , cps_ex2, cps_ex3
    , coindexed_ex1, coindexed_ex2, coindexed_ex3, coindexed_ex4
    , coindexed_ex5, coindexed_ex6, coindexed_ex7, coindexed_ex8
    , coindexed_ex9, coindexed_exA, coindexed_exB
    ]
```

Appendix: Instances
-------------------

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

instance Traversing p => Traversing (Indexed p i) where
    wander f (Indexed p) = Indexed $
         wander (\g (i, s) -> f (curry g i) s) p
```

<h3>Forget</h3>

```haskell ignore
instance Monoid r => Traversing (Forget r) where
    wander f (Forget p) = Forget (getConst . f (Const . p))
```

<h3>ForgetI</h3>

```haskell
instance Profunctor (ForgetI r i) where
    dimap f _ (ForgetI p) = ForgetI (p . f)

instance Strong (ForgetI r i) where
    first' (ForgetI p) = ForgetI (p . fst)

instance Monoid r => Choice (ForgetI r i ) where
    right' (ForgetI p) =  ForgetI (either (const mempty) p)

instance Monoid r => Traversing (ForgetI r i) where
    wander f (ForgetI p) = ForgetI (getConst . f (Const . p))

instance IndexedProfunctor (ForgetI r) where
    ilmap f (ForgetI p) = ForgetI p

instance Monoid r => TraversingWithIndex (ForgetI r) where
    iwanderI f (ForgetI p) = ForgetI (getConst . f (\_ -> Const . p))
```

<h3>StarI</h3>

```haskell
instance Functor f => Profunctor (StarI f i) where
    dimap f g (StarI p) = StarI $ \i ->
        fmap g . p i . f

instance Functor f => Strong (StarI f i) where
    first' (StarI p) = StarI $ \i (a,c) ->
        fmap (,c) (p i a)

instance (Functor f, Pointed f) => Choice (StarI f i) where
    right' (StarI p) = StarI $ \i ->
        either (point . Left) (fmap Right . p i)

instance (Applicative f, Pointed f) => Traversing (StarI f i) where
    wander f (StarI p) = StarI $ f . p

instance IndexedProfunctor (StarI f) where
    ilmap f (StarI p) = StarI $ p . f

instance (Applicative f, Pointed f) => TraversingWithIndex (StarI f) where
    iwanderI f (StarI p) = StarI $ \o -> f $ \i -> p (i, o)
```

<h3>IndexedForget</h3>

```haskell
instance Profunctor (IndexedForget r i) where
    dimap f _ (IndexedForget p) = IndexedForget (\i -> p i . f)

instance Strong (IndexedForget r i) where
    first' (IndexedForget p) = IndexedForget (\i -> p i . fst)

instance Monoid r => Choice (IndexedForget r i) where
    right' (IndexedForget p) =  IndexedForget (\i -> either (const mempty) (p i))

instance Monoid r => Traversing (IndexedForget r i) where
    wander f (IndexedForget p) = IndexedForget (\i -> getConst . f (Const . p i))

instance IndexedProfunctor (IndexedForget r) where
    ilmap f (IndexedForget p) = IndexedForget (p . f)

instance Monoid r => TraversingWithIndex (IndexedForget r) where
    iwanderI f (IndexedForget p) = IndexedForget $ \o ->
        getConst . f (\i -> Const . p (i, o))

instance Monoid r => TraversingWithIndexC (IndexedForget r) where
    iwanderC f (IndexedForget p) = IndexedForget $ \ij ->
        getConst . f (\i -> Const . p (ij i))
```

<h3 id="intances-indexedforgetj">IndexedForgetJ</h3>

```haskell
instance Profunctor (IndexedForgetJ r i j) where
    dimap f _ (IndexedForgetJ p) =
        IndexedForgetJ (\i  -> p i . f)

instance Strong (IndexedForgetJ r i j) where
    first' (IndexedForgetJ p) =
        IndexedForgetJ (\i -> p i . fst)

instance Monoid r => Choice (IndexedForgetJ r i j) where
    right' (IndexedForgetJ p) =
        IndexedForgetJ (\i -> either (const (Right mempty)) (p i))

instance Monoid r => Traversing (IndexedForgetJ r i j) where
    wander f (IndexedForgetJ p)  = IndexedForgetJ $ \i ->
        getE . getConst . f (Const . E . p i)

newtype E a b = E { getE :: Either a b }

instance Monoid r => Monoid (E a r) where
    mempty = E (Right mempty)
    mappend x@(E (Left _)) _ = x
    mappend _ x@(E (Left _)) = x
    mappend (E (Right a)) (E (Right b)) = E (Right (mappend a b))
```

Appendix: Changes
-----------------

Added a note about CPS version of `IndexedOpticI`, thanks to Tom Ellis.

---

Leave comments in [`/r/haskell` thread](https://www.reddit.com/r/haskell/comments/67ov21/indexed_profunctor_optics/)

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
λ> :l IndexedPoptics.lhs
```
fetch the source from
[https://gist.github.com/phadej/638733a00ccf2c69bff66ad419902ff0](https://gist.github.com/phadej/638733a00ccf2c69bff66ad419902ff0)
