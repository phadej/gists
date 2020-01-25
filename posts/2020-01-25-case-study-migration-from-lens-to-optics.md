---
title: "Case study: migrating from lens to optics"
author: Oleg Grenrus
tags: lens, optics
---

As you are reading this post, you probably know that there is

- the [`lens`](https://hackage.haskell.org/package/lens) library by Edward Kmett
  et al. which is de facto *optics* library for Haskell.
  It's famous also for its type errors.
- the [`optics`](https://hackage.haskell.org/package/optics) library
  by Adam Gundry, Andres Löh, Andrzej Rybczak and myself uses
  different representation for *optics* (note: slanted *optics* is a concept,
  monospace `optics` is a library name). I recommend reading
  through [the introduction in the official documentation](https://hackage.haskell.org/package/optics-0.2/docs/Optics.html#g:1),
  especially [Comparison with `lens` section](https://hackage.haskell.org/package/optics-0.2/docs/Optics.html#g:4)

Some time ago I commented on Reddit, that there are no real experience reports
about migrating "real world Haskell codebases" from `lens` to `optics`.
So I decided to do an experiment. The repository is a public Futurice
Haskell code monorepository, which I worked on during my time at the Futurice.
The whole codebase is a bit over 70000 lines in 800 files.

One disclaimer is that I do this *for fun* and  *out of curiosity*, in other
words Futurice didn't ask me to do this (fun is subjective).  Another
disclaimer is that my experiences shouldn't not be extrapolated into how easy
or hard this kind of migration is, only that it's possible. I'm way to familiar
with the codebase under "migration", as well as with `lens` and `optics`
libraries.  On the other hand, it turned relatively *easy for me*, and I share
my experiences, so *it would be easier for others*.

<div id="toc"></div>

Why move from lens to optics
----------------------------

Before discussing *how*, let me talk a little about *why*.

There may be different reasons why a team would like to migrate from
`lens` (or no `lens` at all) to `optics`

One **bad reason** is trying to reduce dependency footprint. It won't.
Libraries use `lens`, and that cannot be easily changed.
The HMR consists of dozens of small web services, which use

- [`swagger2`](https://hackage.haskell.org/package/swagger2) to get Swagger-UI
- [`amazonka`](https://hackage.haskell.org/package/amazonka) to use AWS service
- [`gogol`](https://hackage.haskell.org/package/gogol) to operate with G-Suite
- [`Chart`](https://hackage.haskell.org/package/Chart) to draw pretty graphs

All these libraries depend on `lens`. But we can use them with `optics` too,
as I'll show later. And even we wouldn't have libraries with lens interface,
the `lens` is there somewhere in codebases of this scale. The HMR build plan
consists of *over 500 components*, from which about 400 are dependencies from Hackage.
In fact, from this "industrial" point of view, if `microlens` didn't exist.
It's just duplicate code somewhere there. In the dependency closure there are e.g.

```plain
microlens-th-0.4.3.2
microlens-mtl-0.2.0.1
microlens-0.4.11.2
lens-4.17.1
```

There are also multiple implementations of other things, so this
problem is not only with optics.

My current project has about 450 components in the build plan, so this scale is not unique.

On the other hand, one proper reason to use `optics` could be better type
errors.  (I'm too experiences to judge that properly but `optics` at least
tries to produces better errors).  Another complelling reason is
`OverloadedLabels` which *just work* with `optics`. We'll see example of that.
Thanks to [Andrzej Rybczak PR](#https://github.com/GetShopTV/swagger2/pull/200),
`swagger2`  package has optics interface via `OverloadedLabels` (since version 2.5), and it's neat.

futurice-prelude
----------------

HMR has own prelude.
The `futurice-prelude` package is at the bottom of the package graph, in other words
everything else there uses the package and imports `Futurice.Prelude` module.
It's *a very fat* prelude, re-exporting a lot of stuff.
It also has a bit of auxiliary modules, which most of the downstream would need.

Having a module imported by everything else is nice,
especially as this module currently re-exports the following `lens`
definitions:

```haskell
import Control.Lens
       (At (..), Getter, Iso, Iso', Ixed (..), Lens, Lens', LensLike,
       LensLike', Prism, Prism', Traversal, Traversal', folded, from, ifoldMap,
       ifolded, ifor, ifor_, isn't, iso, itoList, itraverse, itraverse_, lazy,
       lens, makeLenses, makePrisms, makeWrapped, over, preview, prism, prism',
       strict, view, (%=), (%~), (&), (.~), (<&>), (?=), (?~), (^.), (^..),
       (^?), _1, _2, _3, _Empty, _Just, _Left, _Nothing, _Right)
```

That's not much, but enough for basic optics usage. We'll change the exports
to use `optics`:

```haskell
import Data.Function          ((&))
import Data.Functor           ((<&>))
import Optics
       (AffineTraversal, AffineTraversal', At (..), Getter, Iso, Iso',
       Ixed (..), Lens, Lens', Optic, Optic', Prism, Prism', Traversal,
       Traversal', castOptic, coerced, folded, ifoldMap, ifolded, ifor, ifor_,
       isn't, iso, itraverse, itraverse_, lens, lensVL, makeLenses, makePrisms,
       over, preview, prism, prism', re, simple, traversalVL, traverseOf,
       traversed, (%), _1, _2, _3, _Empty, _Just, _Left, _Nothing, _Right)
import Optics.Operators       ((%~), (.~), (?~), (^.), (^..), (^?))
import Optics.State.Operators ((%=), (?=))
```

and then continue fixing type errors, as usually with refactoring in Haskell.

Careful reader may notice that operators are not exported
from the main `Optics` module. If you don't like them, it's easier to not import them.
The `&` and `<&>` operators are available directly from modules in `base`,
so we import them from there.

There are few missing or different bits in the `optics` imports:

* The `from` is just [`re`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Re.html#v:re).

* There are no `LensLike`; we won't need it. Instead we'll need `Optic` and
 `Optic'`, as well as new kind `AffineTraversal` and `AffineTraversal'`
  (I have [written about them](http://oleg.fi/gists/posts/2017-03-20-affine-traversal.html)).

* There aren't `Wrapped` type-class in `optics`. We'll replace it with [`coerced`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Iso.html#v:coerced).
  `coerced` is a bit too polymorphic, so we'll need to add type-annotations.
  This is a small drawback. Or we could use named `Iso`s. It depends.

* The `itoList` and `lazy` and `strict` are missing, likely they are easy
  to implement in the `futurice-prelude`, so the downstream modules
  won't even notice. `itoList` is simple application of `ifoldr`

    ```haskell
    itoList :: Optics.FoldableWithIndex i f => f a -> [(i, a)]
    itoList = Optics.ifoldr (\i x xs -> (i,x) : xs) []
    ```

  The story about `FunctorWithIndex`, `FoldableWithIndex` and
  `TraversableWithIndex` is long, but ultimately it would be nice if both
  `optics` and `lens` could use the same type-classes; however it's not that
  straight forward. So now, `lens` and `optics` define their own variants.

- `strict` and `lazy` allow to convert between strict and lazy `Text`s or
  `ByteString`s. We'll do the simple thing, and make isos using `lens`
  type-class. This should be good enough for now.
  Recall, `lens` is still around.

  ```haskell
  strict :: L.Strict l s => Iso' l s
  strict = iso (L.view L.strict) (L.view L.lazy)

  lazy :: L.Strict l s => Iso' s l
  lazy = iso  (L.view L.lazy) (L.view L.strict)
  ```

Similarly to `lazy` and `strict` there is `packed` and `unpacked`,
but migrating those is *very* trivial:

```diff
-import Data.Text.Lens              (packed, unpacked)
+import Data.Text.Optics            (packed, unpacked)
```

**Very important** is to remember to re-export
[`%`, the `optics` composition operator](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Optic.html#v:-37-).

In addition we also export

- [`simple`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Iso.html#v:simple)
   and [`castOptic`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Optic.html#v:castOptic)
  combinators, we'll need them soon.
  See [optics#286 issue](https://github.com/well-typed/optics/issues/286).
  In `optics` the identity optic is not `id`.

- [`lensVL`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Lens.html#v:lensVL)
  and [`traversalVL`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Traversal.html#v:traversalVL)
  which help convert van Laarhoven lenses (from
  external libraries) to
  `Optics` representation.

- [`traverseOf`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Traversal.html#v:traverseOf):
  sometimes we use van Laarhoven lenses directly as traversals, that trick
  doesn't play with `optics`.
  For the similar reason we export
  [`traversed`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Traversal.html#v:traversed):
  `traverse` is not an `Optic`.

One thing, which I would liked to be able to do is to re-export
`lens` module qualified, something like:

```haskell
module Futurice.Prelude (
   ...
   module qualified L
   ) where
```

It would helped with this migration, but also help to re-export `Data.Map`
and `Data.ByteString` etc. that would be bery nice for fat preludes.

This is it, about the prelude. Next I'll list various issues
I run into when trying to make a single service in HMR to compile.
I picked a particular one, which I remember uses `lens` a lot
for business domain stuff.

To conduct this experiment I:

1. Added `optimization: False` to `cabal.project.local` to make compilation faster
2. `cabal build checklist-app:libs`
3. On error `ghcid -c 'cabal repl failing-lib'`, fix errors in that package
4. go to step 2.

Individual issues
-----------------

<h3>Couldn't match type ‘optics-core-0.2:Optics.Internal.Optic.Optic</h3>

This is by far the most common type of errors:

```haskell
• Couldn't match type
    ‘optics-core-0.2:Optics.Internal.Optic.Optic
      optics-core-0.2:Optics.Internal.Optic.Types.A_Lens
      optics-core-0.2:Optics.Internal.Optic.TypeLevel.NoIx
      s0
      t0
      a1
      b1’
    with ‘a0 -> b0’
  Expected type: a0 -> b0
    Actual type: Lens s0 t0 a1 b1
```

The important bit is

```
Expected type: a0 -> b0
  Actual type: Lens s0 t0 a1 b1
```

That means that we need to replace dot operator `.` with
the optics composition combinator [`%`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Internal-Optic.html#v:-37-).
Another variation on the same is

```haskell
Precedence parsing error
    cannot mix ‘.’ [infixr 9] and ‘%’ [infixl 9] in the same
    infix expression
```

which happens when you forgot to change few dots.

<h3>swagger2</h3>

As already mentioned, `swagger2` has `optics` interface;
the needed instances are defined in 
[`Data.Swagger.Optics`](https://hackage.haskell.org/package/swagger2-2.5/docs/Data-Swagger-Optics.html)
module. I imported it into `Orphans` module of `futurice-prelude`,
to make the instances available everywhere.

The most changes are then of following flavor:

```diff
-   & Swagger.type_ ?~ Swagger.SwaggerObject
-   & Swagger.properties .~ InsOrdHashMap.fromList ...
+   & #type ?~ Swagger.SwaggerObject
+   & #properties .~ InsOrdHashMap.fromList ...
```

I honestly just used regexps: `s/& Swagger./\& #/`, and `s/type_/type/`.

In one place I had to add a type annotation,
as the type become ambiguous. This is not necessarily a bad thing.

```diff
+  schema :: Swagger.Referenced Swagger.Schema -> Swagger.Schema
   schema s = mempty
```
That was very straight-forward, as there **is** `optics` support
in the `swagger2` library now. Thanks Andrzej!

<h3>gogol and amazonka and Chart</h3>

Unfortunately `gogol` and `amazonka` don't have `optics` support,
as far as I know. Both libraries
use `lens` to provide getters and setters to Google Cloud and AWS domain types.

For the code which operates with the libraries directly
(which is relatively few modules, API is encapsulated),
I `import qualified Control.Lens as L` and prefix operators with `L.`.

The resulting code is a slight mess, as you could have both `^.` from `optics`,
and `L.^.` from `lens`.
That's not nice even it works.

A solution would be to invest a day
and define a bunch of `LabelOptic` instances for `gogol`, `amazonka` and `Chart` types.
One could drop prefixes at the same time.
I'd probably write some Template Haskell to automate that task
(TH would be more fun than typing all the definitions by hand). Something like:

```haskell
$(convertToLabelOptics (drop 3)
  [ ''cleSummary
  , ''cleConferenceProperties
  , ...
  ]
```

or maybe even reify module contents, look for lenses and do even more magical
thing.  It would be more uniform, than doing it by hand. AWS and GoogleCloud
APIs are huge, and it's hard to know what part you will need next.

`amazonka` and `gogol` libraries are code generated,
so one other option is to hack the generator to produce
an `optics` packages too.

`Chart` is not much different API lens-wise, though smaller.
There the manual creation of `Chart-optics` might be viable.

<h3>percent operator</h3>

In one place the HMR actually used `Rational`, and 
[the `Rational` smart constructor `%`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Ratio.html#v:-37-).
In that module the optics weren't used, so I
went with `hiding ((%))`. Maybe I'd defined `rational` named constructor
if I needed it more.

<h3>lens-aeson</h3>

In one library integrating with peculiar JSON API, Futurice uses
[`lens-aeson`](https://hackage.haskell.org/package/lens-aeson),
luckily there's also
[`aeson-optics`](https://hackage.haskell.org/package/aeson-optics)
so the that issue is fixed by changing the import.

In that same spot we run into other difference between `optics` and `lens`:
`^.` in optics really wants a getter. You cannot give it a `Prism` like
[`_String`](https://hackage.haskell.org/package/aeson-optics-1.1.0.1/docs/Data-Aeson-Optics.html#v:_String).
`lens` behavior was (ab)used to return `mempty` i.e. empty `Text` on
non-match. I think using
[`foldOf`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Fold.html#v:foldOf)
is better, though a combinator requiring
[`AffineFold`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-AffineFold.html#t:AffineFold)
would been even better (i.e. fold resulting at most one value, the non-match could
be still `mempty`, or `def` from `Default`).

Similarly in some places `^?` were used with folds to get the first value.
There [`headOf`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Fold.html#v:headOf)
is semantically more correct.

By the way, we debated whether we should renamed more stuff for `optics`,
but settled to keep the names mostly the same. It really shows
in the migration overall. Many things just work out of the box,
after changing the imports.

<h3>Representable</h3>

`futurice-prelude` defines a following type-class:

```haskell
class Ixed m => Pick m where
    --   ::              Index m -> Lens' m (IxValue m)
    pick :: Functor f => Index m -> LensLike' f m (IxValue m)

instance Eq e => Pick (e -> a) where
    pick e p f = p (f e) <&> \a e' -> if e == e' then a else f e'
    {-# INLINE pick #-}
```

It's useful when working with `Representable` containers,
where `Rep f`, i.e. index is some enumerable type. For example:

```haskell
data PerDayOfWeek a = PWD a a a a a a a
```

which can be indexable (representble?) by
[`DayOfWeek`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#t:DayOfWeek).
One can use `PerDayOfWeek` to aggregate data per day of the week.
I find that quite common need.

One could directly convert that class to use `optics`, but that is
unnecessary.
`optics`'
[`ix`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-At-Core.html#v:ix)
combinator could be stronger than default `AffineTraversal`,
for example a `Lens` for `Representable` containers.
In my opinion this simplifies code, as one uses the same combinator -- `ix` --
to index `Map`s and the types like `PerDayOfWeek`.

```haskell
instance Ixed (PerDayOfWeek a) where
    type IxKind (PerDayOfWeek a) = A_Lens
    ix = lensVL gix
```

where [`gix`](https://hackage.haskell.org/package/vec-0.3/docs/Data-Vec-DataFamily-SpineStrict-Pigeonhole.html#v:gix)
derives the Lens generically.

<h3>Classy lenses</h3>

HMR has also plenty classy lens type-classes, defined manually.
There the identity instance needs to be implemnted as
[`castOptic`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Optic.html#v:castOptic)
[`simple`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Iso.html#v:simple):

```diff
 class HasUUID a where
     uuid :: Lens' a UUID

 instance HasUUID UUID where
-    uuid = id
+    uuid = castOptic simple
```

Another alternative would be to the scratch whole class, and rather define

```haskell
type HasUuid a = LabelOptic "uuid" A_Lens a a UUID UUID
```

and use `OverloadedLabels`. 

<h3>Small usage of lensVL</h3>

In few places HMR uses `lens` interface to libraries, which
don't have an `optics` counterpart yet.
That's where one could selectively sprinkle
[`lensVL`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Lens.html#v:lensVL):

```diff
-name = datatypeInfo (Proxy :: Proxy a) ^. datatypeName . packed
+name = datatypeInfo (Proxy :: Proxy a) ^. lensVL datatypeName % packed
```

(The library in question is [`generics-sop-lens`](https://hackage.haskell.org/package/generics-sop-lens),
and in fact there is an unreleased `optics` variant: [`optics-sop`](https://github.com/well-typed/optics/tree/master/optics-sop))

Another use-case for `lensVL` is when some lenses are defined manually,
directly using their van Laarhoven encoding:

```diff
-worldTasks f (World es ts ls is arc _ _) = f ts <&> ...
+worldTasks = lensVL $ \f (World es ts ls is arc _ _) -> f ts <&> ...
```

Here we move things a bit and insert `lensVL` in one place, thus
avoiding rewriting the actual code of the lens.

<h3>Custom combinators consuming optics</h3>

One may notice that `optics` doesn't have types like `Getting`, `ALens`
or `ATraversal`. That is because `optics` types aren't `RankNTypes`,
so `Lens` works as well as `ALens`. Almost.

You may need to do rewrites similar to the following one:

```diff
-toIdMapOf :: HasKey a => Getting (Endo [a]) s a -> s -> IdMap a
+toIdMapOf :: (HasKey a, Optics.Is k A_Getter)
+                      => Optic' k is s a -> s -> IdMap a
```

This is not nice, but that's what we have in `optics`.
(Not that `Endo [a]` is nice either).

One could write also

```haskell
toIdMapOf :: HasKey a => Getter s a -> s -> IdMap a
```

but that would accept **only** a `Getter` (same for `lens` IIRC),
thus we need to jump through a hoop to make more usable definitions.


<h3>No IndexedFold</h3>

This a thing which **is renamed** in `optics`, indexed variants
are named more shortly, in this case you are looking for `IxFold`.
The indexed optics are well supported in `optics`,
and though they are slightly different in their interface,
I didn't run into show stopper problems.

<h3>withIndex</h3>

There are no `withIndex` in `optics`. In HMR it was used in a following snippet:

```haskell
toMapOf (ifolded % withIndex % alongside (to EnumValue) id % ifolded) im
```

Let's try to understand what happens there.
We fold with index, drop down the index into value, map over the index,
and as we have `(newIndex, value)` we make an indexed fold with the new index.

Why not just remap index directly using
[`reindexed`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Indexed-Core.html#v:reindexed):

```haskell
toMapOf (ifolded %& reindexed EnumValue) im
```

Turns out we don't need `withIndex` here.
One could use migration to `optics` as an opportunity to make a code
review scan of their codebase.

Note the composition-like combinator,
[`%&`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Optic.html#v:-37--38-),
it allows to post-process optics with "optics transformers", which aren't
optics themselves, like `reindexed`.
`%` and `%&` are left associative (`.` is right associative),
so the above `ifolded &% reindexed EnumValue` would work as a part of a bigger
optic too.

<h3>ALens</h3>

In one module the `ALens` type was used, with its 
special operators `#~`, `^#` etc. You sometimes need to take a whole
`ALens` in, if you both `view` and `set`.
With `optics` one can use `Lens` with `.~` and `^.`.

And that's the last noteworthy issue. After a while, everything compiles
and even tests pass on the first run.

Conclusion
----------

I hope that this (almost) "real case study" illustrates some issues which one
could run into when moving to use `optics`, in old or new codebase.

It's worth mentioning that the most tricky issues were inside
the auxiliary libraries. The core domain logic, with types
which had optics build with `makeLenses` require mostly only
changing `.` to `%`.

The biggest obstacle are libraries which have `lens` i.e. van Laarhoven
encoding interfaces. Yet, creating `optics` interface for them
in advance is not insurmountable task.

Whether `optics` is better for your team is to your team to decide,
`optics` are suitable for "real world haskell".
