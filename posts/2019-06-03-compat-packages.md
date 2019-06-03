---
title: "Compatibility packages"
author: Oleg Grenrus
tags: engineering
---

Supporting wide (version) ranges of dependencies is a common problem in software
engineering. In particular, supporting many major GHC versions is sometimes tricky.
In my opinion it's not because Haskell-the-language changes, very few
extensions are essential for library-writing[^extensions].
A tricky part is the changes in the so called boot libraries: `base`, `transformers`...
Luckily, there is a collection of *compatibility packages*, which smooth
the cross-GHC development experience.

[^extensions]: Using `BlockArguments` or similar syntax-only-extension,
if completely fine. But I don't think using them is alone a good reason
to not supporting older GHCs. Also, I don't think that *dropping* support for
older GHC support only because it's old, is not a good enough reason.

Compatibility packages are symptom of a greater problems: non-reinstallable
`ghc` (and `base`). That will hopefully change soon: reinstallable `lib:ghc`
is mentioned on
[GHC-8.8.1 page in a planned section](https://gitlab.haskell.org/ghc/ghc/wikis/status/ghc-8.8.1#build-system-and-miscellaneous-changes).

[^hadrian]: Also Hadrian becoming the default build system. Anyway, soon.

Another, somewhat related problem is orphan instances. Sometimes instances are
just forgotten, and as upgrading a proper home package is tricky one, a module with
orphan instances is the best one can do.  Or maybe the reason is as simple as
that neither type-class nor data-type defining package can[^can-depend] depend on other.

[^can-depend]: Of course they can, but in addition to being social problem,
  it's also technical challenge: Completely linearising dependency graph into a
chain is non desirable, though *data-type* package depend on *type-class*
packages may still offer enough slack in the dependency graph.

As some of these compatibility packages define orphan instances, it would be
good that people used these packages instead of defining their own orphans.
They are easy to update, if something is still missing.[^instances]

[^instances]: There is an opinion that "lawless" classes like `Arbitrary`,
  `Binary`, `FromJSON` should not exist, but rather we should use explicit
values. I think that opinion is wrong, at the very least too extreme.  Having
explicit values and combinators is good, we can have both. These kind of
type-classes enable generic programming and things like `servant`: they let
compiler write code for you. But this topic is better discussed in a separate
blog post.

In this post I'll discuss some compatibility packages, I'm aware of.
I'll also mention few extras.

Acknowledgments: [Ryan Scott](https://ryanglscott.github.io/about/) maintains
a lot of packages listed below. I cannot appreciate enough all the time and
work put into their maintenance.

<div id="toc"></div>

base
----

`base` is a core package. Along the years, there were bigger changes like
[Applicative-Monad-Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal),
[Semigroup-Monoid-Proposal](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid),
[MonadFail-Proposal](https://wiki.haskell.org/MonadFail_Proposal),
but also small changes, like addition of new instances
and functions.

<h3>base-compat</h3>

The scope of [`base-compat`](https://hackage.haskell.org/package/base-compat)
is to provide functions available in later versions of base to a wider (older)
range of compilers.

One common at the time problem was `<$>` not in scope, due AMP. This
kind of small problems are easily fixed with

```haskell
import Prelude ()
import Prelude.Compat
```

`base-compat` provides "an alternative" prelude, which I can recommend to use.

Similarly to `<$>`, `base-compat` provides `Semigroup` right out of `Prelude.Compat`.
However due the fact that `base-compat` does not add any orphan instances
(`base-orphans` does).  neither backport any types, `Semigroup` is provided
only with GHC-8.0 / `base-4.9`. Latter is solved by `base-compat-batteries`.

Also `base-compat` (and `base-compat-batteries`) provide a lot of `.Compat` modules

```haskell
import Data.Either.Compat (fromLeft) -- is always there
```

<h3>base-compat-batteries</h3>

[`base-compat-batteries`](https://hackage.haskell.org/package/base-compat-batteries)
provides the same API as the `base-compat` library, but depends on
compatibility packages (such as `semigroups`) to offer a wider (or/and more
complete) support than `base-compat`.

The most is understood by looking at `build-depends` definitions of `base-compat-batteries`:

```cabal
if !impl(ghc >= 7.8)
  build-depends:
    tagged               >= 0.8.5   && < 0.9
if !impl(ghc >= 7.10)
  build-depends:
    nats                 >= 1.1.2   && < 1.2,
    void                 >= 0.7.2   && < 0.8
if !impl(ghc >= 8.0)
  build-depends:
    fail                 >= 4.9.0.0 && < 4.10,
    semigroups           >= 0.18.4  && < 0.20,
    transformers         >= 0.2     && < 0.6,
    transformers-compat  >= 0.6     && < 0.7
if !impl(ghc >= 8.2)
  build-depends:
    bifunctors           >= 5.5.2   && < 5.6
if !impl(ghc >= 8.6)
  build-depends:
    contravariant        >= 1.5     && < 1.6
```

In some cases (like application development), `base-compat-batteries` is a good
choice to build your own prelude on.  Even some projects are often built with
single GHC only, `base-compat-batteries` can help smooth GHC migrations story.
You can adapt to the newer `base` without updating the compiler (and all other
bundled dependencies).

However when developing libraries, you might want be more precise.  Then you
can use the same conditional `build-depends` definitions to incur dependencies
only when `base` is lacking some modules. Note that we use GHC version as a
proxy for `base` version[^semantics]

[^semantics]: Semantically using GHC version as proxy for `base` is imprecise, but it's simpler than currently available alternatives.

- [`bifunctors`](https://hackage.haskell.org/package/bifunctors) provides `Data.Bifunctor`, `Data.Bifoldable` and `Data.Bitraversable`
- [`contravariant`](https://hackage.haskell.org/package/contravariant) provides `Data.Functor.Contravariant`.
- [`fail`](https://hackage.haskell.org/package/fail) provides `Control.Monad.Fail`
- [`nats`](https://hackage.haskell.org/package/nats) provides `Numeric.Natural`
- [`semigroups`](https://hackage.haskell.org/package/semigroups) provides `Data.Semigroup`
- [`tagged`](https://hackage.haskell.org/package/tagged) provides `Data.Proxy`
- [`void`](https://hackage.haskell.org/package/void) provides `Data.Void` module.

Note, that some of this packages provide additional functionality, for example
`semigroups` have `Data.Semigroup.Generic` and `contravariant`
`Data.Functor.Contravariant.Divisible` modules.

<h3>base-orphans</h3>

[`base-orphans`](https://hackage.haskell.org/package/base-orphans) defines
orphan instances that mimic instances available in later versions of base to a
wider (older) range of compilers.

```haskell
import Data.Orphans ()
```

My personal favourite is 

```haskell
instance Monoid a => Monoid (IO a) where
    mempty = pure mempty
    mappend = liftA2 mappend
```

instance, which is in `base` only since `4.7.0.0`.

A word of warning: sometimes the instance definition changes,
and that cannot be adopted in a library.

<h3>generic-deriving</h3>

[`generic-deriving`](https://hackage.haskell.org/package/generic-deriving)
is the package providing `GHC.Generics` things.

Notably it provides missing `Generic` instances for things in `base`.
If you ever will need `Generic (Down a)`, it's there.

transformers
------------

[`transformers`](https://hackage.haskell.org/package/transformers) is a well
known library for monad transformers (and functors!).

<h3>transformers-compat</h3>

[`transformers-compat`](https://hackage.haskell.org/package/transformers-compat)
backports versions of types from newer transformers.
For example an `ExceptT` transformer.

Note that for example `Data.Functor.Identity` may be in `base`, `transformers`
or `transformers-compat`, so when you do

```haskell
import Data.Functor.Identity
```

it may come from different packages.

<h3>writer-cps-transformers</h3>

[`writer-cps-transformers`](https://hackage.haskell.org/package/writer-cps-transformers)
have become a compatibility package, as since `0.5.6.0` `transformers` itself
provide "stricter" `Writer` monad in `Control.Monad.Trans.Writer.CPS`.

There is also a [`writer-cps-mtl`](https://hackage.haskell.org/package/writer-cps-mtl) package
which provides `mtl` instances.

deepseq
-------

[`deepseq`](https://hackage.haskell.org/package/deepseq) provides methods for
fully evaluating data structures.

<h3>deepseq-generics</h3>

Since `deepseq-1.4.0.0` (bundled with GHC-7.10) the default implementation of
`rnf` uses Generics. Before that

```haskell
instance NFData Foo
```

worked for any type and did force only to WHNF: `rnf x = x `seq` ()`. If your
library define types, and support older than GHC-7.10 compilers, the correct
variant is to define `rnf` explicitly, using
[`deepseq-generics`](https://hackage.haskell.org/package/deepseq-generics)

```haskell
import Control.DeepSeq.Generics (genericRnf)

instance NFData Foo where rnf = genericRnf
```

template-haskell
----------------

*Template Haskell* (TH) is the standard framework for doing type-safe,
compile-time meta programming in the GHC.  There are at least two compat issues
with Template Haskell: defining `Lift` instances, missing `Lift` instances, and
changes in `template-haskell` library itself.

They are solved by three (or four) packages:

- `th-lift` provides `TemplateHaskell` based deriving code for `Lift`
  type-class (and defines `Lift Name`);
- `th-lift-instances` provides instances for small set of core packages,
  and acts as a instance compat module (e.g. provides `Lift ()`, which wasn't in `template-haskell` from the beginning);
- `th-orphans` provides instances for the types in `template-haskell`
- and `th-abstraction` which helps write Template Haskell code.

Note: `th-lift` and `th-lift-instances` only use `TemplateHaskellQuotes`,
therefore they don't need interpreter support.
That means that your library can provide Template Haskell functionality
without itself requiring it. This is important e.g. for GHCJS (template haskell
is _very_ slow), or in cross-compilation (tricky issues), or the simple fact
the system doesn't have dynamic loading
(See *Dyn Libs* in [GHC supported platforms](https://gitlab.haskell.org/ghc/ghc/wikis/platforms))[^th-custom].

[^th-custom]: In my opinion Template Haskell, `build-type: Custom` `Setup.hs`
  scripts, `unsafeCoerce` or `unsafePerformIO` are all in the same group. Every
time you use any, you have to write 200 words essay. Any consequtive use will
require a 100 words longer one.  Please send me those essays, I promise to
publish the best parts.

<h3>th-lift</h3>

`Lift` type-class provides `lift` method, which let's you *lift* expressions
in Template Haskell quotations. It's useful to embed data into final
library

```haskell
myType :: MyType
myType = $(readMyTypeInQ "mytype.txt" >>= lift)
```

[`th-lift`](https://hackage.haskell.org/package/th-lift) provides
`TemplateHaskell` based deriving code for `Lift` type-class.

With GHC-8.0 and later, you can write

```haskell
{-# LANGUAGE DeriveLift #-}

data MyType = ...
  deriving (Lift)
```

however, for older GHCs you can use `th-lift`

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Lift (deriveLift)

data MyType = ...

deriveLift ''MyType
```

<h3>th-lift-instances</h3>

[`th-lift-instances`](https://hackage.haskell.org/package/th-orphans) provides
instances for small set of core packages.

```haskell
import Instances.TH.Lift ()
```

to get e.g. `Lift Text` instance.

<h3>th-orphans</h3>

[`th-orphans`](https://hackage.haskell.org/package/th-orphans) provides
instances for `template-haskell` types, in particular `Ord` and `Lift`.
This package is useful when you *write* Template Haskell code,
not so when you use it.

```haskell
import Language.Haskell.TH.Instances ()
```

<h3>th-abstraction</h3>

[`th-abstraction`](https://hackage.haskell.org/package/th-abstraction)
is not precisely a compat package, but it normalizes variations in the
interface for inspecting datatype information via Template Haskell so that
packages can use a single, easier to use informational datatype while
supporting many versions of Template Haskell.

If you can write your TH code using `th-abstraction` interface, it's way
simpler than all CPP involved with raw `template-haskell` usage.

binary
------

[`binary`](https://hackage.haskell.org/package/binary) provides
binary serialisation. There are various alternatives[^binary-alternatives], but
`binary` is bundled with GHC, so if you don't have special requirements it's
good enough default choice. The benefit of `binary` is that there **is** a lot
of support, many packages provide `Binary` instances for their types.

[^binary-alternatives]: Check [`serialise`](https://hackage.haskell.org/package/serialise)
which is also fast, but also uses [CBOR](https://cbor.io/) format. That is useful,
as the binary representation is self-descriptive, at least to some degree.

<h3>binary-orphans</h3>

[`binary-orphans`](https://hackage.haskell.org/package/binary-orphans)
provides instances defined in later versions of `binary` package.
For example it provides `MonadFail Get` instance, which was
the main motivation for the creation of the package.

```haskell
import Data.Binary.Orphans ()
```

<h3>binary-instances</h3>

[`binary-instances`](https://hackage.haskell.org/package/binary-instances)
scope is broader, it provides `Binary` instances for types in some
popular packages: `time`, `vector`, `aeson` et cetera.

```haskell
import Data.Binary.Instances ()
```

bytestring
----------

[`bytestring`](https://hackage.haskell.org/package/bytestring) provides
an immutable byte string type (pinned memory).

<h3>bytestring-builder</h3>

[`bytestring-builder`](https://hackage.haskell.org/package/bytestring-builder)
provides `Data.ByteString.Builder` and `Data.ByteString.Short` modules
for old `bytestring`.

The commonly needed missing piece is `Data.ByteString.Lazy.toStrict` and
`fromStrict`. They are not needed that often though, so I have written
it inline when needed.
Compat `toStrict` is actually is more efficient than it looks[^to-strict]:

[^to-strict]: Compare
[`toStrict` from 0.10.8.2](https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Lazy.html#toStrict)
with [`concat` from 0.9.2.1](https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/src/Data-ByteString.html#concat).

```haskell
import qualified Data.ByteString as BS
import qualified Data.ByteString as LBS

fromStrict :: BS.ByteString  -> LBS.ByteString
toStrict   :: LBS.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0,10,0)
fromStrict = LBS.fromStrict
toStrict   = LBS.toStrict
#else
fromStrict bs = LBS.fromChunks [bs]
toStrict lbs  = BS.concat LBS.toChunks lbs  -- good enough.
#endif
```

time
----

[`time`](https://hackage.haskell.org/package/time) provides most time related
functionality you need. In `1.9` version it got a lot of nice things,
including `CalendarDiffDays` to represent "calendar" day difference.

<h3>time-compat</h3>

[`time-compat`](https://hackage.haskell.org/package/time-compat) shims
the `time` package to it's current latest version. This is my recent
experiment. *All* `time` modules have a `.Compat` version.
This means that you can change to dependency definition

```diff
- build-depends: time         >=... && <...
+ build-depends: time-compat ^>=1.9.2
```

and imports 

```diff
- import Data.Time
+ import Data.Time.Compat
```

and get reasonably[^time-reason] compatible behaviour across different GHC (7.0
... 8.8) and time (1.2 ... 1.9.2) versions.

[^time-reason]: For example changing instance implementation is impossible,
therefore parsing and formatting behaves as in underlying `time` versions.
In any case, more compatible then bare `Data.Time`.

GHC functionality
-----------------

We already mention `th-lift` which provide *Template Haskell* based
functionality to derive `Lift` type-class instances. There are other
classes to be derived.

<h3>deriving-compat</h3>

[`deriving-compat`](https://hackage.haskell.org/package/deriving-compat)
provides Template Haskell functions that mimic deriving extensions that were
introduced or modified in recent versions of GHC. 

Particularly, it provides a way to mimic `DerivingVia` which is only in GHC-8.6+.
The `deriving-compat` is ugly, but if you are stuck with GHC-8.2 or GHC-8.4,
that's an improvement: you can prepare code to be `DerivingVia` ready.

So if real `DerivingVia` looks like

```haskell
{-# LANGUAGE DerivingVia #-}

data V2 a = V2 a a
  deriving (Functor)
  deriving (Semigroup, Monoid) via (Ap V2 a)

instance Applicative V2 where ...
```

the `deriving-compat` way looks like

```haskell
{-# LANGUAGE TemplateHaskell, ... #-}

import Data.Deriving.Via

data V2 a = V2 a a
  deriving (Functor)

instance Applicative V2 where ...

deriveVia [t| forall a. Semigroup a => Semigroup (V2 a) `Via` (Ap V2 a) |]
deriveVia [t| forall a. Monoid    a => Monoid    (V2 a) `Via` (Ap V2 a) |]
```

It feels like `StandaloneDeriving` with all pros and cons of it.

QuickCheck
----------

[`QuickCheck`](https://hackage.haskell.org/package/QuickCheck) is a well known
library for random testing of program properties. It's usage
relies on `Arbitrary` instances for various types.

<h3>quickcheck-instances</h3>

The goal of [`quickcheck-instances`](https://hackage.haskell.org/package/quickcheck-instances)
is to supply `QuickCheck` instances for types provided by the Haskell Platform.
Also to keep `QuickCheck` dependency light, and also CPP free,
`quickcheck-instances` provides instances for types like `Natural` or
`NonEmpty`.

```haskell
import Test.QuickCheck.Instances ()
```

hashable
--------

[`hashable](https://hackage.haskell.org/package/hashable) defines a class,
`Hashable`, for types that can be converted to a hash value. 

<h3>hashable-time</h3>

[`hashable-time`](https;//hackage.haskell.org/package/hashable-time) is
a small package providing `Hashable` instances for types from `time`.

If you want to use `unordered-containers` with `time` types

```
import Data.Hashable.Time ()
```

to get missing instances.

My mistake
----------

I have to mention one personal mistake:
[`aeson-compat`](https://hackage.haskell.org/package/aeson-compat).
[`aeson`](https://hackage.haskell.org/package/aeson) is not a GHC bundled boot package, and
upgrading its version shouldn't be a problem. Creating `aeson-compat` felt like a
good idea back then, but currently I'd recommend to just use as recent `aeson`
as you need. I'd also advice against creating any compatibility packages
for any other non-boot libraries, it's quite pointless.

In general, I don't see point sticking to the too old versions of non-boot
dependencies. Virtually no-one uses the low ends of dependency ranges, at least
based by amount of incorrect lower-bounds I find during my own
experiments[^experiments]. *Stackage* and *nixpkgs* add some friction into the
equation. Allowing versions of dependencies in a current *Stackage LTS* is a
kind thing to do, but if you need newer version, then at least I don't feel bad
myself about putting higher lower bound.

[^experiments]: I actually compile a lot packages sweeping their entire
  dependency-range. My `~/cabal/.store` have grown to 175G in a past month, and
it contains e.g. 20 variants of `text-1.2.3.1` for a single compiler version.
Often lower bounds have bit-rotten, but sometimes there are also "interesting"
interactions in the middle of dependency ranges.

Future thoughts: text
---------------------

It's quite likely that [`text`](https://hackage.haskell.org/package/text) will
adopt UTF-8 as the internal representation of `Text`. My gut feeling says that
the change won't be huge issue for the most of the users. But for example for
the `aeson` it will be: it would be silly not to exploit UTF-8 representation.
`attoparsec` should need adaptation as well.  However, `text` is bundled with
GHC, and though it shouldn't be a problem to upgrade (as only `mtl`, `parsec`
and `Cabal` depend on it, not `ghc` itself), it's hard to predict what subtle
problems there might be.  `text-utf8` has an [*old* fork of
`aeson`](https://github.com/text-utf8/aeson), but it pretends there's only new
`text-utf8`. So there is a lot of (compatibility) work to do for someone who
cares about the details!


Final words
-----------

Library features are rarely the problem preventing wide GHC support windows.
Unfortunately the compatibility story grew up organically, so the naming is
inconsistent, which hurts discoverability of these compatibility packages.
However, I think it's very great, given it's somewhat uncoordinated and voluntary effort.
It's possible to write code which works for at least GHC 7.4 ... GHC-8.6.
GHC-7.4.1 was released in 2012, seven years ago.
We should measure support windows in years, not major versions.
And from this point-of-view Haskell is definitely suitable for an enterprise development! 

Libraries will keep changing, and it's good to think about compatibility a
bit too. Luckily, there is prior art in Haskell eco-system to learn from!
