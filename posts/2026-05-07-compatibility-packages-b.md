---
title: "Compatibility packages in 2026"
author: Oleg Grenrus
tags: engineering
---

Seven years ago I wrote a post about compatibility packages.
It is now highly outdated, so let us revisit the matter.

Recently there have been a small push towards [reinstallable base](https://github.com/well-typed/reinstallable-base).
While it's still far from being a thing, it made me remember that using
`impl(ghc >= 7.9)`-like conditionals to guard against different `base` versions is semantically
wrong.

Also recently there is increasing? interest in [MicroHs](https://github.com/augustss/MicroHs).
While I personally don't care about that compiler, I realized that I can make its users experience
at least slightly nicer though still somewhat ignoring MicroHs existence.

An example
----------

Luckily there is a solution, and it was around for a long time: *automatic flags*.
Here is a complete example:

```cabal
flag base-ge-4-16
  description: @base >=4.16@ (GHC-9.2)
  default:     True
  manual:      False

flag base-ge-4-17
  description: @base >=4.17@ (GHC-9.4)
  default:     True
  manual:      False

library
  ...
  build-depends:
      base    >=4.12.0.0 && <4.23

  if !flag(base-ge-4-16)
    build-depends: OneTuple >=0.4.2 && <0.5

  if !flag(base-ge-4-17)
    build-depends: data-array-byte >=0.1.0.1 && <0.2

  if flag(base-ge-4-16)
    build-depends: base >=4.16
  else
    build-depends: base <4.16

  if flag(base-ge-4-17)
    build-depends: base >=4.17
  else
    build-depends: base <4.17
```

First we declare the flags. I chose to use a naming scheme reminiscing the condition:
`base-ge-4-17` for `base >=4.17`.

Then we make the flag selection *deterministic*:

```cabal
  if flag(base-ge-4-17)
    build-depends: base >=4.17
  else
    build-depends: base <4.17
```

Because the `base >=4.17` and `base <4.17` conditions are disjoint,
there is at most one valid flag assignment for any given install plan
which includes `base` - but because `base` is a direct dependency it has to be in the install plan.
This is why I call such flag deterministic[^deterministic].

[^deterministic]: In my opinion all automatic flags have to be deterministic.
For example having *automatic* `debug` flag is IMO just wrong. There are also a
bit edge cases related to `pkg-config`, and I think it's a "bug" in
`.cabal` format that we cannot make `pkg-config` based library version selection deterministic.

And finally we use the flag value to add a conditional dependency:

```cabal
  if !flag(base-ge-4-17)
    build-depends: data-array-byte >=0.1.0.1 && <0.2
```

Previously I would written

```cabal
  if !impl(ghc >=9.4)
     build-depends: data-array-byte >=0.1.0.1 && <0.2
```

but as I mentioned in an introduction that is semantically wrong.
In this case `Data.Array.Byte` module is introduced in `base-4.17`,
which just happen to be available in GHC-9.4.
In the future there might not be one-to-one correspondence between
(major) GHC and `base` versions.

Moving to use automatic flags removes the direct mention of GHC.
This also (hopefully) helps MicroHS users: we don't need to edit

```diff
-  if !impl(ghc >=9.4)
+  if !impl(ghc >=9.4) && !impl(mhs)
```

as there are no direct mention of compilers.
The library compatibility conditions are expressed using library version vocabulary.

Low-level tools for high level concept
--------------------------------------

It is worth mentioning that the three parts:
defining the flag, making flag selection deterministic and using the flag
value as a condition is indirect way to say something like

```
if !depends(base >=4.17)
  build-depends: data-array-byte >=0.1.0.1 && <0.2
```

In other words we use "low-level" tools to express a high level concept.

Maybe some future version of `.cabal` format would include the high-level way directly. 
However, the low-level "desugaring" makes it impossible to scrutinize flag selection
on indirect dependencies, e.g. we do add dependency to `base`

```cabal
  if flag(base-ge-4-17)
    build-depends: base >=4.17

  else
    build-depends: base <4.17
```

Viewing it from that perspective if a consturct like `depends(base >=4.17)` is
added to `.cabal` format, it should also add a constraint for install plan to
include `base`, though not necessarily adding it direct dependency. That way
the conditional will be deterministic.  But such implicit dependency might feel
unnatural.

Conclusion
----------

I already rewrote `impl(ghc)` conditionals to use automatic flags in few
packages, and will continue to do that as I'm doing other maintenance tasks.

It seems that `OneTuple` and `data-array-byte` are the only few relevant
compatibility packages at the moment (using GHC 9);
there were a lot of compatibility packages in the last decade (`tagged`, `nats`,
`void`, `fail`, `semigroups`, `bifunctors`, `contravariant`,
`bifunctor-classes-compat`, `type-equality`, `foldable1-classes-compat`),
but if you don't need to support very old `base`s & GHCs, we don't need to depend on them for their compatibility shims anymore.

The library part of compatibility story is relatively good, even without having
higher level construct like `if depends (lib >= x.y)` construct.  However, the
compatibility of language level constructs is lacking.  There is no way to ask
in `.cabal` file whether compiler support `DeriveGeneric` or `TemplateHaskell`.
We can *require* these extensions, but we cannot ask whether they exist at all.
Neither we can differentiate between different versions. Is compiler's
`ImpredicativeTypes` "broken" or not, does `LambdaCase` include `\cases` etc.
Some part of me wishes the MicroHs a great success, so those issues become more
pressing and eventually solved. Solved in some other ways than maintainers
hardcoding compiler versions in the package definitions.
