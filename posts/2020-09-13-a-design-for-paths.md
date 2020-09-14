---
title: A design for paths in Cabal
author: Oleg Grenrus
tags: engineering
---

Where a big part of `Cabal` is about interpreting `your-package.cabal`
file, also an important part of it and also `cabal-install` are **filepaths**.
After all, `cabal-install` is a build tool.

Currently (as of `Cabal-3.4`) the type used for all filepath needs is infamous

```
type FilePath = String
```

One can say that all paths in the codebase are *dynamically typed*.
It is very hard to say whether paths are absolute or relative,
and if relative to what.

A solution would be to use
[`path`](https://hackage.haskell.org/package/path) or
[`paths`](https://hackage.haskell.org/package/paths) library.

I like `paths` better, because it is set up to talk about relative paths
to arbitrary roots, not only *absolute paths*.

Still, neither is good enough. Why I say so?
Because `Cabal` and `cabal-install` have to deal with *three kinds of paths*.

1. Abstract paths
2. Paths on the host system
3. Paths on the target system

It is that simple, but `path` is very concretely the second kind,
and `paths` is somewhere in between first and second, but doesn't
let you differentiate them.

Abstract paths
--------------

Abstract paths are the ones written in `your-package.cabal` file.
For example `hs-source-dirs: src/`.
It is not Unix path. It is not Windows path. It is in fact something
which should be interpretable as either, and also path inside tarball archive.
In fact it currently have to be common denominator of it,
which means that backslashes `\`, i.e. Windows filepaths aren't portable,
but I suspect they work *if you build on Windows*.

Just *thinking* about types uncovers a possible bug.

If we had a 

```
-- | An abstract path.
data APath root = ...
```

Then we could enforce format, for example prohibiting some (i.e. all known)
special characters.

*Note:* abstract paths are relative. There might be some *abstract* root,
for example `PackageRoot`, but its interpretation still depends.

The representation of `APath` is not important. It, however, should be some kind of *text*.


Paths on the host system
------------------------

These are the concrete paths on your disk.

```
-- | A path on host (build) system
data HPath root = ...
```

The `HPath` can have different roots as well, for example `CWD` (for current working directory),
`HomeDir` or `Absolute`.
Maybe even talking about `HPath PackageRoot` is meaningful.
My gut feeling says that we rather should be able to provide
an operation to resolve `APath PackageRoot` into `HPath Absolute`,
given a `HPath Absolute` of package root.

Also `directory` operations, i.e. IO operations, like `listDirectory` are only meaningful for `HPath`s.
These are concrete paths.

`HPath`s have to be represented in a systems native way.
It can still be `FilePath` in the first iteration,
but e.g. absolute paths on Windows may start with `\\?\` and use backslashes
as directory separators (c.f. `APath` which probably will look like POSIX path everywhere).

Paths on the target system
--------------------------

The third kind of paths are paths on the *target system*.
While cross-compilation support in `Cabal` is barely existing,
having own type for paths for target system should help that improve.

One example are `YourPackage_Paths` modules.
Currently it contains hardcoded paths to e.g. `data-files` directory
of *installed* package, i.e. somewhere on the *target* system.

While having hardcodes absolute paths in `YourPackage_Paths` is a bad idea nowadays, and the `data-files` discovery should be based on some relative (relocatable, using abstract `APath`s maybe?) system,
having a

```
-- | A path on the target (run) system
data TPath root = ...
```

will at least show where we use (absolute) target system paths.
Perfectly we won't have them anywhere, if that is possible.
But identifying where we have them *now* will help to get rid of them.

Another example is running (custom) `./Setup` or tests or benchmarks.
I hope that we can engineer the code in a way that executables built
for target system won't be callable, but will need to use a runner
wrapper (which we have, but I don't know much about it).
Even a *host = target* (common) system case, where the wrapper is trivial.

*Note:* whether `TPath` is Windows path or POSIX path will depend on run-time information, so the conversion functions will need that bit of information.
You couldn't be able to purely `convert :: APath -> TPath`, we will need to pass an extra context.

Here again, better types should help guide the design process.

Conclusion
----------

These are my current thoughts about how the paths will look in some future version of `Cabal`.
Instead of one `FilePath` (or `Path`) there will be three: `APath`, `HPath` and `TPath`[^naming].

As I write down, this seems so obvious, this is how about paths have to be classified.
Have anyone done something like this before?
Please tell me, so I could learn from your experiences.

[^naming]: Names are subject to change, maybe `SymPath` (for symbolic), `HostPath` and `TargetPath`.
