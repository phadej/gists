-------------------------------
title: hashable arch native
author: Oleg Grenrus
-------------------------------

In `hashable-1.4.5.0` I made it use a XXH3 algorithm for hashing byte arrays.
The version `1.4.5.0` and `1.4.6.0` backlashed, as I enabled `-march=native`
by default, and that causes distribution issues. Version `1.4.7.0` doesn't
enable `-march=native` by default.

This by default leaves some performance on the table, e.g. a quick benchmark comparison on my machine (`model name: AMD Ryzen Threadripper 2950X 16-Core Processor`) gives

```plain
Benchmark              without   with            
hash/Text/strict/11    1.481e-8  1.289e-8 -12.95%
hash/Text/strict/128   0.319e-7  0.263e-7 -17.73%
hash/Text/strict/2^20  2.220e-4  1.252e-4 -43.61%
hash/Text/strict/40    1.934e-8  1.714e-8 -11.37%
hash/Text/strict/5     1.194e-8  0.995e-8 -16.64%
hash/Text/strict/512   0.778e-7  0.649e-7 -16.62%
hash/Text/strict/8     1.215e-8  0.983e-8 -19.09%
Geometric mean         0.810e-7  0.644e-7 -20.47%
```

i.e. the new default is 15% slower for small inputs (which is probably the use case for `hashable`), and it gets worse for larger ones.

https://hackage.haskell.org/package/xxhash-ffi-0.3/xxhash-ffi.cabal doesn't give any control to the user, specifically; there's also a bit of non-determinism because the `pkg-config` flag is automatic - you may not notice which version you use, having `libxxhash-dev` installed is rare, but it may happen. (So if you  have `package xxhash-ffi` `cc-options: -march=native`, it might be not used, if you forget to force off the `pkg-config` flag).

architecture selection and chip optimizations
---------------------------------------------

Which made me wonder, **how much this kind of very low-level performance optimisation we leave on the table** when we only care about running binaries locally (e.g. tests; but also benchmarks).

For example, [`popCount`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Bits.html#v:popCount) is relatively common, https://hackage-search.serokell.io/?q=popCount says 1948 matches across 196 packages; and includes things like `vector-algorithms` which one would hope to be fast.
[`countLeadingZeros`](https://hackage-search.serokell.io/?q=countLeadingZeros) is also common with 541 matches across 114 packages (and 532 matches across 82 package for `countTrailingZeros` including `unordered-containers`).

To get the `popcnt` operation you need to enable `msse4.2`, to get `lzcnt` instead of `bsr` you need to enable `-mbmi2`.
`popCount` fallback is a loop, it's slow (I was thinking about that when I wrote `splitmix` in 2017; however there `popCount` is not used in hot path; except if you split a lot... like you do when using `QuickCheck's` `Gen`... hopefully doesn't matter... does it?).
[This StackOverflow](https://stackoverflow.com/a/43443701) answer says that there is no fallback from `lzcnt` to `bsr`, but maybe it's `LZCNT == (31 - BSR)` as accepted answer says. I'm not an expert in x86 ISA; nor I want to be writing Haskell; I hope there was some good reason to introduce LZCNT, and it's worth using when it exists.

I don't think many people add

```
package *
  ghc-options: -msse4.2 -mavx -mbmi -mbmi2
```

to their `cabal.project.local` files. Does it matter? I hope that shouldn't make anything worse (except the portability).

There are few small issues with code generation like
https://gitlab.haskell.org/ghc/ghc/-/issues/25019 or https://gitlab.haskell.org/ghc/ghc/-/issues/24989, I'm sure these will be fixed soon.

However, I'm not so optimistic about bigger issues like adding `arch=native` and also `-mtune=...`; as far as I understand, architecture flags tell compiler that it can or cannot use some instructions, where `mtune` is an *optimization* flag. Even if some instruction is supported by a chip, it doesn't mean it's fast (but maybe it's more relevant for SIMD stuff). That's knowledge I hope compiler to know better than me.

Or even bigger ones to decide whether `-march=native -mtune=native` should be default. Arguably, e.g. GCC and Clang produce very portable binaries by default, but at least they have convenient enough ways to tune binaries for local execution too.

text
----

This low-level instruction business is surprisingly common. E.g. [`text`](https://hackage.haskell.org/package/text) uses [`simdutf`](https://github.com/simdutf/simdutf) except your `text` probably doesn't because GHC ships `text` without `simdutf` (as currently around GHC-9.10.1 time).
The `text` doesn't suffer from `-march=native` issue like hashable, at least partially because of the above. I'm not sure how the things work there, it looks like `simdutf` compiles code for various processors:

```c
#define SIMDUTF_TARGET_HASWELL SIMDUTF_TARGET_REGION("avx2,bmi,pclmul,lzcnt")
#define SIMDUTF_TARGET_WESTMERE SIMDUTF_TARGET_REGION("sse4.2,pclmul")
```

and then uses dynamic dispatch. Or maybe the `sse4.2` is just so common nowadays, that the few rare people who compile `text` themselves don't run into portability issues. (GHC only enables sse2 for Haskell code

`text` also has some non-simdutf code too [as e.g. the issue about avx512 detection](https://github.com/haskell/text/pull/566) highlights; and that uses dynamic dispatch as far as I can tell. (What's the cost of dynamic dispatch? I doubt it's free, and when the operations are small it might show, does it?)

Given all that, I think that it won't hurt if one could compile `text` so there aren't runtime ISA-detection (so things can be tuned for your chip), even if the default were to do a runtime dispatch.
(e.g. if we had that, there would be an immediate workaround for above avx512 detection issue: explicitly turn it off).
And again, it would be nice if GHC and `cabal-install` had convenient ways to enable for-local-execution optimisations (and for bundled libraries like `text` it's almost impossible nowadays, due no good way to force their re-installation, [Cabal#8702](https://github.com/haskell/cabal/issues/8702) is a related issue).

containers
----------

[`containers`](https://hackage.haskell.org/package/containers) also use `popCount`, `countLeadingZeros`; but I bet that it's always used with the portable configuration in practice, as it's bundled with GHC, similarly as `text` library is. (The `IntSet` / `IntMap` implementation uses bit level operations, it might benefit from using better instructions when available).

Conclusion
----------

It feels that the end of compilation pipeline - the assembly generation - isn't getting as much attention as it could[^10years]. Sure, these improvements would only decrease run times constant factors only. On the other hand, if we could get 2-3% improvements in hot loops without source code changes, why not get these?

[^10years]: I noticed [the "GHC gets divide-by-constant optimisation, closing my 10 years old ticket about 10x slowdowns" post](https://www.reddit.com/r/haskell/comments/1dmlefx/ghc_gets_dividebyconstant_optimisation_closing_my/) on Reddit yesterday. Fun coincidence.

I'm biased, (not only) as maintainer of `hashable` I would like to see [`CLMUL`](https://gitlab.haskell.org/ghc/ghc/-/issues/24075) instruction, and `AESENC` would be nice to play with. But if I the 99.9% used default would rely on their software fallbacks rather than fast silicon implementation, I bet there won't be anything interesting to discover.

And it would be nice to have CPP macros to reflect whether GHC will generate POPCNT, LZCNT, CLMUL, AESENC instruction or their fallbacks. E.g. in `hashable` it's worth using `AESENC` for mixing if it's a silicon one, otherwise it's probably better to stick to a different but simpler fallback.
(Maybe we already has these: [GHC#7554](https://gitlab.haskell.org/ghc/ghc/-/issues/7554) suggests so, maybe it's only a documentation issue [GHC#25021](https://gitlab.haskell.org/ghc/ghc/-/issues/25021)).
