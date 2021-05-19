---
title: Do not default to HashMap
author: Oleg Grenrus
---

There are two widely used associative containers in Haskell:

- `Map` from `containers`
- `HashMap` from `unordered-containers`

`Map` is using `Ord` (total order), and most operations are $\mathcal{O}(\log n)$.
`HashMap` is using `Hashable` (some hash), and most operations are $\mathcal{O}(1)$.
`HashMap` is obviously better!

**It depends**.

Sometimes `HashMap` is the only choice, for example when the keys are [`Unique`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Unique.html#t:Unique)
which has `Hashable` instance but does not have `Ord` one.
Let us discard such cases and only consider situations when key type
has both `Ord` and `Hashable` instances.

The thing we often forget to consider is *unordered* in `unordered-containers`.
`toList :: HashMap k v -> [(k, v)]` doesn't not respect the equality.
The order in the resulting list is arbitrary, it depends on how `HashMap` is constructed.

And not only on that, hash functions provided by `hashable` **are not stable**.
They depend on library version, operating system, architecture
and starting with `hashable-1.3.2.0` optionally it's randomised on each execution
of final executable.

I added the `random-init-seed` flag to help find cases when we
(unintentionally) depend on (non-existent) stability of hash function.
I immediately [caught a bug in `lucid` test suite](https://github.com/chrisdone/lucid/commit/242ec1748a9f1bf43d07bb37a608a7a223e830dd).
Even the test-suite took care of different orders,
there was a typo.

Why `lucid` uses `HashMap` (for HTML tag attributes) instead of `Map`.
I actually don't know.
These maps are usually of "human size", at which point the performance difference
doesn't show up. (Maybe a `Trie Text` would be the best option. I'm not sure.)
See my [previous blog post on benchmarking discrimination package](https://oleg.fi/gists/posts/2021-01-07-discrimination-benchmarks.html).
`List.nub` *is* faster then `Set` or `HashSet` based `ordNub` and `hashNub`
with small inputs,
even the member check in List has linear complexity. Constant factors matter.
If `lucid` used `Map`, its output would be deterministic.
Now it's not, attributes can appear in arbitrary order, and
that causes problems, e.g. for [`dhall-docs`](https://github.com/dhall-lang/dhall-haskell/issues/2179).

Another example is `aeson`. It is using `HashMap` for the representation of
JSON objects.  I don't have any data whether using `Map` would be worse for
performance in typical cases, I doubt.  (It matters when JSON contains an
actual dictionary mapping from keys to values, but "record objects" are
small.). We can argue that it would be better in worst case, as there is known
DDoS attacks on HashMaps with deterministic (and weak, such `hashable`) hash
functions.  If we would use stronger hash function, the constant factor will go
up, and then `Map` might be faster and safer by construction.

To conclude, I'm sorry (not really) that I broke all of your test-suites with
`hashable-1.3.1.0` release which changed initial seed of the default hash.
I think it was good to realise how much we rely on something we shouldn't.
Therefore `hashable-1.3.2.0` introduces the `random-init-seed` flag which you can
use in your tests to find those issues.
(I don't consider that flag to be a security feature).
That release also finally mentions in haddocks the "known but hidden" fact
that hash in `hashable` is not stable. Don't rely on it.

I suggest you default to `Map` when you need an associative container,
and only consider `HashMap` when you actually have a case for it
(benchmark, don't guess, base your decision on data).
