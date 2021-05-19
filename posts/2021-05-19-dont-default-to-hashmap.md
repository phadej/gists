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
which has `Hashable` instance but does not have `Ord` one. [^concurrent]
Let us discard such cases and only consider situations when key type
has both `Ord` and `Hashable` instances.

[^concurrent]: And concurrent one as in [`unique` package](https://hackage.haskell.org/package/unique)
cannot have `Ord` instance.

The thing we often forget to consider is *unordered* in `unordered-containers`.
`toList :: HashMap k v -> [(k, v)]`[^toList] doesn't not respect the equality.
The order in the resulting list is arbitrary, it depends on how `HashMap` is constructed.

[^toList]: If you rely (directly or indirectly) on `HashMap.toList` or
  `HashSet.toList` where the order may leak, they are likely wrong data
structures.  It is ok to use `toList` if we remove order-dependency later for
example by sorting the data at the end or folding with a commutative operation.

We should be aware that we *trade stability for performance*,
it is a trade-off, not a pure win.

And not only on that, hash functions provided by `hashable` **are not stable**.
They depend on a library version, operating system, architecture.
The reason is simple: we should be able to tweak hash functions
to whatever is the best for performance related properties (speed and collision resistance is own tradeoff already).

Today released `hashable-1.3.2.0` optionally could randomise the hash seed on startup of final executable.
If `random-init-seed` flag is enabled, the initial seed is not a
compile-time constant but a value initialized on the first use.
I added the `random-init-seed` flag to help find issues when we
(unintentionally) depend on (non-existent) stability of the hash function.
I immediately [found a bug in the `lucid` test suite](https://github.com/chrisdone/lucid/commit/242ec1748a9f1bf43d07bb37a608a7a223e830dd).
Even the test-suite took care of different orders already, there was a typo in test case.

I **do not recommend** enabling that flag in production, only in tests.

Why `lucid` uses `HashMap` (for HTML tag attributes) instead of `Map`.
*I actually don't know.*
These attribute dictionaries are usually of "human size", at which point the performance difference
doesn't show up. (Maybe a `Trie Text` would be the best option. I'm not sure.)
See my [previous blog post on benchmarking discrimination package](https://oleg.fi/gists/posts/2021-01-07-discrimination-benchmarks.html).
`List.nub` *is* faster then `Set` or `HashSet` based `ordNub` and `hashNub`
with small inputs,
even the member check in List has linear complexity. Constant factors matter.
If `lucid` used `Map`, its output would be stable.
Now it's not, attributes can appear in arbitrary order, and
that causes problems, e.g. for [`dhall-docs`](https://github.com/dhall-lang/dhall-haskell/issues/2179).[^dhall]

[^dhall]: I think `dhall-docs` should use e.g. `tagsoup` to reparse the output,
(normalise), and compare it as "tag tree", not as text.  That's not very
convenient, but it is more correct &mdash; even if `lucid` had fully
stable output.

Another example is `aeson`. It is using `HashMap` for the representation of
JSON objects.  I don't have any data whether using `Map` would be worse for
performance in typical cases, I doubt. It might matter when JSON contains an
actual dictionary mapping from keys to values, but "record objects" are small.
Whether key-value mappings would be that big that it mattered, is also not
unknown. We can argue that it would be better in worst case, as there is known
DDoS attacks on HashMaps with deterministic (and weak, such `hashable`) hash
functions.  If we would use stronger hash function, the constant factor will go
up, and then `Map` might be faster and safer by construction.

To conclude, I'm sorry that I broke all of your test-suites with
`hashable-1.3.1.0` release which changed initial seed of the default hash.  On
the other hand, I think it was good to realise how much we rely on something we
shouldn't.  Therefore `hashable-1.3.2.0` introduces the `random-init-seed` flag
which you can use in your tests to find those issues.  (N.B. I don't consider
that flag to be a security feature).  That release also finally mentions in
haddocks the "known but hidden" fact that hash in `hashable` is not stable.
Don't rely on that.

I suggest you default to `Map` when you need an associative container, and only
consider `HashMap` when you actually have a case for it
Benchmark, don't guess, base your decision on data, rerun these benchmark periodically.
