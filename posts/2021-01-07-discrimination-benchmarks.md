---
title: Benchmarks of discrimination package
author: Oleg Grenrus
---

I originally posted these
as a [Twitter thread](https://twitter.com/phadej/status/1346439299487641600).
Its point is to illustrate that constant factors matter,
not only the whether it is $\mathcal{O}(n)$, $\mathcal{O}(n \log n)$ or $\mathcal{O}(n^2)$ (though quadratic is quite bad quite soon).

I have been playing with [`discrimination`](https://hackage.haskell.org/package/discrimination) package.

It offers linear-time grouping and sorting.
[`Data.List.nub`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:nub)
vs [`Data.Discrimination.nub`](https://hackage.haskell.org/package/discrimination-0.4/docs/Data-Discrimination.html#v:nub) chart is fun to look at.
(Look at the x-axis: size of input).

Don't use `Data.List.nub.`

![List.nub graph](disc-nub.svg)

---

There is `ordNub` (in many librairies, e.g. [in `Cabal`](https://hackage.haskell.org/package/Cabal-3.2.1.0/docs/Distribution-Utils-Generic.html#v:ordNub)).
And indeed, it is a good alternative.
`Data.Discrimination.nub` is still faster, when n is large enough
(around hundred thousands for Word64 I use in these benchmarks).

![ordNub graph](disc-ordnub.svg)

---

```haskell
hashNub :: (Eq a, Hashable a) => [a] -> [a]
```

performs well too:

![hashNub graph](disc-hashnub.svg)

---

All four variants on the same graph. Even for small n, nub is bad.
ordNub and Discrimination.nub are about the same.
hashNub is fastest.

(I have to find something better to play with than `Word64`)

![four nub variants](disc-nubs.svg)

---

UUID is slightly more intestesting type.

```
data UUID = UUID !Word64 !Word64
  deriving (Eq, Ord, Generic, Show, NFData, Hashable, Grouping, Sorting)
```

Same pattern: `hashNub` is the fastest, `ordNub` becomes slower then
`Discrimination.nub` when there are enough elements.

![nub of UUID](disc-nubsuuid.svg)

---

I was asked about zooming for small $n$.

![nub of UUID](disc-nubsmall-lin.svg)

Because it is hard to see, we can try loglog plot.
Everything looks linear there, but we can see crossover points better.

![nub of UUID](disc-nubsmall-loglog.svg)

---

But what about sorting, you may ask.

It turns out that [`Data.List.sort`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:sort) is quite good,
at least if your lists are less than a million in length.
Comparison with [`Data.Discrimination.sort`](https://hackage.haskell.org/package/discrimination-0.4/docs/Data-Discrimination.html#v:sort):
(I sort UUIDs, for more fun).

![List.sort](disc-sortlist.svg)

---

Making a vector from a list, sorting it (using vector-algorithms)
and converting back to list seems to be a good option too,
(we only copy pointers, copying million pointers is ... cheap).

![Vector sorts](disc-sortvec-806.svg)

---

Something weird happens on GHC-9.0 though.
`discrimination` has the same performance, yet `vector` based degrades.

![Vector sorts](disc-sortvec.svg)

---

Yet, @ekmett thinks there is still plenty of opportunity to make discrimination faster.
In the meantime, I'll add (a variant of) [these benchmarks](https://github.com/ekmett/discrimination/pull/33) to the repository.

