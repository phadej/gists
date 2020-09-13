---
title: VersionRange normalisation
author: Oleg Grenrus
---

Recently I have been working on improving the `VersionRange` normalisation in `Cabal`.
The version ranges in `build-depends` can be written as version predicates (`>=2` or `<1`) combined arbitrarily with `&&` (intersection) or `||` (union).
The syntax is natural, yet various calculations are more easily done on a *canonical form*, rather then on the (surface) syntax tree.
Also, Hackage shows the canonical version range for dependencies on package pages.

One canonical form for version ranges is a *union of non-overlapping non-empty continuous intervals*.
For example, `>=1.2.3.0 && <1.3` is already in the canonical form and can be drawn as

\begin{tikzpicture}
\draw[thick] (0,0) -- (4,0);
\draw[fill=black] (0,0) circle (0.15);
\draw[fill=white] (4,0) circle (0.15);
\draw (5,0) node[right] {\texttt{>=1.2.3.0 \&\& <1.3}};
\end{tikzpicture}

where filled circle represents an inclusive bound, and unfilled is an exclusive bound.
Often enough we need more than one continuos interval,
for example `>=0.3.0.0 && <0.4 || >=0.4.1.0 && <0.6`

\begin{tikzpicture}
\draw[thick] (0,0.5) -- (4,0.5);
\draw[fill=black] (0,0.5) circle (0.15);
\draw[fill=white] (4,0.5) circle (0.15);
\draw (10,0.5) node[right] {\texttt{>=0.3.0.0 \&\& <0.4}};
%
\draw[thick] (5,0) -- (9,0);
\draw[fill=black] (5,0) circle (0.15);
\draw[fill=white] (9,0) circle (0.15);
\draw (10,0) node[right] {\texttt{>=0.4.1.0 \&\& <0.6}};
\end{tikzpicture}

Here and in the following I won't draw intervals "in scale", and only preserve the ordering of the endpoints of the intervals.

Current implementation of the version range normalisation is conceptually
very simple. There is a type `VersionIntervals`

```
data VersionIntervals = VersionIntervals [VersionInterval]
```

which is a list of (continuous) `VersionInterval`s, which
are stored in order and are non-overlapping.
For example, if someone writes `>= 0.2 && <0.4 || >=0.3 && <0.5`,
then the partial intervals are overlapping:

\begin{tikzpicture}
\draw[thick] (0,0.5) -- (4,0.5);
\draw[fill=black] (0,0.5) circle (0.15);
\draw[fill=white] (4,0.5) circle (0.15);
\draw (7,0.5) node[right] {\texttt{>=0.2 \&\& <0.4}};
%
\draw[thick] (2,0) -- (6,0);
\draw[fill=black] (2,0) circle (0.15);
\draw[fill=white] (6,0) circle (0.15);
\draw (7,0) node[right] {\texttt{>=0.3 \&\& <0.5}};
\end{tikzpicture}

and can be simplified (canonicalised, normalised) into

\begin{tikzpicture}
\draw[thick] (0,0.5) -- (6,0.5);
\draw[fill=black] (0,0.5) circle (0.15);
\draw[fill=white] (6,0.5) circle (0.15);
\draw (7,0.5) node[right] {\texttt{>=0.2 \&\& <0.5}};
\end{tikzpicture}

The functions for taking an union and intersection of `VersionIntervals` ([`unionVersionIntervals`](https://hackage.haskell.org/package/Cabal-3.2.0.0/docs/src/Distribution.Types.VersionInterval.html#unionVersionIntervals) and [`intersectVersionIntervals`](https://hackage.haskell.org/package/Cabal-3.2.0.0/docs/src/Distribution.Types.VersionInterval.html#intersectVersionIntervals)) are not simple, as they maintain the whole invariant.

Since [`cabal-version: 2.0`](https://cabal.readthedocs.io/en/3.4/file-format-changelog.html#cabal-version-2-0)
Cabal supports *PVP caret-style version operator*. For example we can write

```cabal
build-depends: text ^>=1.2.3.0
```

To the first approximation this means `>=1.2.3.0 && <1.3`, i.e. since given version and until the next major (exclusively).
Unfortunately the `VersionIntervals` normalisation destroys the use of `^>=` operator.
(Somewhat succint) version range specifications like `^>=0.5.5.0 || ^>=0.6.1.0` are expanded into
`>=0.5.5.0 && <0.6 || >=0.6.1.0 && <0.7`[^middle].
Also the [`--allow-newer`](https://cabal.readthedocs.io/en/3.4/cabal-project.html?highlight=allow-newer#cfg-field-allow-newer)
behavior to relax only caret bounds (i.e. `allow-newer: ^all`) won't work
on version ranges normalised by the current implementation[^argument].
We can try to "infer" back the `^>=` ranges, but that won't be semantically correct.
The `^>=` version ranges should occur in the normalised output only if they were present in the original version range.

[^middle]: Many people would "simplify" the `^>=0.5.5.0 || ^>=0.6.1.0` into `>=0.5.5.0 && <0.7`.
These are not the same, as version `0.6` might not actually be incompatible. For example, if in a big 0.6 refactoring some function was accidentally removed, and only added back in `0.6.1.0`.
Based on my experience with bit-rotten lowerbounds in `build-depends`, I don't think that average maintainers ever test intermediate versions of the dependencies for compatibility.

[^argument]: We can argue how well `allow-newer: ^all` could work if people used `^>=` bounds more.
For `^all` relaxation to work well, the known incompatibilities have to be specified,
i.e. maintainers would need to edit `^>=1.2.3.0` version bounds into `^>=1.2.3.0 && <1.3`, when it's known that `1.3` version in fact doesn't work.
For now I'm very happy that people specify any upper bounds without distinguishing the semantic difference between `^>=1.2.3.0 && <1.3` and `^>=1.2.3.0`.

The current implementation, while correct, is not easily extended.
Maintaining complete `VersionIntervals` invariant at all times is difficult.

A simple, but largely applicable wisdom is that **final invariant doesn't need to be maintained in the transit**.
If we can perform calculations maintaining just some weaker (and more easily maintained) invariant, and only strenghten it at the end with a (cheap) postprocessing pass, the implementation will be simpler to understand, or even possible at all!
One example of this wisdom is the insertion into red-black tree. There we temporarily suspend the *red nodes have black children* invariant, which is recovered later[^challenge].

[^challenge]: As an exercise try to implement red-black trees with a color as an index of GADT. You will be forced to define a new type which doesn't enforce the invariant.
Stephanie Weirich demonstrated that in 2014, if you want to see a solution.

In the rest of the post I will

* Show how to normalise `VersionRange` **without `^>=` ranges** in more simple way, using simpler intermediate invariant, and
* how to **add `^>=` ranges** to that variant.

Normalisation in two steps
--------------------------

Is there a simpler way to normalise `VersionRanges`?
For me it feels that inventing algorithms is something you do some what
accidentally, after the brain reconfigured itself.

Lets look at taking the union and intersection of version intervals,
or in fact any distributive gadget,
without knowing anything more about them.
The union of unions is simple:

$$
(x_1 \lor x_2) \lor (y_1 \lor y_2) =
x_1 \lor x_2 \lor y_1 \lor y_2
$$

but intersection is quadractic:

$$
(x_1 \lor x_2) \land (y_1 \lor y_2) =
(x_1 \land y_1) \lor
(x_1 \land y_2) \lor
(x_2 \land y_1) \lor
(x_2 \land y_2)
$$

(Current `intersectVersionIntervals` carefully examines
the heads of union lists and carefully produces at most $n + m$ intervals).

Adding carets
-------------

Here we will extend the above to work with `^>=` ranges as well.

\begin{tikzpicture}
\usetikzlibrary{snakes}
\draw[thick,snake=zigzag] (0,0.5) -- (4,0.5);
\draw[thick] (4,0.5) -- (6,0.5);
\draw[fill=black] (0,0.5) circle (0.15);
\draw[fill=white] (4,0.5) circle (0.15);
\draw[fill=white] (6,0.5) circle (0.15);
\draw (7,0.5) node[right] {\texttt{\textasciicircum>=0.2 \&\& <0.5}};
\end{tikzpicture}

TBW

Conclusion
----------

TBW
