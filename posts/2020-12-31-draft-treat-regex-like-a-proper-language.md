---
title: "Draft: Treat regular expressions like a proper language..."
author: Oleg Grenrus
---

It's almost a year since I touched [`kleene`](https://hackage.haskell.org/package/kleene)
library, and almost two years since I published it.
Good time to write a little about regular expressions.

I very much like regular expressions, they are truly declarative way to
write down a grammar... when it's expressible in regular expressions.

Matthew Might, Daniel Darais and Daniel Spiewak have written
a paper [*Functional Pearl: Parsing with Derivatives*](https://dl.acm.org/doi/10.1145/2034773.2034801) published in ICFP '11 Proceedings
([pdf](http://matt.might.net/papers/might2011derivatives.pdf))
in which a regular expressions are extended to handle context-free languages.
However, they rely on a *memoization*, and as structures are infinite also
on reference equality. In short, not implementable in idiomatic Haskell. [^derp]

[^derp]: An implementation in [`derp`](https://hackage.haskell.org/package/derp) package uses `unsafePerformIO` and `Data.IORef`.

There's a technique for a subset of context-free languages,
which is in my opinion very elegant, and not painfully slow.
The result is on Hackage: the [`rere`](http://hackage.haskell.org/package/rere) library.

The idea is to treat regular expressions as a proper programming language,
and add a constructions which proper languages should have:
*variables* and *recursion*.

<div id="toc"></div>

Regular expression recap
------------------------

The abstract syntax of a regular expression over is given by the following
"constructors":

- Null regexp, ${\color{red!80!black}\emptyset}$
- Null string, ${\color{red!80!black}\varepsilon}$
- Characters, $\mathtt{\color{green!50!black}a}$ or $\mathtt{\color{green!50!black}b}$
- Concatenation, ${\color{blue}\mathit{r}_{\mathtt{\color{red!50!blue}}1}}{\color{blue}\mathit{r}_2}$
- Alternation, ${\color{blue}\mathit{r}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{r}_2}$
- Kleene star, ${\color{blue}\mathit{r}}^\star$

That can be translated directly into Haskell:

```haskell
data RE a
    = Null
    | Eps
    | Ch Char
    | App RE RE
    | Alt RE RE
    | Star RE
```

In the `rere` implementation, instead of bare `Char` we use a set of characters, `CharSet`,
as recommented by Owens et al in
[*Regular-expression derivatives reexamined*](https://dl.acm.org/doi/10.1017/S0956796808007090)
([pdf](https://www.ccs.neu.edu/home/turon/re-deriv.pdf)).
This makes implementation more efficient, as a common case of character
sets is implicitly taken into account.
We write them in curly braces:
$\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}$.

We can give *declarative* semantics to these constructors.
These will look like typing rules.
As the context we'll have *strings* (list of characters),
and as types the regular expressions which recognize them.
For example a rule for application would look like:

$$
\prftree[r]{\textsc{App}}
{{\color{red!50!blue}\Gamma_1} \vdash {\color{blue}\mathit{r}_1}  }
{{\color{red!50!blue}\Gamma_2} \vdash {\color{blue}\mathit{r}_2}  }
{{\color{red!50!blue}\Gamma_1\Gamma_2} \vdash {\color{blue}\mathit{r}_1}{\color{blue}\mathit{r}_2}  }
$$

and for alternation we'd have two rules:

$$
\begin{aligned}
\prftree[r]{\textsc{Alt}$_1$}
{{\color{red!50!blue}\Gamma} \vdash {\color{blue}\mathit{r}_1} }
{{\color{red!50!blue}\Gamma} \vdash {\color{blue}\mathit{r}_1}\cup{\color{blue}\mathit{r}_2}  }
&&
\prftree[r]{\textsc{Alt}$_2$}
{{\color{red!50!blue}\Gamma} \vdash {\color{blue}\mathit{r}_2} }
{{\color{red!50!blue}\Gamma} \vdash {\color{blue}\mathit{r}_1}\cup{\color{blue}\mathit{r}_2}  }
\end{aligned}
$$

The rules resemble the structure of *non-commutative intuitionistic linear logic*,
if you are into such stuff.
Not only you have to use everything exactly once, you have to use in order;
there aren't any substructural rules, no weakening, no contraction
and even no exchange. I omit the rest of the rules, look them up
(and think how rules for Kleene star would look like TBW `!`).

It's wise to define smart versions of constructors,
which would simplify regular expressions as they are created.
For example, we defined `Semigroup` instance for concatenation,
i.e. `<>` is smart `App`:

```haskell
instance Eq a => Semigroup (RE a) where
    -- Null annihilates
    Null  <> _    = Null
    _     <> Null = Null
    -- Eps is unit of <>
    Eps   <> r    = r
    r     <> Eps  = r
    -- otherwise use App
    r     <> s    = App r s
```

We also define `\/` for smart `Alt`, and `star` for smart `Star`.
We can check that these simplifications are sound, by using the
rules we have, for example `Eps <> r = r` equation is justified by:

$$
\begin{aligned}
\prftree[r]{\textsc{App}}%
{\prftree[r]{\textsc{Eps}}{{\color{red!80!black}\varepsilon} \vdash {\color{red!80!black}\varepsilon} }}%
{\prfsummary{\mathcal{E}}{{\color{red!50!blue}\Gamma} \vdash {\color{blue}\mathit{r}}  }}%
{{\color{red!50!blue}\Gamma} \vdash {\color{red!80!black}\varepsilon}{\color{blue}\mathit{r}}  }%
\qquad=\qquad
\prfsummary{\mathcal{E}}{{\color{red!50!blue}\Gamma} \vdash {\color{blue}\mathit{r}}  }
\end{aligned}
$$

If string $\color{red!50!blue}\Gamma$ is matched by
${\color{red!80!black}\varepsilon}{\color{blue}\mathit{r}}$,
then "the match" can be constructor only in one way, by applying the
$\textsc{App}$ rule.  Therefore $\color{red!50!blue}\Gamma$ is also matched by
bare ${\color{blue}\mathit{r}}$.  If we'd introduce *proof terms*, we'll have a
concrete evidence of the match as terms in this language.

There is a problem: matching using declarative rules is not practical: we have
to guess.  We have to guess whether we should pick left or right branch, or
where we should split string to match concatenated regular expression.  We need
a *syntax directed* approach.  The system consists only of two rules:

$$
\begin{aligned}
\prftree[r]{\textsc{Nullable}}%
{{\color{blue}\mathit{r}} \text{ is nullable}}
{{\color{red!80!black}\varepsilon} \vdash {\color{blue}\mathit{r}}}
&\qquad&
\prftree[r]{\textsc{Derivative}}%
{{\color{red!50!blue}\Gamma} \vdash D_\gamma({\color{blue}\mathit{r}})  }%
{{\color{red!50!blue}\gamma\Gamma} \vdash {\color{blue}\mathit{r}}  }%
\end{aligned}
$$

where we use to operations: decision procedure `nullable :: RE -> Bool`
and mapping $D_c(r)$, `derivative :: Char -> RE -> RE`; the lowercase
$\color{red!50!blue}\gamma$ represents a single character.

`nullable` tells whether the regular expression accepts an empty string,
and is defined as a straight-forward recursive function:

```haskell
nullable Null      = False
nullable Eps       = True
nullable (Ch _)    = False
nullable (App r s) = nullable r && nullable s
nullable (Alt r s) = nullable r || nullable s
nullable (Star _)  = True
```

The Brzozowski derivative is easiest understood by consider
*formal language* regular expressions represent:

$$
D_c(L) = \{ w \mid cw \in L \}
$$

in other words: `derivative c r` matches string `str`, when `r` matches `c : str`.
This gives up matching procedure if we can define `derivative`, and obviously
we can:

```haskell
derivative :: Char -> RE -> RE
derivative _ Null        = Null
derivative _ Eps         = Null
derivative _ (Ch x)
    | c == x             = Eps
    | otherwise          = Null
derivative c (App r s)
    | nullable r         = derivative c s \/ derivative c r <> s
    | otherwise          =                   derivative c r <> s
derivative c (Alt r s)   = derivative c r \/ derivative c s
derivative c r0@(Star r) = derivative c r <> r0
```

We could try to show that the *declarative* and *syntax directed*
systems are equivalent, but I it's already done in the literature
(though probably not in this way & notation).

We can then watch how regular expression evolves when it matches
a string. For example take a regular expression
$(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star$,
which in code looks like

```haskell
ex1 :: RE
ex1 = star (Ch 'a' <> Ch 'b')
```

The following is how the `match ex1 "abab"` proceeds:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}abab} &&\vdash (\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
& \mathtt{\color{red!50!blue}bab} &&\vdash \mathtt{\color{green!50!black}b}(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
& \mathtt{\color{red!50!blue}ab} &&\vdash (\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
& \mathtt{\color{red!50!blue}b} &&\vdash \mathtt{\color{green!50!black}b}(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
&{\color{red!80!black}\varepsilon} &&\vdash (\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star \\
\end{aligned}
$$

We can see that there's a small finite state automaton,
with two states:
initial $\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star$
and secondary $\mathtt{\color{green!50!black}b}(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star$.
This is how `kleene` transforms regular expressions into finite state machines.
There is *character set* optimization from
[*Regular-expression derivatives re-examined*](https://dl.acm.org/doi/10.1017/S0956796808007090)
by Owens, Reppy and Turon, but essentially that how it works:
Try all possible derivatives, and in the process collect all the states and construct a transition function. [^minimal]

[^minimal]: With smart constructors we get small automata, but not minimal. To get minimal ones, `kleene` can compare regular expressions for an equivalence, e.g. concluding that
$\mathtt{\color{green!50!black}a}^\star$ and
$\mathtt{\color{green!50!black}a}^\star\mathtt{\color{green!50!black}a}^\star$ are in fact equivalent.

As the matching process stops at the `nullable` state,
the string is accepted.


Variables and let-expressions
-----------------------------

The first thing we would add to regular-expressions are *let-expressions*.
They alone do not add any matching power, they are prerequisite
to the adding of recursive expressions.

We already used meta-variables in the rules in the previous section,
let-expression would allow to internalize this notion. Declaratively:

$$
\prftree[r]{\textsc{Let}}
{{\color{red!50!blue}\Gamma}\vdash
{\color{blue}\mathit{s}}[{\color{blue}\mathit{x}} \to {\color{blue}\mathit{r}}]}
{{\color{red!50!blue}\Gamma}\vdash
\mathbf{let}\,{\color{blue}\mathit{x}}={\color{blue}\mathit{r}} \,\mathbf{in}\,{\color{blue}\mathit{s}}
}
$$

where ${\color{blue}\mathit{s}}[{\color{blue}\mathit{x}} \to {\color{blue}\mathit{r}}]$
is a notation we use for substitution.

To have let-expressions we need to represent *variables* and be
able to perform substitution. My tool of choice for that
is [`bound`](https://hackage.haskell.org/package/bound) library,
but for the sake of being self-contained we'll define the needed bits inline.
We'll also do a simple variant of bound, essentially it is
de Bruijn indices using polymorphic recursion.

We could use `Maybe` as "natural numbers", but we rather define
an own type:

```haskell
data Var a
    = B     -- ^ bound
    | F a   -- ^ free
  deriving (Eq, Show, Functor, Foldable, Traversable)
```

With this, we can extend regular expressions with let.
First we make it a functor, i.e. change `RE` to `RE a`,
and then also add two new constructors: `Var` and `Let`:

```haskell
data RE a
    = Null
    | Eps
    | Ch Char
    | App (RE a) (RE a)
    | Alt (RE a) (RE a)
    | Star (RE a)

    | Var a
    | Let (RE a) (RE (Var a))
```

In `rere` library, `Let` (and further defined `Fix`) constructors
Also have irrevant `Name` field, which allows to retain
the names and uses them for pretty-printing.
I omit them from the presentation in this blog post.

Now we can write regular expression like
$\mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}{\color{blue}\mathit{r}}$
instead of
$\mathtt{\color{green!50!black}a}^\star \mathtt{\color{green!50!black}a}^\star$:

```haskell
ex2 :: RE Void
ex2 = Let (star (Ch 'a')) (Var B <> Var B)
```

where `Void` tells us that expression is *closed*, i.e. doesn't contain
free variables.

We also need to extend `nullable` and `derivative` to work with new
constructors.
For `nullable` we'll simply pass a function telling whether
variables in context are nullable.
The existing constructors just pass a context around:

```haskell
nullable :: RE Void -> Bool
nullable = nullable' absurd

nullable' :: (a -> Bool) -> RE a -> Bool
nullable' _ Null      = False
nullable' _ Eps       = True
nullable' _ (Ch _)    = False
nullable' f (App r s) = nullable' f r && nullable' f s
nullable' f (Alt r s) = nullable' f r || nullable' f s
nullable' _ (Star _)  = True
```

`Var` and `Let` use and extend respectively:

```haskell
-- Var: look in the context
nullable' f (Var a)   = f a

-- Let: - compute `nullable r`
--      - extend the context
--      - continue with `s`
nullable' f (Let r s) = nullable' (unvar (nullable' f r) f) s
```

The `unvar` is a `maybe` function, but for `Var`:

```haskell
unvar :: r -> (a -> r) -> Var a -> r
unvar b _ B     = b
unvar _ f (F x) = f x
```

The `derivative` needs a bit more thinking.
The idea is similar: add to the context the bits needed for a variables.
The key insight is to add a new `Let` for each existing `Let`
with a `derivative` of existing one.

The context for `derivative` consists from three pieces:

- whether the variable is `nullable` (we need it for `derivative` of `App`)
- The derivative of a variable
- The re-indexed variable

Note: `derivative' :: ... -> RE a -> RE b` changes the index.
The character `c` is bound by enclosing `derivative :: Char -> RE Void -> RE Void`

```haskell
derivative' :: (Eq a, Eq b) => (a -> (Bool, b, b)) -> RE a -> RE b
```

The `derivative'` of variable is simple, just lookup in the context:

```haskell
go f (Var a) = Var (sndOf3 (f a))
```

And `Let` is actually interesting:

```haskell
derivative' f (Let r s)
    = let_ (fmap (trdOf3 . f) r)       -- rename variables in r
    $ let_ (fmap F (derivative' f r))  -- binding for derivative of r
    $ derivative' (\case
        B   -> (nullable' (fstOf3 . f) r,  Var B, F B)
        F x -> bimap (F . F) (F . F) (f x))
    $ s
...
```

As a formula it looks like:

$$
D_c (\mathbf{let}\,{\color{blue}\mathit{x}}={\color{blue}r}\,\mathbf{in}\,{\color{blue}s})
= \begin{aligned}[t]
\mathbf{let}\,&{\color{blue}\mathit{x}}={\color{blue}r}; \\
            \,&{\color{blue}\mathit{x_c}}=D_c({\color{blue}r}) \\
\mathbf{in}&\,D_c({\color{blue}s}) \quad\text{where}\quad D_c({\color{blue}\mathit{x}}) = {\color{blue}\mathit{x_c}}
\end{aligned}
$$

Because ${\color{blue}s}$ didn't depend on derivative of ${\color{blue}r}$
originally, we don't run into problems. That's the reason
why `derivative'` is changing indices.
In the above, we call `derivative'` recursively with `RE (Var a)` argument (one variable: `x`),
but get back a `RE (Var (Var b))` (two variables: `x` and `x_c`).

The careful reader also noticed the smart constructor `let_`, which
does a number of standard rewritings on the fly
(which I explain in a [*Do you have a problem? Write a compiler!* talk](https://oleg.fi/gists/posts/2019-09-26-write-a-compiler.html)).
These are justified by the properties of substitution:

```haskell
-- let-from-let
let x = (let y = a in b) in c
-- ==>
let y = a; x = b in c
```

```haskell
-- inlining of cheap bindings
let x = a in b
-- ==>
b [ x -> a ] -- when a is cheap, i.e. Null, Eps, Ch or Var
```

```haskell
-- used once, special case
let x = a in x
-- ==>
a
```

```haskell
-- unused binding
let x = a in b
-- ==>
b -- when x is unused in b
```

And importantly we do a quick form common-subexpression-elimination (CSE):

```haskell
let x = a in f x a
-- ==>
let x = a in f x x
```

This form of CSE is easy and fast to implement, as
we don't introduce new `let`s, only consider what we already bound
and "try to increase sharing" so to speak.

It's time for examples, recall `ex2` we defined as
$\mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}{\color{blue}\mathit{r}}$:

```haskell
ex2 :: RE Void
ex2 = Let "r" (star (Ch 'a')) (Var B <> Var B)
```

Let's try to match `aaa`:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}aaa} &&\vdash \mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}{\color{blue}\mathit{r}} \\
& \mathtt{\color{red!50!blue}aa} &&\vdash \mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}\cup{\color{blue}\mathit{r}}{\color{blue}\mathit{r}} \\
& \mathtt{\color{red!50!blue}a} &&\vdash \mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}\cup{\color{blue}\mathit{r}}{\color{blue}\mathit{r}} \\
&{\color{red!80!black}\varepsilon} &&\vdash \mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}\cup{\color{blue}\mathit{r}}{\color{blue}\mathit{r}} \\
\end{aligned}
$$

As our smart constructors are quite smart, the automaton stays
in its single state, the union comes from the `derivative` of `App`,
as `r` is nullable, we get `derivative 'a' r \/ derivative 'a' r <> r`.
And as `derivative 'a' r = r`, we don't see additional `let` bindings.

Recursion
---------

Now we are ready for the main topic of the post: *recursion*.
It's matter of adding one more constructor:

```haskell
data RE a
    ...
    | Fix (RE (Var a))
````

`Fix` looks similar to `Let`, except that the variable value
is semantically the expression itself.
We can *unroll* each $\mathbf{fix}$ expression by substituting it into
itself:

$$
\prftree[r]{\textsc{Unroll}}
{{\color{red!50!blue}\Gamma}\vdash
{\color{blue}r}[{\color{blue}x} \to \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{blue}r}]
}
{{\color{red!50!blue}\Gamma}\vdash
\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{blue}r}
}
$$

The `Fix` constructor make Kleene star unnecessary as
${\color{blue}r}^\star$ can be expressed as
$\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup{\color{blue}r}{\color{blue}\mathit{x}}$,
which feels like a very natural definition indeed.
For example `ex1` previously defined using Kleene star as $(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star$ could also be re-defined as
$\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}}$
That looks like

```haskell
ex3 :: RE Void
ex3 = Fix "x" (Eps \/ Ch 'a' <> Ch 'b' <> Var B)
```

in code.

The problem is now the same as with `Let`: How to define `nullable` and `derivative`?
Luckily, we have most of machinery in place since we added `Var` and `Let`.

Nullability of `Fix` relies on a Kleene's theorem to compute the
least fixed point of a monotonic recursive definition,
like in *Parsing with Derivatives*. The structure of `nullable` is
such that if we try to compute the nullability of `Fix` assuming
its nullability is `False`, either it will stay `False`,
or it might turn into `True`.
This is hand-wavy explanation, the important point is that it works.
In code it looks like

```haskell
nullable' :: (a -> Bool) -> RE a -> Bool
...
nullable' f (Fix _ r)   = nullable' (unvar False f) r
```

In other words, we literally assume that the nullability of new binding
is `False`, and see what comes out. We don't need to iterate
more then once, as `False` will flip to `True` right away, or will
never do so.

Similarly the the smart constructor `fix_`
may recognise `Null` fixed-point by substuting `Null`:

```haskell
fix_ :: RE (Var a) -> RE a
fix_ r | (r >>>= unvar Null Var) == Null = Null
...
```

This works also as `Null` is a bottom of language-inclusion lattice
(similarly as `False` is a bottom of a `Bool` lattice).

The extension of `derivative` is again complicated,
but at the end it resembles what happens with `Let`.

As the body $r$ of fix point contains self references $x$,
the derivative of fix point will also be a fix point,
that way when we would need to compute derivative of $x$ we'll use $x_c$.
We should also remember that not all $x$s in the body of of fix point
will be under derivative (right of the `App`, or in `Star`),
so we *need to save it in a `let` binding*. How handy,
that we just introduced those. Schematically it looks like:

$$
D_c (\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{blue}r}[{\color{blue}x}])
= \begin{aligned}[t]
\mathbf{let}\,&{\color{blue}\mathit{x}}= \mathbf{fix}\,{\color{blue}\mathit{x}_1}={\color{blue}r}[{\color{blue}x_1}] \\
\mathbf{in}&\,
\mathbf{fix}\,{\color{blue}\mathit{x}_c} = D_c({\color{blue}r}[{\color{blue}x}])
\quad\text{where}\quad D_c({\color{blue}\mathit{x}}) = {\color{blue}\mathit{x_c}}
\end{aligned}
$$

We'll also use a shorthand notation for let-expression binding to fixed point, like:
$\mathbf{let}\,{\color{blue}\mathit{x}}= \mathbf{fix}\,{\color{blue}\mathit{x}_1}={\color{blue}r}[{\color{blue}x_1}]$.
We will write them more succintly as
$\mathbf{let}\,{\color{blue}\mathit{x}}=_R {\color{blue}r}[{\color{blue}x}]$,
where the $R$ subscript of equals sign indicates that binding is recursive.
We avoid temptation of using $\mathbf{letrec}$ as in a cascade of $\mathbf{let}$ expressions,
individual ones can be fixed points, but we still *cannot* forward reference.

The equation above looks then like

$$
D_c (\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{blue}r}[{\color{blue}x}])
= \begin{aligned}[t]
\mathbf{let}\,&{\color{blue}\mathit{x}}=_R {\color{blue}r}[{\color{blue}x}] \\
\mathbf{in}&\,
\mathbf{fix}\,{\color{blue}\mathit{x}_c} = D_c({\color{blue}r}[{\color{blue}x}])
\quad\text{where}\quad D_c({\color{blue}\mathit{x}}) = {\color{blue}\mathit{x_c}}
\end{aligned}
$$

compare this to the let case, written slightly differently:

$$
D_c (\mathbf{let}\,{\color{blue}\mathit{x}}={\color{blue}r}\,\mathbf{in}\,{\color{blue}s})
= \begin{aligned}[t]
\mathbf{let}\,&{\color{blue}\mathit{x}}={\color{blue}r} \\
\mathbf{in}&\,
\mathbf{let}\,
{\color{blue}\mathit{x_c}}=D_c({\color{blue}r})\,
\mathbf{in}\,
  D_c({\color{blue}s}) \quad\text{where}\quad D_c({\color{blue}\mathit{x}}) = {\color{blue}\mathit{x_c}}
\end{aligned}
$$

And consequently, the implementation in Haskell looks also similar
to the `Let` case:

```haskell
derivative' f r0@(Fix r)
    = let_ (fmap (trdOf3 . f) r0)
    $ fix_
    $ derivative' (\case
        B   -> (nullable' (fstOf3 . f) r0, B, F B)
        F x -> bimap (F . F) (F . F) (f x))
    $ r
```

Let's see how it works. The same example as we did with `ex1`, matching `abab`, executes nicely:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}abab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
& \mathtt{\color{red!50!blue}bab} &&\vdash \mathbf{let}\,{\color{blue}\mathit{x}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}}\,\mathbf{in}\,\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
& \mathtt{\color{red!50!blue}ab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
& \mathtt{\color{red!50!blue}b} &&\vdash \mathbf{let}\,{\color{blue}\mathit{x}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}}\,\mathbf{in}\,\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
&{\color{red!80!black}\varepsilon} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
\end{aligned}
$$

We couldn't wish better outcome, we see the same two-state ping-pong
behavior as we got using Kleene star.

More examples
-------------

The $\mathbf{fix}$ / `Fix` is more powerful construction than Kleene star.
Let's see some examples:

<h3>a<sup>n</sup>b<sup>n</sup></h3>

Probably the simplest non-regular language is some amount of $a$ followed
by the *same* amount of $b$s:

$$
L = \{ a^n b^n \mid n \in \mathbb{N} \}
$$

We can describe that language using our library, thanks to fix point:
$\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}$
(note the blue $\color{blue}x$ in between of green literal symbols)
or as code:

```haskell
ex4 :: RE Void
ex4 = Fix (Eps \/ Ch 'a' <> Var B <> Ch 'b')
```

And we can test it on a string in the language, for example `"aaaabbbb"`:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}aaaabbbb} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}aaabbbb} &&\vdash \mathbf{let}\,{\color{blue}\mathit{x}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}\,\mathbf{in}\,{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}aabbbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}abbbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}bbbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaa}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaa}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}bbb} &&\vdash \mathbf{let}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaab}}}=\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}b}\,\mathbf{in}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaab}}}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}bb} &&\vdash \mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}b} &&\vdash \mathtt{\color{green!50!black}b} \\
&{\color{red!80!black}\varepsilon} &&\vdash {\color{red!80!black}\varepsilon} \\
\end{aligned}
$$

Now things become more interesting. We see how this "regular"
expression not only labels states of the automaton, but is also a stack
of what seems to be [push down automaton](https://en.wikipedia.org/wiki/Pushdown_automaton).

From that trace one can relatively easily see, that if we forget one `b` from
the end, then the "state" `b` isn't `nullable`, so the string won't
be recognized.

<h3>Left recursion</h3>

In the first example we rewrote
$(\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b})^\star$ as
$\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}}$,
but we can also rewrite
it with recursion on the left, in other words as
$\mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}$

```haskell
ex5 :: RE Void
ex5 = Fix "x" (Eps \/ Var B <> Ch 'a' <> Ch 'b')
```

This automaton works too, in fact in some sense better
than the right-recursive one: we can see (as an artifact of variable naming),
that we get the derivatives as output of each step.
We do save the original expression in `let`, but as it is unused
in the result, so it's forgotten:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}abab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}bab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}=\mathtt{\color{green!50!black}b}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}ab} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}ab}}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}ab}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}b} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aba}}}=\mathtt{\color{green!50!black}b}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aba}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
&{\color{red!80!black}\varepsilon} &&\vdash \mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}abab}}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}abab}}}\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b} \\
\end{aligned}
$$

<h3>Arithmetic expression</h3>

Another go to example of context free grammars is arithmetic
expressions. We'll have only digits 0 to 4, for simplicity:

$$
\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}\,{\color{blue}\mathit{d}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}}=\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\end{aligned}
$$

The Haskell variant is not even too obfuscated (though de Bruijn indices start to cause slight inconvenience):

```haskell
ex6 :: RE Void
ex6 = Let (Ch '0' \/ Ch '1' \/ Ch '2' \/ Ch '4')
    $ Let (Var B <> star (Var B))
    $ Fix
    $  Ch '(' <> Var B <> Ch ')'
    \/ Var (F B)
    \/ Var B <> Ch '+' <> Var B
    \/ Var B <> Ch '*' <> Var B
```

And the trace of matching `1*(20+3)` is

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}\,{\color{blue}\mathit{d}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}}=\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\end{aligned} \\
& \mathtt{\color{red!50!blue}\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{n}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}1}}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{e}}=_R\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}={\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}1}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\end{aligned} \\
&&& \vdots \\
& \mathtt{\color{red!50!blue}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{n}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{e}}=_R\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}}}}=_R{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}}}}=_R{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}}}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\end{aligned} \\
& \mathtt{\color{red!50!blue}\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{n}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}3}}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{e}}=_R\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}=_R{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}=_R{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}=_R{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\end{aligned} \\
&{\color{red!80!black}\varepsilon} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{n}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{e}}=_R\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{)}}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}=_R{\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{e}}\end{aligned} \\
\end{aligned}
$$


The final state is nullable:
one alternative of the final fix point is ${\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}$
which itself is nullable because it's an union with ${\color{red!80!black}\varepsilon}$.

The other two alternatives are "continuations" starting with `+` or `*`,
as the arithmetic expression could indeed continue starting with them.

Conversion from context-free grammars
-------------------------------------

Could all context-free languages be expressed in this framework?
Is there some algorithm to rewrite
a usual context-free grammar into the formalism presented here?
The answer is **yes**.

For example the non-ambiguous grammar for arithmetic expressions

$$
\begin{aligned}
{\color{blue}\mathit{digit}} &= \{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\} \\
{\color{blue}\mathit{digits}} &= {\color{blue}\mathit{digit}}\,{\color{blue}\mathit{digit}}^\star \\
{\color{blue}\mathit{term}} &= {\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}} \\
{\color{blue}\mathit{mult}} &= {\color{blue}\mathit{term}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}}\cup{\color{blue}\mathit{term}} \\
{\color{blue}\mathit{expr}} &= {\color{blue}\mathit{mult}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}} \\
\end{aligned}
$$

can be converted to recursive regular expression

$$
\mathbf{let}\,{\color{blue}\mathit{digits}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star\,\mathbf{in}\,\mathbf{fix}\,{\color{blue}\mathit{expr}}=\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}}=_R{\color{blue}\mathit{term}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}}\end{aligned}
$$

And it works, it's fascinating to see how state-expression evolves:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \mathbf{let}\,{\color{blue}\mathit{digits}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star\,\mathbf{in}\,\mathbf{fix}\,{\color{blue}\mathit{expr}}=\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}}=_R{\color{blue}\mathit{term}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}}\end{aligned} \\
&&& \;\vdots \\
& \mathtt{\color{red!50!blue}\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{digits}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{digits}_{\mathtt{\color{red!50!blue}3}}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{expr}}=_R\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}}=_R{\color{blue}\mathit{term}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}}\end{aligned}; \\ &{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}={\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}=_R{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}3}}}={\color{blue}\mathit{digits}_{\mathtt{\color{red!50!blue}3}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{digits}_{\mathtt{\color{red!50!blue}3}}}; \\ &{\color{blue}\mathit{expr}_{\mathtt{\color{red!50!blue}20\text{+}3}}}={\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}3}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}3}}}; \\ &{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}={\color{blue}\mathit{expr}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}={\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\end{aligned} \\
&{\color{red!80!black}\varepsilon} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{digits}}=\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}\{{\color{green!50!black}0} \ldots {\color{green!50!black}9}\}^\star; \\ &{\color{blue}\mathit{expr}}=_R\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}}=_R{\color{blue}\mathit{term}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}}\end{aligned}; \\ &{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}={\color{blue}\mathit{digits}}\cup\mathtt{{\color{green!50!black}\text{(}}}{\color{blue}\mathit{expr}}\mathtt{{\color{green!50!black}\text{)}}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}=_R{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}=\mathtt{{\color{green!50!black}\text{+}}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{red!80!black}\varepsilon}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\mathtt{{\color{green!50!black}\text{*}}}{\color{blue}\mathit{expr}}\cup{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\end{aligned} \\
\end{aligned}
$$

The conversion from context-free grammar to recursive regular expression
relies on a simple observation: Context free grammars
can be represented as fixed point of $\mathsf{RE}^n \to \mathsf{RE}^n$ function,
`letrec` with multiple variables. Using theorem by Bekić
we can reduce it to a fixed point on of $\mathsf{RE} \to \mathsf{RE}$ function.

**Bisection lemma** (Bekić, [*Definable operations in general algebras, and the theory of automata and flowcharts*](https://doi.org/10.1007/BFb0048939)):

For monotone $f : P \times Q \to P$ and $g : P \times Q \to Q$

$$
\left(\mathbf{fix}_{P\times Q} ( x,y ) = ((x,y) , g(x,y) )\right) = (x_0, y_0)
$$

where

$$
\begin{aligned}
x_0 & = (\mathbf{fix}_P\,x = f(x, y_0))
\\
y_0 & = (\mathbf{fix}_Q\,y = g(x_0, y))
\end{aligned}
$$

We can use bisection lemma to eliminate simultaneous recursion in CFG,
reducing it to recursive regular expression.
A CFG is a fixed point of $h_n : \mathsf{RE}^n \to \mathsf{RE}^n$,
We can assume that there's at least one production, the starting symbol.
If CFG has only one single production, than we can convert it to recursive
regular expression using $\mathbf{fix}$.
Otherwise $n = 1 + m$, take $P = \mathsf{RE}$ and $Q = \mathsf{RE}^m$
mextract functions $f : \mathsf{RE} \times \mathsf{RE}^m \to \mathsf{RE}$
and $g : \mathsf{RE} \times \mathsf{RE}^m \to \mathsf{RE}^m$,
and define

$$
h_m \bar{y} = \begin{aligned}[t]
\mathbf{let}\, &x_0 = \mathbf{fix}\,x = f(x, \bar{y}) \\
\mathbf{in}\,& g(x_0, \bar{y})
\end{aligned}
$$

where $\bar{y}$ means $m$ distinct $\mathsf{RE}$ variables.

Using this equation we can iterate top-level production removal,
until we reach the single production base case.

Note on performance
---------------------------

The note is short, because I haven't really measured anything.
The examples above run at interactive speed in GHCi,
and I suspect that collecting and pretty-printing the traces is
not free.

I wrote the parser from above using `parsec`:

```haskell
ex7parsec :: P.Parser ()
ex7parsec = expr where
    expr   = void $ P.try (mult >> P.char '*' >> expr) <|> mult
    mult   = void $ P.try (term >> P.char '+' >> mult) <|> term
    term   = P.try digits <|> void (P.char '(' *> expr *> P.char ')')
    digits = void $ some digit
    digit  = P.satisfy (\c -> c >= '0' && c <= '9')
```

Note, that it only *recognises*, i.e. doesn't build a parse tree.
I also didn't used *good practices* in writing `parsec` parsers,
rather translating the CFG as directly as possible.

The result is salty. The recursive-regexp approach is
1000-10000 times slower (and getting slower the longer the input string is).
Not really surprising, as the matching algorithm recomputes
a lot of things on each character, but still unfortunate.

We can get 100x speedup (but be still 100x slower than `parsec`)
by introducing explicit sharing instead of `Fix` (and `Let`).
At the end doing the same as Might, Darais and Spiewak;
with a difference that our public interface is non-opaque.

We take the original regular expression we started with,
and add a new constructor `Ref`:

```haskell
-- | Knot-tied recursive regular expression.
data RR
    = Eps
    | Ch CS.CharSet
    | App RR RR
    | Alt RR RR
    | Star RR
    | Ref !Int RR
```

This structure can now be circular, as long as we as cycles use
`Ref`. Conversion from `RE` with `Fix` is direct mapping of constructors,
the intersting part happens with `Fix`, we have to use `mfix`
(and a lazy state monad):

```haskell
    go :: RE RR -> State Int RR
    go (R.Fix _ r) = mfix $ \res -> do
        i <- newId
        r' <- go (fmap (unvar res id) r)
        return (Ref i r')
```

The result is still simple and not non-acceptably slow.
In fact, if speed is required one can fallback to nasty `derp` implementation,
as in my simple benchmark it seemed to be quite fast!
The benefit of `rere` is having a non-opaque inspectable
representation, it's good to know that we can go fast if needed.

```
benchmarking parsec
time                 22.31 μs   (21.44 μs .. 23.66 μs)

benchmarking rere
time                 237.2 ms   (207.5 ms .. 266.1 ms)

benchmarking ref
time                 6.029 ms   (5.486 ms .. 6.801 ms)

benchmarking derp
time                 20.31 μs   (18.37 μs .. 22.07 μs)
```

Intersection
------------

We know that regular expressions are closed under intersection,
and it's possible to convert from `RE` with `And` constructor
to `RE` without one. Context-free grammaras are not closed under intersection,
but our recursive `RE` can still be extended with additional `And` constructor,
and all the above will continue to work.

```haskell
data RE a =
    ...
    | And (RE a) (RE a)
```

We can also add `Full` constructor which would match everything.

The extension of `nullable` and `derivative` is so simple,
you may think something will break, yet nothing does:

```haskell
nullable' Full      = True
nullable' (And r s) = nullable' r && nullable' s

derivative' _ Full      = Full
derivative' f (And r s) = derivative' f r /\ derivative' f s

(/\) :: Ord a => RE a -> RE a -> RE a
...
r /\ s = And r s
```

The example is a intersection of two languages:

$$
\begin{aligned}
X &= \{ a^n b^n c^m \mid n, m \in \mathbb{N} \}
&
Y &= \{ a^m b^n c^n \mid n, m \in \mathbb{N} \}
\end{aligned}
$$

which is known to be non context-free:

$$
L = X \cap Y = \{ a^n b^n c^n \mid n \in \mathbb{N} \}
$$

However, we can simply match with it:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}aaabbbccc} &&\vdash \mathtt{\color{green!50!black}a}^\star(\mathbf{fix}\,{\color{blue}\mathit{bc}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c})\cap(\mathbf{fix}\,{\color{blue}\mathit{ab}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b})\mathtt{\color{green!50!black}c}^\star \\
& \mathtt{\color{red!50!blue}aabbbccc} &&\vdash \mathbf{let}\,{\color{blue}\mathit{ab}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b}\,\mathbf{in}\,\mathtt{\color{green!50!black}a}^\star(\mathbf{fix}\,{\color{blue}\mathit{bc}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c})\cap{\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}c}^\star \\
& \mathtt{\color{red!50!blue}abbbccc} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{ab}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{ab}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,\mathtt{\color{green!50!black}a}^\star(\mathbf{fix}\,{\color{blue}\mathit{bc}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c})\cap{\color{blue}\mathit{ab}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}c}^\star\end{aligned} \\
& \mathtt{\color{red!50!blue}bbbccc} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{ab}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{ab}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{ab}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{ab}_{\mathtt{\color{red!50!blue}aa}}}={\color{blue}\mathit{ab}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,\mathtt{\color{green!50!black}a}^\star(\mathbf{fix}\,{\color{blue}\mathit{bc}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c})\cap{\color{blue}\mathit{ab}_{\mathtt{\color{red!50!blue}aa}}}\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}c}^\star\end{aligned} \\
& \mathtt{\color{red!50!blue}bbccc} &&\vdash \mathbf{let}\,{\color{blue}\mathit{bc}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c}\,\mathbf{in}\,{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c}\cap\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}c}^\star \\
& \mathtt{\color{red!50!blue}bccc} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{bc}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c}; \\ &{\color{blue}\mathit{bc}_{\mathtt{\color{red!50!blue}b}}}={\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c}\\ \mathbf{in} & \,{\color{blue}\mathit{bc}_{\mathtt{\color{red!50!blue}b}}}\mathtt{\color{green!50!black}c}\cap\mathtt{\color{green!50!black}b}\mathtt{\color{green!50!black}c}^\star\end{aligned} \\
& \mathtt{\color{red!50!blue}ccc} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{bc}}=_R{\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}b}{\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c}; \\ &{\color{blue}\mathit{bc}_{\mathtt{\color{red!50!blue}b}}}={\color{blue}\mathit{bc}}\mathtt{\color{green!50!black}c}; \\ &{\color{blue}\mathit{bc}_{\mathtt{\color{red!50!blue}bb}}}={\color{blue}\mathit{bc}_{\mathtt{\color{red!50!blue}b}}}\mathtt{\color{green!50!black}c}\\ \mathbf{in} & \,{\color{blue}\mathit{bc}_{\mathtt{\color{red!50!blue}bb}}}\mathtt{\color{green!50!black}c}\cap\mathtt{\color{green!50!black}c}^\star\end{aligned} \\
& \mathtt{\color{red!50!blue}cc} &&\vdash \mathtt{\color{green!50!black}c}\mathtt{\color{green!50!black}c}\cap\mathtt{\color{green!50!black}c}^\star \\
& \mathtt{\color{red!50!blue}c} &&\vdash \mathtt{\color{green!50!black}c}\cap\mathtt{\color{green!50!black}c}^\star \\
&{\color{red!80!black}\varepsilon} &&\vdash {\color{red!80!black}\varepsilon} \\
\end{aligned}
$$

This is of course slightly cheating, as we have `And` / $\cap$ as outer constructor,
not inside `Fix` / $\mathbf{fix}$. But even then it doesn't pose
problems for the algorithm, coming up with meaningful examples is.
One can also remember that we interpret $\mathbf{fix}$ as a lowest fixed point,
so for example even

$$
\mathbf{fix}\,{\color{blue}x} = {\color{blue}x} \cap {\color{red!80!black}\varepsilon}
$$

has ${\color{red!80!black}\varepsilon}$ as a fixed point, the
${\color{red!80!black}\emptyset}$ is also a fixed point and is the lowest one.
Therefore the above expression works (and indeed is automatically simplified to)
${\color{red!80!black}\emptyset}$.

However `And` / $\cap$ adds expressive power to the language, so it cannot be
admitted as in pure regular expressions (where it causes combinatorial
explosion of expression size, so not always a good idea either).

There's one clear trouble with `And` however: languages defined using
`And` cannot be easily generated. We can use first branch to generate
the candidate, but it must also match the second branch.
I don't know whether "is language empty" problem is decidable (for CFGs it is).
Consider simple regular expressions, strings of $a$ of odd and even length:

$$
\mathit{odd} = a(aa)^\star
\qquad\text{and}\qquad
\mathit{even} = (aa)^\star
$$

Their intersection is empty, but it's not structurally obvious.
$\mathbf{fix}$ makes the problem even more tricky.

Conclusion
----------

It was nice to combine known things in a new way, the result is interesting.

As a consequence, we know that we can add recursive types
to non-commutative intuitionistic linear logic,
even the first-order term synthesis becomes undecidable
(as CFG equivalence is undecidable).
That's pity, even in such restricted logic recursion
makes problems very hard.
On the other hand, we could add "lists" (Kleene Star),
and then synthesis could still be possible,
as ordinary regular expressions can be compared for equivalence.
As a concrete example, we can ask whether

$$
A^\star \overset{?}{\longleftrightarrow} A^\star A^\star
$$

and the synthesis would produce an append (`++`) function for the other
direction (and probably some trivial (`\xs -> ([], xs)` in other, thus *not* an
isomorphism, yet preserving all the $A$s and keeping them in order).

Also, we can use the recursive `RE` not only to match on strings,
but also to generate ones. Therefore we can use
it to statistically determine grammar equivalence (which is not a new idea).

Finally, this is not only for fun.
I'm trying to formalize the grammars of fields in `.cabal` files.
The `parsec` parsers are **the definition**, but we could
have a declarative definition and compare these two statistically, i.e. using
`QuickCheck`.
As a side effect we get nice looking $\text{\LaTeX}$ (hopefully) human-readable grammar definitions.
If you ever wondered which is full and precise syntax for version ranges in `.cabal` files, here is what it could look like:

$$
\begin{aligned}
\mathit{version} &=
{\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}
\\
\mathit{version\text{-}range} &=
\mathbf{fix}\;\mathop{\mathit{version\text{-}range}}\;\mathbf{in}\;\left\{
\begin{gathered}\mathop{\mathord{``}\mathtt{\text{-}any}\mathord{"}}\\
\mathop{\mathord{``}\mathtt{\text{-}none}\mathord{"}}\\
\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\
\mathop{\mathord{``}\mathtt{\text{>}}\mathord{"}}\circ\mathop{\mathit{version}}\\
\mathop{\mathord{``}\mathtt{\text{<}}\mathord{"}}\circ\mathop{\mathit{version}}\\
\mathop{\mathord{``}\mathtt{\text{<}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\
\mathop{\mathord{``}\mathtt{\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\
\mathop{\mathord{``}\mathtt{\text{\textasciicircum}\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\
\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{|}\text{|}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\\
\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{\&}\text{\&}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\\
\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}}\\
\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ{\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}\mathop{\mathord{``}\mathtt{\text{.}\text{*}}\mathord{"}}\\
\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{version}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}}\\
\mathop{\mathord{``}\mathtt{\text{\textasciicircum}\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{version}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}}
\end{gathered}
\right\}
\end{aligned}
$$

The notation is a bit obscure for uninitiated,
tuned to express concisely the particularities of `.cabal` grammars.
Short summary of main features are:

- terminals are enclosed in quotes $\mathord{``}\mathtt{abc}\mathord{"}$
- $\circ$ means any amount of white space,
- subscripts in the repetions syntax stand for used separators,
- and unions are enclosed in curly braces.

Code in `Parsec` instances are getting history baggage, and also written to produce helpful error messages.
However, we can compare it (and its companion `Pretty` instance)
with `RE` counterpart to find possible inconsistencies.
Also `RE`-derived generator can be amended to produce slightly skewed strings,
for example inserting or removing white space. `Cabal` has history
of not handling whitespace well, either always requiring, completely forbidding,
or allowing where it shouldn't be allowed.
