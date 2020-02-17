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

I omit rest of the rules, they are simple t


The rules resemble the structure of *non-commutative intuitionistic linear logic*,
if you are into such stuff.
Not only you have to use everything exactly once, you have to use in order;
there aren't any substructural rules, no weakening, no contraction
and even no exchange.

It's wise to define smart versions of constructors,
which would simplify regular expressions as they are created.
For example, we defined `Semigroup` instance for concatenation,
i.e. `<>` is smart `App`:

```haskell
instance Eq a => Semigroup (RE a) where
    -- Null is annihilates
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
then it can be matched only in one way, by applying the $\textsc{App}$ rule.
Therefore $\color{red!50!blue}\Gamma$ is also matched by bare ${\color{blue}\mathit{r}}$.
If we'd introduce *proof terms*, we'll have a concrete evidence of the match
as terms in this language.

Matching using declarative rules is not practical: we have to guess.
We have to guess whether we should pick left or right branch,
or where we should split string to match concatenated regular expression.
We need a *syntax directed* approach.
The system consists only of two rules:

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
and mapping $D_c(r)$, `derivative :: Char -> RE -> RE`.

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
is a syntax for substitution.

To have let-expressions we need to represent *variables* and be
able to perform substitution. My tool of choice for that
is [`bound`](https://hackage.haskell.org/package/bound),
but for the sake of being self-contained we'll define needed bits here.
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

Now we can write regular expression like
$\mathbf{let}\,{\color{blue}\mathit{r}}=\mathtt{\color{green!50!black}a}^\star\,\mathbf{in}\,{\color{blue}\mathit{r}}{\color{blue}\mathit{r}}$

```haskell
ex2 :: RE Void
ex2 = Let "r" (star (Ch 'a')) (Var B <> Var B)
```

where `Void` tells that expression is *closed*, i.e. doesn't contain
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

The `derivative'` of variable is simple, look what is the context:

```haskell
go f (Var a) = Var (sndOf3 (f a))
```

And `Let` is actually interesting:

```haskell
derivative' f (Let r s)
    = let_ (fmap (trdOf3 . f) r)
    $ let_ (fmap F (derivative r')
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
we don't introduce new `let`s, only consider what we already bind
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

As our smart constructors are quite smart, the "automaton" stays
in its single state, the union comes from the `derivative` of `App`,
as `r` is nullable, we get `derivative 'a' r \/ derivative 'a' r <> r`.
And as `derivative 'a' r = r`, we don't see additional `let` bindings.

Recursion
---------

The thing we have waiting for: *recursion*. It's matter of adding
one more constructor:

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

The problem is now: How to define `nullable` and `derivative`?
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

in other words, we literally assume that the nullability of new binding
is `False`, and see what comes out. We don't need to iterate
more then once, as `False` will flip to `True` right away, or will
never do so.

The extension of `derivative` is again slightly more complicated, but not much.

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

And in the code this looks very much like `Let`:

```haskell
derivative' f r0@(Fix r)
    = let_ (fmap (trdOf3 . f) r0)
    $ Fix
    $ derivative' (\case
        B   -> (nullable' (fstOf3 . f) r0, B, F B)
        F x -> bimap (F . F) (F . F) (f x))
    $ r
```

Let's see how it works. The same example as we did with `ex1` executes nicely:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}abab} &&\vdash_\varepsilon \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
& \mathtt{\color{red!50!blue}bab} &&\vdash_\kappa \mathbf{let}\,{\color{blue}\mathit{x}}=\mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}\,\mathbf{in}\,\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
& \mathtt{\color{red!50!blue}ab} &&\vdash_\varepsilon \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
& \mathtt{\color{red!50!blue}b} &&\vdash_\kappa \mathbf{let}\,{\color{blue}\mathit{x}}=\mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}\,\mathbf{in}\,\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
&{\color{red!80!black}\varepsilon} &&\vdash_\varepsilon \mathbf{fix}\,{\color{blue}\mathit{x}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}\mathtt{\color{green!50!black}b}{\color{blue}\mathit{x}} \\
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
L = \{ a^n b^n | n \in \mathbb{N} \}
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
& \mathtt{\color{red!50!blue}aaabbbb} &&\vdash \mathbf{let}\,{\color{blue}\mathit{x}}=\mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}b}\,\mathbf{in}\,{\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b} \\
& \mathtt{\color{red!50!blue}aabbbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}}=\mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}abbbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}}=\mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}bbbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}}=\mathbf{fix}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}={\color{red!80!black}\varepsilon}\cup\mathtt{\color{green!50!black}a}{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}={\color{blue}\mathit{x}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}a}}}\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaa}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aa}}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaa}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}bbb} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aab}}}=\mathtt{\color{green!50!black}b}; \\ &{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaab}}}={\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aab}}}\mathtt{\color{green!50!black}b}\\ \mathbf{in} & \,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaab}}}\mathtt{\color{green!50!black}b}\end{aligned} \\
& \mathtt{\color{red!50!blue}bb} &&\vdash \mathbf{let}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaabb}}}=\mathtt{\color{green!50!black}b}\,\mathbf{in}\,{\color{blue}\mathit{x}_{\mathtt{\color{red!50!blue}aaabb}}}\mathtt{\color{green!50!black}b} \\
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
\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}4}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned}
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
& \mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
& \mathtt{\color{red!50!blue}\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}1}}}={\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{e}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}={\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}1}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
&&& \vdots \\
& \mathtt{\color{red!50!blue}\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}3}}}={\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{e}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3};1}}={\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
&{\color{red!80!black}\varepsilon} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{e}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3\text{)}}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3\text{)}};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3\text{)}};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3\text{)}};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}};1}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3\text{)}}}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
\end{aligned}
$$

The final state is nullable:
one alternative of the final fix point is ${\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}$
which itself is nullable because it contains ${\color{red!80!black}\varepsilon}$.

The other two alternatives are "continuations" starting with `+` or `*`,
as the arithmetic expression could indeed continue starting with them.

Also one could notice that ${\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}}}} =\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3\text{)}};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}$
is in fact ${\color{red!80!black}\emptyset}$, but our constructors aren't sufficiently smart to recognize that case:
With fix points it's easy to write a null expression, which is not syntactically obviously null.

One idea, which I'm not sure is correct is to substitute `Null`:
If there's anything else an expression could match that by Kleene's theorem one should get not `Null`. *Maybe*.
This is a small addition to smart constructor `fix_`:

```haskell
fix_ :: Ord a => RE (Var a) -> RE a
fix_ r
    | Just r' <- unused r
    = r'
    -- here: substitute Null and see what pop-out:
    | (r >>>= unvar Null Var) == Null
    = Null
fix_ r = Fix r


```

Then the example above becomes a bit more compact:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
& \mathtt{\color{red!50!blue}\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}1}}}={\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{e}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}={\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}1}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
&&& \vdots \\
& \mathtt{\color{red!50!blue}\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}3}}}={\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{e}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3};1}}={\color{blue}\mathit{n}_{\mathtt{\color{red!50!blue}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3};1}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
&{\color{red!80!black}\varepsilon} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{d}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{n}}={\color{blue}\mathit{d}}{\color{blue}\mathit{d}}^\star; \\ &{\color{blue}\mathit{e}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}=\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}\cup{\color{blue}\mathit{n}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}=\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}};1}}={\color{red!80!black}\varepsilon}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}};1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}};1}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}={\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{e}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}\text{(}20\text{+}3\text{)}}}}\cup{\color{blue}\mathit{e}_{\mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{e}}\end{aligned} \\
\end{aligned}
$$

A short note on performance
---------------------------

The note is short, because I haven't measured anything.
The examples above run at interactive speed in GHCi,
and I suspect that collecting and pretty-printing the traces is
not free.

It's really hard to tell how slow this is.
Not all smart constructors are cheap,
e.g. the `let_` one is expensive as it checks whether the
expression is closed. Also `fix_` perform a substitution.
On the other hand, expression size doesn't seem to explode
exponentially, which is a good sign.

I'm quite sure that Might, Darais, Spiewak approach is more efficient,
but this one is elegantly pure.

Conclusion and further work
---------------------------

It was nice to combine known things in a new way, the result is interesting.

There's interesting follow up questions, like:
could all context-free languages be expressed in this framework?
Is there some algorithm to rewrite
a usual context-free grammar into the formalism presented here?
(I think yes, I can imagine an inductive algorithm removing non-terminal symbols
at each iteration, converting them to $\mathbf{fix}$ or $\mathbf{let}$). 

```plain
EXPR ::= MULT * EXPR | MULT
MULT ::= TERM + MULT | TERM
TERM ::= DIGIT | "(" EXPR ")"
```

in somewhat straight forward manner is convertible to:

$$
\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{digit}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{digits}}={\color{blue}\mathit{digit}}\,{\color{blue}\mathit{digit}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{expr}}=\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{expr}}\mathtt{\color{green!50!black}\text{)}}; \\ &{\color{blue}\mathit{mult}}=\mathbf{fix}\,{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}={\color{blue}\mathit{term}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{term}}\cup{\color{blue}\mathit{mult}}\end{aligned}\end{aligned}
$$

And it works:

$$
\begin{aligned}
& \mathtt{\color{red!50!blue}1\text{*}\text{(}20\text{+}3\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{digit}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{digits}}={\color{blue}\mathit{digit}}{\color{blue}\mathit{digit}}^\star\\ \mathbf{in} & \,\mathbf{fix}\,{\color{blue}\mathit{expr}}=\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{expr}}\mathtt{\color{green!50!black}\text{)}}; \\ &{\color{blue}\mathit{mult}}=\mathbf{fix}\,{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}={\color{blue}\mathit{term}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{term}}\cup{\color{blue}\mathit{mult}}\end{aligned}\end{aligned} \\
&&& \vdots \\
& \mathtt{\color{red!50!blue}\text{)}} &&\vdash \begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{digit}}=\mathtt{\color{green!50!black}0}\cup\mathtt{\color{green!50!black}1}\cup\mathtt{\color{green!50!black}2}\cup\mathtt{\color{green!50!black}3}; \\ &{\color{blue}\mathit{digits}}={\color{blue}\mathit{digit}}{\color{blue}\mathit{digit}}^\star; \\ &{\color{blue}\mathit{digits}_{\mathtt{\color{red!50!blue}3}}}={\color{blue}\mathit{digit}}^\star; \\ &{\color{blue}\mathit{expr}}=\mathbf{fix}\,{\color{blue}\mathit{expr}_{\mathtt{\color{red!50!blue}}1}}=\begin{aligned}[t] \mathbf{let}& \,{\color{blue}\mathit{term}}={\color{blue}\mathit{digits}}\cup\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{expr}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{)}}; \\ &{\color{blue}\mathit{mult}}=\mathbf{fix}\,{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}={\color{blue}\mathit{term}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{term}}\\ \mathbf{in} & \,{\color{blue}\mathit{mult}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{term}}\cup{\color{blue}\mathit{mult}}\end{aligned}; \\ &{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}={\color{blue}\mathit{digits}}\cup\mathtt{\color{green!50!black}\text{(}}{\color{blue}\mathit{expr}}\mathtt{\color{green!50!black}\text{)}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}2}}=\mathbf{fix}\,{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}3}}={\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}3}}\cup{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}; \\ &{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}3}}}={\color{blue}\mathit{digits}_{\mathtt{\color{red!50!blue}3}}}\mathtt{\color{green!50!black}\text{+}}{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}}2}}\cup{\color{blue}\mathit{digits}_{\mathtt{\color{red!50!blue}3}}}; \\ &{\color{blue}\mathit{expr}_{\mathtt{\color{red!50!blue}20\text{+}3}}}={\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}3}}}\mathtt{\color{green!50!black}\text{*}}{\color{blue}\mathit{term}_{\mathtt{\color{red!50!blue}}1}}\cup{\color{blue}\mathit{mult}_{\mathtt{\color{red!50!blue}3}}}\\ \mathbf{in} & \,{\color{blue}\mathit{expr}_{\mathtt{\color{red!50!blue}20\text{+}3}}}\mathtt{\color{green!50!black}\text{)}}\end{aligned} \\
&{\color{red!80!black}\varepsilon} &&\vdash {\color{red!80!black}\varepsilon} \\
\end{aligned}
$$

but I cannot describe the procedure precisely.


Another question is whether expressions in this format
can be compared for an equivalence. If they can represent all CFGs,
then the answer is **No**, as that problem is generally undecidable.
But maybe there's some equivalence (broader than structural equality, even
after applying smart constructors), which could be somehow useful?

At the very least, we can use the `RE` not only to match on strings,
but also to generate ones. Therefore at least statistical testing
is possible (not a new idea).

Finally, this is not only for fun.
I'm trying to formalize the grammars of fields in `.cabal` files.
The `parsec` parsers are **the definition**, but we could
have a declarative definition
- which can be visualized (in e.g $\text{\LaTeX}$),
  i.e. more easily understandable by humans. `Parsec` instances
  are getting history baggage, and also written to produce
  helpful error messages.
- and compared against:
  - We can generate data using `Pretty` instance and parse using 
    this framework, and the opposite direction
  - We can use regexp to generate strings which should be
    accepted by `Parsec` instance
  - Or we can perturb the regexp to produce something else,
    and check whether whether regexp matching and `parsec` parsing
    agree.
    For example inserting or removing white space, which is a mistake made more
    than once in the grammars.
