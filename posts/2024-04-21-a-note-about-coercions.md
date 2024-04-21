-------------------------------
title: A note about coercions
author: Oleg Grenrus
-------------------------------

Safe coercions in GHC are a very powerful feature.
However, they are not perfect; and already many years ago I was also thinking
about how we could make them more expressive.

In particular such things like "higher-order roles" have been buzzing.
For the record, I don't think [Proposal #233](https://github.com/ghc-proposals/ghc-proposals/pull/233) is great;
but because that proposal is almost four years old, I don't remember why;
nor I have tangible counter-proposal either.

So I try to recover my thoughts.

I like to build small prototypes; and I wanted to build a small language with zero-cost coercions.

The first approach, I present here, *doesn't work*.

While it allows model coercions, and very powerful ones,
these coercions *are not zero-cost* as we will see.
For language like GHC Haskell where being zero-cost is non-negotiable requirement, this simple approach doesn't work.

The small "formalisation" is in Agda file https://gist.github.com/phadej/5cf29d6120cd27eb3330bc1eb8a5cfcc

Syntax
------

We start by defining syntax. Our language is "simple":
there are types

```text
A, B = A -> B     -- function type, "arrow"
```

coercions

```text
co = refl A        -- reflexive coercion
   | sym co        -- symmetric coercions
   | arr co₁ co₂   -- coercion of arrows built from codomain and domain
                   -- type coercions
```

and terms

```text
f, t, s = x         -- variable
        | f t       -- application
        | λ x . t   -- lambda abstraction
        | t ▹ co    -- cast
```

Obviously we'd add more stuff (in particular, I'm interested in expanding coercion syntax),
but these are enough to illustrate the problem.

Because the language is simple (i.e. not dependent),
we can define typing rules and small step semantics independently.

Typing
------ 

There is nothing particularly surprising in typing rules.

We'll need a "well-typed coercion" rules too though, but these are also very straigh-forward

```text
Coercion Typing:  Δ ⊢ co : A ≡ B

------------------
Δ ⊢ refl A : A ≡ A

Δ ⊢ co : A ≡ B
------------------
Δ ⊢ sym co : B ≡ A

Δ ⊢ co₁ : C ≡ A
Δ ⊢ co₂ : D ≡ B
-------------------------------------
Δ ⊢ arr co₁ co₂ : (C -> D) ≡ (A -> B)
```

Terms typing rules are using two contexts, for term and coercion
variables (GHC has them in one, but that is unhygienic, there's [a GHC issue about that](https://gitlab.haskell.org/ghc/ghc/-/issues/17291)).
The rules for variables, applications and lambda abstractions are as usual,
the only new is the typing of the cast:

```text
Term Typing: Γ; Δ ⊢ t : A

Γ; Δ ⊢ t : A 
   Δ ⊢ co : A ≡ B
-------------------------
Γ; Δ ⊢ t ▹ co : B 
```

So far everything is good.

But when playing with coercions, it's important to specify the reduction rules too.
Ultimately it would be great to show that we could erase coercions
either before or after reduction, and in either way we'll get the same result.
So let's try to specify some reduction rules.

Reduction rules
---------------

Probably the simplest approach to reduction rules is to try to inherit
most reduction rules from the system without coercions;
and consider coercions and casts as another "type" and "elimination form".

An elimination of refl would compute trivially:

```text
t ▹ refl A ~~> t
```

This is good.

But what to do when cast's coercion is headed by `arr`?

```text
t ▹ arr co₁ co₂ ~~> ???
```

One "easy" solution is to eta-expand t, and split the coercion:

```text
t ▹ arr co₁ co₂ ~~> λ x . t (x ▹ sym co₁) ▹ co₂
```

We cast an argument before applying it to the function, and then cast the result.
This way the reduction is type preserving.

**But this approach is not zero-cost**.

We could not erase coercions completely, we'll still need some indicator
that there were an arrow coercion, so we'll remember to eta-expand:

```text
t ▹ ??? ~~> λ x . t x
```

Conclusion
----------

Treating coercions as another type constructor with cast operation being its elimination form may be a good first idea, but is not good enough.
We won't be able to completely erase such coercions.

Another idea is to complicate the system a bit.
We could "delay" coercion elimination until the result is 
scrutinised by another elimination form, e.g. in application case:

```text
(t ▹ arr co₁ co₂) s ~~> t (s ▹ sym co₁) ▹ co₂ 
```

And that is the approach taken in [Safe Zero-cost Coercions for Haskell](https://www.seas.upenn.edu/~sweirich/papers/coercible-JFP.pdf),
you'll need to look into JFP version of the paper, as that one has appendices.

<blockquote>
(We do not have space to elaborate, but a key example is the use
of <code>nth</code> in rule <code>S_KPUSH</code>, presented in the extended version
of this paper.)
</blockquote>

The rule `S_Push` looks some what like:

```text
---------------------------------------------- S_Push
(t ▹ co) s ~~> t (s ▹ sym (nth₁ co)) ▹ nth₂ co
```

where we additionally have `nth` coercion constructor to *decompose* coercions.

Incidentally there was, technically is, [a proposal to remove decomposition rule](https://github.com/ghc-proposals/ghc-proposals/pull/276),
but it's a wrong solution to the known problem. The problem and a proper solution was kind of already identified in the original paper

<blockquote>
We could similarly imagine a lattice keyed by classes whose instance
definitions are to be respected; with such a lattice, we could allow the coercion of
<code>Map Int v</code> to <code>Map Age v</code> precisely when <code>Int</code>’s and <code>Age</code>’s <code>Ord</code> instances correspond.
</blockquote>

The original paper also identified the need for higher-order roles.
And also identified that

<blockquote>
This means that <code>Monad</code> instances could be defined only for types
that expect a representational parameter.
</blockquote>

which [I argue should be already required for `Functor`](https://oleg.fi/gists/posts/2019-07-31-fmap-coerce-coerce.html) (and `traverseBia` hack
with unlawful `Mag` would still work if GHC had unboxed representational
coercions, i.e. GADTs with baked-in representational (not only nominal) coercions).

There also the mention of *unidirectional `Coercible`*, which people
[asked about later](https://github.com/ghc-proposals/ghc-proposals/issues/198)
and [recently](https://discourse.haskell.org/t/one-way-coercible/9242):

<blockquote>
Such uni-directional version of <code>Coercible</code> amounts to <i>explicit
inclusive subtyping</i> and is more complicated than our current symmetric system.
</blockquote>

It is fascinating that authors were able to predict the relevant future work so well.
And I'm thankful that GHC got `Coercible` implemented even it was already known to not be perfect. It's useful nevertheless.
But I'm sad that there haven't been any results of future work since.
