---
title: Three different thinnings
author: Oleg Grenrus
---

I was lately again thinking about thinnings.

Thinnings are a weaker form of renamings, which we use in well-scoped or well-typed implementations of programming languages.
(Their proper name is *order-preserving embeddings*, mathematicians may know them as morphism in *augmented simplex category* Δ₊)

There is one well known and used implementation implementation for them.
It's simple to use and write proofs about.
However it's not super great. Especially it's not great in Haskell, as it cannot be given `Category` instance.
(Though you almost never need thinnings in Haskell, so the reason is a bit moot).

I'll show two other implementations, and show that they are equivalent, using *Cubical Agda* to state the equivalences.
Before we dive in, Agda module prologue:

```agda
{-# OPTIONS --cubical --safe #-}
module 2022-09-30-thinnings where

open import Cubical.Core.Everything
open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism
open import Cubical.Data.Nat
open import Cubical.Data.Empty
open import Cubical.Data.Sigma
open import Cubical.Relation.Nullary
```

I will show only a well-scoped thinnings. So the context are simply natural numbers.
As there are plenty of them, let us define few common variables.

```agda
variable
  n m p r : ℕ
```

Orthodox thinnings
-----------------

For the sake of this post, I call well known thinnings *orthodox*, and use ₒ subscript to indicate that.

```agda
data _⊑ₒ_ : ℕ → ℕ → Type where
  nilₒ   :           zero   ⊑ₒ zero
  skipₒ  : n ⊑ₒ m →  n      ⊑ₒ suc m
  keepₒ  : n ⊑ₒ m →  suc n  ⊑ₒ suc m

Orth = _⊑ₒ_
```

An example thinning is like

```agda
exₒ : 5 ⊑ₒ 7
exₒ = keepₒ (skipₒ (keepₒ (skipₒ (keepₒ (keepₒ (keepₒ nilₒ))))))
```

Which would look like:

\begin{tikzpicture}
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (A) at (0,0.00) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (B) at (0,0.50) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (C) at (0,1.00) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (D) at (0,2.00) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (E) at (0,3.00) {};
  \node[anchor=east] at (A) {$0$};
  \node[anchor=east] at (B) {$1$};
  \node[anchor=east] at (C) {$2$};
  \node[anchor=east] at (D) {$3$};
  \node[anchor=east] at (E) {$4$};
  
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (X) at (2,0.00) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (Y) at (2,0.50) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (Z) at (2,1.00) {};
  \node[circle, draw,             inner sep=0pt, minimum width=4pt] (U) at (2,1.50) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (V) at (2,2.00) {};
  \node[circle, draw,             inner sep=0pt, minimum width=4pt] (W) at (2,2.50) {};
  \node[circle, draw, fill=black, inner sep=0pt, minimum width=4pt] (Q) at (2,3.00) {};
  \node[anchor=west] at (X) {$0$};
  \node[anchor=west] at (Y) {$1$};
  \node[anchor=west] at (Z) {$2$};
  \node[anchor=west] at (U) {$3$};
  \node[anchor=west] at (V) {$4$};
  \node[anchor=west] at (W) {$5$};
  \node[anchor=west] at (Q) {$6$};

  \draw[-] (A) -- (X);
  \draw[-] (B) -- (Y);
  \draw[-] (C) -- (Z);
  \draw[-] (D) -- (V);
  \draw[-] (E) -- (Q);
\end{tikzpicture}

We can define identity thinning:

```agda
idₒ : n ⊑ₒ n
idₒ {zero}   = nilₒ
idₒ {suc n}  = keepₒ idₒ
```

Note how it pattern matches on the size (of the context).
That what makes it impossible to defined `Category` instance in Haskell.

We can also define composition, and weakening on top of the context

```agda
_⦂ₒ_ : n ⊑ₒ m → m ⊑ₒ p → n ⊑ₒ p
δ₁        ⦂ₒ nilₒ      = δ₁
δ₁        ⦂ₒ skipₒ δ₂  = skipₒ (δ₁ ⦂ₒ δ₂)
keepₒ δ₁  ⦂ₒ keepₒ δ₂  = keepₒ (δ₁ ⦂ₒ δ₂)
skipₒ δ₁  ⦂ₒ keepₒ δ₂  = skipₒ (δ₁ ⦂ₒ δ₂)

wkₒ : n ⊑ₒ suc n
wkₒ = skipₒ idₒ
```

As said, the proofs about this formulation are simple.
Plenty of equalities hold definitionally:

```agda
keep-id≡idₒ : keepₒ idₒ ≡ idₒ {suc n}
keep-id≡idₒ = refl
```

Separate thinning
-----------------

As mentioned in previous section the orthodox thinning is not very efficient.
For example when implementing *normalization by evaluation* (NbE) we run into problems.
There we need identity thinning when evaluating every application,
so we will pay a price proportional to the size of the current context!

In his work Andras Kovacs makes a variant swapping `nilₒ` for `idₒ`.
However then thinnings won't have unique representation anymore and proofs become more inconvenient to write.

We can make a special case for identity thinning without sacrificing unique representation for the cost of slightly more complicated definition.
We just need to consider identity thinning and non-identity ones separately. 

```agda
data _⊏ₛ_ : ℕ → ℕ → Type where
  wkₛ    :           n      ⊏ₛ suc n
  keepₛ  : n ⊏ₛ m →  suc n  ⊏ₛ suc m
  skipₛ  : n ⊏ₛ m →  n      ⊏ₛ suc m

data _⊑ₙ_ : ℕ → ℕ → Type where
  idₙ :              n ⊑ₙ n
  strict : n ⊏ₛ m →  n ⊑ₙ m

Strict = _⊏ₛ_
NonStr = _⊑ₙ_
```

We can implement most operations without much problems.
Note that also `wkₙ` has a small, context-size independent, representation.

```agda
nilₙ : zero ⊑ₙ zero
nilₙ = idₙ

wkₙ : ∀ {n} → n ⊑ₙ suc n
wkₙ = strict wkₛ

skipₙ : n ⊑ₙ m → n ⊑ₙ suc m
skipₙ idₙ         = wkₙ
skipₙ (strict x)  = strict (skipₛ x)

keepₙ : n ⊑ₙ m → suc n ⊑ₙ suc m
keepₙ idₙ         = idₙ
keepₙ (strict δ)  = strict (keepₛ δ)

keep-id≡idₙ : keepₙ idₙ ≡ idₙ {suc n}
keep-id≡idₙ = refl
```

Composition is a bit more complicated then for orthodox variant,
but not considerably:

```agda
_⦂ₛ_ : n ⊏ₛ m → m ⊏ₛ p → n ⊏ₛ p
δ₁        ⦂ₛ wkₛ       = skipₛ δ₁
δ₁        ⦂ₛ skipₛ δ₂  = skipₛ (δ₁ ⦂ₛ δ₂)
wkₛ       ⦂ₛ keepₛ δ₂  = skipₛ δ₂
keepₛ δ₁  ⦂ₛ keepₛ δ₂  = keepₛ (δ₁ ⦂ₛ δ₂)
skipₛ δ₁  ⦂ₛ keepₛ δ₂  = skipₛ (δ₁ ⦂ₛ δ₂)

_⦂ₙ_ : n ⊑ₙ m → m ⊑ₙ p → n ⊑ₙ p
δ₁         ⦂ₙ idₙ         = δ₁
idₙ        ⦂ₙ strict δ₂        = strict δ₂
strict δ₁  ⦂ₙ strict δ₂  = strict (δ₁ ⦂ₛ δ₂)
```

**Are these orthodox and this thinning the same?**

Are `⊑ₒ` and `⊑ₙ` the same?
We can construct an isomorphism between them to answer that question positively.

```agda
Orth→NonStr : n ⊑ₒ m → n ⊑ₙ m
Orth→NonStr nilₒ        = nilₙ
Orth→NonStr (keepₒ δ)   = keepₙ (Orth→NonStr δ)
Orth→NonStr (skipₒ δ)   = skipₙ (Orth→NonStr δ)

Strict→Orth : n ⊏ₛ m → n ⊑ₒ m
Strict→Orth wkₛ         = wkₒ
Strict→Orth (keepₛ δ)   = keepₒ (Strict→Orth δ)
Strict→Orth (skipₛ δ)   = skipₒ (Strict→Orth δ)

NonStr→Orth : n ⊑ₙ m → n ⊑ₒ m
NonStr→Orth idₙ         = idₒ
NonStr→Orth (strict δ)  = Strict→Orth δ
```

It is not enough to define conversion functions we also need to show that they cancel out.
Luckily this is not difficult, we need few auxiliary homomorphism lemmas.

```agda
NonStr→Orth-keepₒ : (δ : n ⊑ₙ m) → NonStr→Orth (keepₙ δ) ≡ keepₒ (NonStr→Orth δ)
NonStr→Orth-skipₒ : (δ : n ⊑ₙ m) → NonStr→Orth (skipₙ δ) ≡ skipₒ (NonStr→Orth δ)
Orth→NonStr-id≡id : ∀ n → Orth→NonStr idₒ ≡ idₙ {n}
```

```agda
NonStr→Orth-keepₒ idₙ         = refl
NonStr→Orth-keepₒ (strict _)  = refl

NonStr→Orth-skipₒ idₙ         = refl
NonStr→Orth-skipₒ (strict _)  = refl

Orth→NonStr-id≡id zero    = refl
Orth→NonStr-id≡id (suc n) = cong keepₙ (Orth→NonStr-id≡id n)
```

And finally we can show that `Orth→NonStr` `NonStr→Orth` are each others inverses.

```agda
Orth→NonStr→Orth    : (δ : n ⊑ₒ m) → NonStr→Orth (Orth→NonStr δ) ≡ δ
Strict→Orth→NonStr  : (δ : n ⊏ₛ m) → Orth→NonStr (Strict→Orth δ) ≡ strict δ
NonStr→Orth→NonStr  : (δ : n ⊑ₙ m) → Orth→NonStr (NonStr→Orth δ) ≡ δ
```

```agda
Orth→NonStr→Orth nilₒ       = refl
Orth→NonStr→Orth (keepₒ δ)  = NonStr→Orth-keepₒ (Orth→NonStr δ) ∙ cong keepₒ (Orth→NonStr→Orth δ)
Orth→NonStr→Orth (skipₒ δ)  = NonStr→Orth-skipₒ (Orth→NonStr δ) ∙ cong skipₒ (Orth→NonStr→Orth δ)

Strict→Orth→NonStr wkₛ        = cong skipₙ (Orth→NonStr-id≡id _)
Strict→Orth→NonStr (keepₛ δ)  = cong keepₙ (Strict→Orth→NonStr δ)
Strict→Orth→NonStr (skipₛ δ)  = cong skipₙ (Strict→Orth→NonStr δ)

NonStr→Orth→NonStr idₙ         = Orth→NonStr-id≡id _
NonStr→Orth→NonStr (strict δ)  = Strict→Orth→NonStr δ
```

In *Cubical Agda* we can promote the above isomorphism to an equality.

```agda
Orth≡NonStr-pointwise : (n ⊑ₒ m) ≡ (n ⊑ₙ m)
Orth≡NonStr-pointwise = isoToPath
  (iso Orth→NonStr NonStr→Orth NonStr→Orth→NonStr Orth→NonStr→Orth)

Orth≡NonStr : Orth ≡ NonStr
Orth≡NonStr i n m = Orth≡NonStr-pointwise {n} {m} i
```

But are they still the same?

Even the types are the same, are the operations we defined on them the same?
We still need to show that the operations give the same results.

I'll define a simplified "category operations" type,
with an identity and a composition:

```agda
CatOps : (ℕ → ℕ → Type) → Type
CatOps _↝_
  = (∀ {n} → n ↝ n)                       -- identity
  × (∀ {n m p} → n ↝ m → m ↝ p → n ↝ p )  -- composition
```

Orthodox category ops are:

```agda
CatOps-Orth : CatOps Orth
CatOps-Orth = idₒ , _⦂ₒ_
```

And NonStr ops are:

```agda
CatOps-NonStr : CatOps NonStr
CatOps-NonStr = idₙ , _⦂ₙ_
```

And we can show transport orthodox ops along `Orth≡NonStr` to get other variant

```agda
CatOps-NonStrₜ : CatOps NonStr
CatOps-NonStrₜ = subst CatOps Orth≡NonStr CatOps-Orth
```

The goal is to show that all these are equal.

First We can construct a path between two `CatOps NonStr` structures,

For identity part we need identity homomorphism:

```agda
Orth→NonStr-id : Orth→NonStr idₒ ≡ idₙ {n}
Orth→NonStr-id {zero}  = refl
Orth→NonStr-id {suc n} = cong keepₙ (Orth→NonStr-id {n})
```

Then we can extract the transported identity, and show it is the same as `idₙ`:

```agda
idₙₜ : n ⊑ₙ n
idₙₜ = fst CatOps-NonStrₜ

idₙₜ≡idₙ : idₙₜ ≡ idₙ {n}
idₙₜ≡idₙ = transportRefl (Orth→NonStr idₒ) ∙ Orth→NonStr-id
```

The composition is slightly more complicated.

```agda
skip-⦂ₙ : (δ₁ : n ⊑ₙ m) → (δ₂ : m ⊑ₙ p)
        → skipₙ (δ₁ ⦂ₙ δ₂) ≡ (δ₁ ⦂ₙ skipₙ δ₂)
skip-⦂ₙ idₙ         idₙ         = refl
skip-⦂ₙ (strict _)  idₙ         = refl
skip-⦂ₙ idₙ         (strict _)  = refl
skip-⦂ₙ (strict _)  (strict _)  = refl

skip-keep-⦂ₙ : (δ₁ : n ⊑ₙ m) (δ₂ : m ⊑ₙ p)
             → skipₙ (δ₁ ⦂ₙ δ₂) ≡ (skipₙ δ₁ ⦂ₙ keepₙ δ₂)
skip-keep-⦂ₙ δ₁          idₙ         = refl
skip-keep-⦂ₙ idₙ         (strict _)  = refl
skip-keep-⦂ₙ (strict _)  (strict _)  = refl

keep-keep-⦂ₙ : (δ₁ : n ⊑ₙ m) (δ₂ : m ⊑ₙ p)
             → keepₙ (δ₁ ⦂ₙ δ₂) ≡ (keepₙ δ₁ ⦂ₙ keepₙ δ₂)
keep-keep-⦂ₙ δ₁          idₙ         = refl
keep-keep-⦂ₙ idₙ         (strict x)  = refl
keep-keep-⦂ₙ (strict _)  (strict _)  = refl
```

We can show that `Orth→NonStr` preserves composition.

```agda
Orth→NonStr-⦂ : (δ₁ : n ⊑ₒ m) (δ₂ : m ⊑ₒ p)
              → Orth→NonStr (δ₁ ⦂ₒ δ₂) ≡ Orth→NonStr δ₁ ⦂ₙ Orth→NonStr δ₂
Orth→NonStr-⦂ δ₁          nilₒ        = refl
Orth→NonStr-⦂ δ₁          (skipₒ δ₂)  = cong skipₙ (Orth→NonStr-⦂ δ₁ δ₂) ∙ skip-⦂ₙ (Orth→NonStr δ₁) (Orth→NonStr δ₂)
Orth→NonStr-⦂ (skipₒ δ₁)  (keepₒ δ₂)  = cong skipₙ (Orth→NonStr-⦂ δ₁ δ₂) ∙ skip-keep-⦂ₙ (Orth→NonStr δ₁) (Orth→NonStr δ₂)
Orth→NonStr-⦂ (keepₒ δ₁)  (keepₒ δ₂)  = cong keepₙ (Orth→NonStr-⦂ δ₁ δ₂) ∙ keep-keep-⦂ₙ (Orth→NonStr δ₁) (Orth→NonStr δ₂)
```

Using the above fact, we can show that \AgdaFunction{⦂ₙ} and \AgdaFunction{⦂ₙₜ} are pointwise equal.
The proof looks complicated, but is pretty straightforward in the end.

```agda
_⦂ₙₜ_ : n ⊑ₙ m → m ⊑ₙ p → n ⊑ₙ p
_⦂ₙₜ_ = snd CatOps-NonStrₜ

⦂ₙₜ≡⦂ₙ : (δ₁ : n ⊑ₙ m) (δ₂ : m ⊑ₙ p) → δ₁ ⦂ₙₜ δ₂ ≡ δ₁ ⦂ₙ δ₂
⦂ₙₜ≡⦂ₙ {n} {m} {p} δ₁ δ₂ =
  transport refl expr₁  ≡⟨ transportRefl expr₁ ⟩
  expr₁                 ≡⟨ expr₁≡expr₂ ⟩
  expr₂                 ≡⟨ Orth→NonStr-⦂ (NonStr→Orth δ₁) (NonStr→Orth δ₂) ⟩
  expr₃                 ≡⟨ (λ i → NonStr→Orth→NonStr δ₁ i ⦂ₙ
                                  NonStr→Orth→NonStr δ₂ i) ⟩
  δ₁ ⦂ₙ δ₂ ∎
  where
    expr₁ = Orth→NonStr (NonStr→Orth (transport refl δ₁) ⦂ₒ
                         NonStr→Orth (transport refl δ₂))
    expr₂ = Orth→NonStr (NonStr→Orth δ₁ ⦂ₒ NonStr→Orth δ₂)
    expr₃ = Orth→NonStr (NonStr→Orth δ₁) ⦂ₙ Orth→NonStr (NonStr→Orth δ₂)

    expr₁≡expr₂ : expr₁ ≡ expr₂
    expr₁≡expr₂ i = Orth→NonStr (NonStr→Orth (transportRefl δ₁ i) ⦂ₒ
                                 NonStr→Orth (transportRefl δ₂ i))
```

And finally we can state that first equality:

```agda
CatOps-NonStr≡ : CatOps-NonStrₜ ≡ CatOps-NonStr
CatOps-NonStr≡ i = idₙₜ≡idₙ i , λ δ₁ δ₂ → ⦂ₙₜ≡⦂ₙ δ₁ δ₂ i
```

and the quality we actually wanted to say,
that `CatOps-Orth` and `CatOps-NonStr` are equal (if we equate their types by `Orth≡NonStr`)!!!

```agda
CatOps-Orth≡NonStr : (λ i → CatOps (Orth≡NonStr i))
  [ CatOps-Orth ≡ CatOps-NonStr ]
CatOps-Orth≡NonStr = toPathP CatOps-NonStr≡
```

Higher-inductive type
---------------------

*Cubical Agda* also supports *higher inductive types* (HITs), i.e. types with additional equalities.
We can formalize Andras better performing thinning as a HIT, by throwing in an additional equality.
*Agda* will then ensure that we always respect it.

```agda
data _⊑ₕ_ : ℕ → ℕ → Type where
  idₕ    :           n      ⊑ₕ n
  keepₕ  : n ⊑ₕ m →  suc n  ⊑ₕ suc m
  skipₕ  : n ⊑ₕ m →  n      ⊑ₕ suc m

  -- it is what it says: keep idₕ ≡ idₕ
  keep-id≡idₕ : ∀ n → keepₕ (idₕ {n = n}) ≡ idₕ {n = suc n}

HIT = _⊑ₕ_
```

Composition for HIT-thinning looks very similar to the orthodox version...

```agda
_⦂ₕ_ : n ⊑ₕ m → m ⊑ₕ p → n ⊑ₕ p
δ₁        ⦂ₕ idₕ       = δ₁
δ₁        ⦂ₕ skipₕ δ₂  = skipₕ (δ₁ ⦂ₕ δ₂)
idₕ       ⦂ₕ keepₕ δ₂  = keepₕ δ₂
keepₕ δ₁  ⦂ₕ keepₕ δ₂  = keepₕ (δ₁ ⦂ₕ δ₂)
skipₕ δ₁  ⦂ₕ keepₕ δ₂  = skipₕ (δ₁ ⦂ₕ δ₂)
```

... except that we have extra cases
which deal with an extra equality we threw in.

We have to show that equations are consistent
with `keep-id≡idₕ` equality.
The goals may be obfuscated, but relatively easy to fill.

```agda
keep-id≡idₕ n i ⦂ₕ keepₕ δ₂ = goal i
  where
  lemma : ∀ {n m} → (δ : HIT n m) → idₕ ⦂ₕ δ ≡ δ
  lemma idₕ = refl
  lemma (keepₕ δ) = refl
  lemma (skipₕ δ) = cong skipₕ (lemma δ)
  lemma (keep-id≡idₕ n i) j = keep-id≡idₕ n i

  goal : keepₕ (idₕ ⦂ₕ δ₂) ≡ keepₕ δ₂
  goal i = keepₕ (lemma δ₂ i)

idₕ               ⦂ₕ keep-id≡idₕ n i = keep-id≡idₕ n i
keepₕ δ₁          ⦂ₕ keep-id≡idₕ n i = keepₕ δ₁
skipₕ δ₁          ⦂ₕ keep-id≡idₕ n i = skipₕ δ₁
keep-id≡idₕ .n i  ⦂ₕ keep-id≡idₕ n j = goal i j
  where
   goal : Square refl (keep-id≡idₕ n) refl (keep-id≡idₕ n)
   goal i j = keep-id≡idₕ n (i ∧ j)
```

We can try to prove that the HIT variant is the same as orthodox one.
The conversion functions are extremely simple,
because the data-type is almost the same:

```agda
Orth→HIT : n ⊑ₒ m → n ⊑ₕ m
Orth→HIT nilₒ      = idₕ
Orth→HIT (keepₒ δ) = keepₕ (Orth→HIT δ)
Orth→HIT (skipₒ δ) = skipₕ (Orth→HIT δ)

HIT→Orth : n ⊑ₕ m → n ⊑ₒ m
HIT→Orth idₕ                = idₒ
HIT→Orth (keepₕ δ)          = keepₒ (HIT→Orth δ)
HIT→Orth (skipₕ δ)          = skipₒ (HIT→Orth δ)
HIT→Orth (keep-id≡idₕ n i)  = keep-id≡idₒ {n} i
```

Converting orthodox representation to HIT and back doesn't change the thinning.
The proof is straightforward structural induction.

```agda
Orth→HIT→Orth : (δ : Orth n m) → HIT→Orth (Orth→HIT δ) ≡ δ
Orth→HIT→Orth nilₒ       = refl
Orth→HIT→Orth (keepₒ δ)  = cong keepₒ (Orth→HIT→Orth δ)
Orth→HIT→Orth (skipₒ δ)  = cong skipₒ (Orth→HIT→Orth δ)
```

On the other hand the opposite direction is tricky.

Easy part is to show that `Orth→HIT` preserves the identity,
that will show that `idₕ` roundtrips.

```agda
Orth→HIT-id : ∀ n → Orth→HIT idₒ ≡ idₕ {n}
Orth→HIT-id zero     = refl
Orth→HIT-id (suc n)  = cong keepₕ (Orth→HIT-id n) ∙ keep-id≡idₕ n
```

We also have to show that `keep-id≡idₕ` roundtrips.
This is considerably more challenging.
Luckily if you squint enough (and are familiar with `cubical` library),
you notice the pattern:

```agda
lemma : ∀ n → Square
  (cong keepₕ (Orth→HIT-id n))
  (cong keepₕ (Orth→HIT-id n) ∙ keep-id≡idₕ n)
  (refl {x = keepₕ (Orth→HIT idₒ)})
  (keep-id≡idₕ n)
lemma n = compPath-filler
  {x = keepₕ (Orth→HIT idₒ)}
  (cong keepₕ (Orth→HIT-id n))
  (keep-id≡idₕ n)
```

(In general, proving the equalities about equalities in Cubical Agda, i.e. filling squares and cubes feels to be black magic).

Using these lemmas we can finish the equality proof:

```agda
HIT→Orth→HIT : (δ : HIT n m) → Orth→HIT (HIT→Orth δ) ≡ δ
HIT→Orth→HIT idₕ                  = Orth→HIT-id _
HIT→Orth→HIT (keepₕ δ)            = cong keepₕ (HIT→Orth→HIT δ)
HIT→Orth→HIT (skipₕ δ)            = cong skipₕ (HIT→Orth→HIT δ)
HIT→Orth→HIT (keep-id≡idₕ n i) j  = lemma n i j

Orth≡HIT-pointwise : n ⊑ₒ m ≡ n ⊑ₕ m
Orth≡HIT-pointwise =
  isoToPath (iso Orth→HIT HIT→Orth HIT→Orth→HIT Orth→HIT→Orth)

Orth≡HIT : Orth ≡ HIT
Orth≡HIT i n m = Orth≡HIT-pointwise {n} {m} i
```

And we can show that this thinning identity and composition behave as the orthodox one.
The identity homomorphism we have already proven,
composition is trivial as the HIT structure resembles the structure orthodox thinning:

```agda
Orth→HIT-⦂ : ∀ {n m p} (δ₁ : Orth n m) (δ₂ : Orth m p)
  → Orth→HIT (δ₁ ⦂ₒ δ₂) ≡ Orth→HIT δ₁ ⦂ₕ Orth→HIT δ₂
Orth→HIT-⦂ δ₁           nilₒ       = refl
Orth→HIT-⦂ δ₁          (skipₒ δ₂)  = cong skipₕ (Orth→HIT-⦂ δ₁ δ₂)
Orth→HIT-⦂ (keepₒ δ₁)  (keepₒ δ₂)  = cong keepₕ (Orth→HIT-⦂ δ₁ δ₂)
Orth→HIT-⦂ (skipₒ δ₁)  (keepₒ δ₂)  = cong skipₕ (Orth→HIT-⦂ δ₁ δ₂)
```

Then we can repeat what we did with previous thinning.

```agda
CatOps-HIT : CatOps HIT
CatOps-HIT = idₕ , _⦂ₕ_

CatOps-HITₜ : CatOps HIT
CatOps-HITₜ = subst CatOps Orth≡HIT CatOps-Orth
```

Identities are equal:

```agda
idₕₜ : n ⊑ₕ n
idₕₜ = fst CatOps-HITₜ

idₕₜ≡idₕ : idₕₜ ≡ idₕ {n}
idₕₜ≡idₕ = transportRefl (Orth→HIT idₒ) ∙ Orth→HIT-id _
```

and composition (literally the same code as in previous section, it can be automated but it's not worth for a blog post)

```agda
_⦂ₕₜ_ : n ⊑ₕ m → m ⊑ₕ p → n ⊑ₕ p
_⦂ₕₜ_ = snd CatOps-HITₜ

⦂ₕₜ≡⦂ₕ : (δ₁ : n ⊑ₕ m) (δ₂ : m ⊑ₕ p) → δ₁ ⦂ₕₜ δ₂ ≡ δ₁ ⦂ₕ δ₂
⦂ₕₜ≡⦂ₕ {n} {m} {p} δ₁ δ₂ =
  transport refl expr₁  ≡⟨ transportRefl expr₁ ⟩
  expr₁                 ≡⟨ expr₁≡expr₂ ⟩
  expr₂                 ≡⟨ Orth→HIT-⦂ (HIT→Orth δ₁) (HIT→Orth δ₂) ⟩
  expr₃                 ≡⟨ (λ i → HIT→Orth→HIT δ₁ i ⦂ₕ HIT→Orth→HIT δ₂ i) ⟩
  δ₁ ⦂ₕ δ₂ ∎
  where
    expr₁ = Orth→HIT (HIT→Orth (transport refl δ₁) ⦂ₒ
                      HIT→Orth (transport refl δ₂))
    expr₂ = Orth→HIT (HIT→Orth δ₁ ⦂ₒ HIT→Orth δ₂)
    expr₃ = Orth→HIT (HIT→Orth δ₁) ⦂ₕ Orth→HIT (HIT→Orth δ₂)

    expr₁≡expr₂ : expr₁ ≡ expr₂
    expr₁≡expr₂ i = Orth→HIT (HIT→Orth (transportRefl δ₁ i) ⦂ₒ
                              HIT→Orth (transportRefl δ₂ i))
```

And the equalities of `CatOps`:

```agda
CatOps-HIT≡ : CatOps-HITₜ ≡ CatOps-HIT
CatOps-HIT≡ i = idₕₜ≡idₕ i , λ δ₁ δ₂ → ⦂ₕₜ≡⦂ₕ δ₁ δ₂ i

CatOps-Orth≡HIT : (λ i → CatOps (Orth≡HIT i)) [ CatOps-Orth ≡ CatOps-HIT ]
CatOps-Orth≡HIT = toPathP CatOps-HIT≡
```

Conclusion
----------

We have seen three definitions of thinnings. Orthodox one, one with identity constructor yet unique representation
and variant using additional equality.
Using Cubical Agda we verified that these three definitions are equal,
and their identity and composition behave the same.

What we can learn from it?

Well. It is morally correct to define

```haskell
data Thin n m where
  ThinId   ::             Thin    n     n
  ThinSkip :: Thin n m -> Thin    n  (S m)
  ThinKeep :: Thin n m -> Thin (S n) (S m)
```

as long as you pay attention to not differentiate between `ThinKeep ThinId` and `ThinId`, you are safe.
GHC won't point you if you wrote something inconsistent.

For example checking whether the thinning is an identity:

```haskell
isThinId :: Thin n m -> Maybe (n :~: m)
isThinId ThinId = Just Refl
isThinId _      = Nothing
```

is not correct, but will be accepted by GHC. (Won't be by Cubical Agda).

But if you don't trust yourself, you can go for slightly more complicated

```haskell
data Thin n m where
  ThinId ::              Thin n n
  Thin'  :: Thin' n m -> Thin n m

data Thin' n m where
  ThinWk   ::              Thin'    n  (S n)
  ThinSkip :: Thin' n m -> Thin'    n  (S m)
  ThinKeep :: Thin' n m -> Thin' (S n) (S m)
```

In either case you will be able to write `Category` instance:

```haskell
instance Category Thin where
  id = ThinId
  (.) = _look_above_in_the_Agda_Code
```

which is not possible with an orthodox thinning definition.

Extras
------

```agda
open import Cubical.Data.Nat.Order

-- thinnings can be converted to less-than-or-equal-to relation:
⊑ₕ→≤ : n ⊑ₕ m → n ≤ m
⊑ₕ→≤ idₕ = 0 , refl
⊑ₕ→≤ (keepₕ δ) with ⊑ₕ→≤ δ
... | n , p = n  , +-suc n _ ∙ cong suc p
⊑ₕ→≤ (skipₕ δ) with ⊑ₕ→≤ δ
... | n , p = suc n , cong suc p
⊑ₕ→≤ (keep-id≡idₕ n i) = lemma' i where
  lemma' : ⊑ₕ→≤ (keepₕ idₕ) ≡ ⊑ₕ→≤ (idₕ {suc n})
  lemma' = Σ≡Prop (λ m  → isSetℕ (m + suc n) (suc n)) (refl {x = 0})

-- Then we can check whether thinning is an identity.
-- Agda forces us to not cheat.
-- (Well, and also → Dec (n ≡ m))
isThinId : n ⊑ₕ m → Dec (n ≡ m)
isThinId idₕ = yes refl
isThinId (keepₕ δ) with isThinId δ
... | yes p = yes (cong suc p)
... | no ¬p = no λ p → ¬p (injSuc p)
isThinId {n} {m} (skipₕ δ) with ⊑ₕ→≤ δ
... |  (r , p) = no λ q → ¬m+n<m {m = n} {n = 0}
  (r , (r + suc (n + 0)    ≡⟨ +-suc r (n + 0) ⟩
        suc (r + (n + 0))  ≡⟨ cong (λ x → suc (r + x)) (+-zero n) ⟩
        suc (r + n)        ≡⟨ cong suc p ⟩
        suc _              ≡⟨ sym q ⟩
        n                  ∎))

isThinId (keep-id≡idₕ n i) = yes (λ _ → suc n)

-- Same for orthodox
⊑ₒ→≤ : n ⊑ₒ m → n ≤ m
⊑ₒ→≤ nilₒ = 0 , refl
⊑ₒ→≤ (skipₒ δ) with ⊑ₒ→≤ δ
... | n , p = suc n , cong suc p
⊑ₒ→≤ (keepₒ δ) with ⊑ₒ→≤ δ
... | n , p = n  , +-suc n _ ∙ cong suc p

-- if indices match, δ is idₒ
⊥-elim : {A : Type} → ⊥ → A
⊥-elim ()

idₒ-unique : (δ : n ⊑ₒ n) → δ ≡ idₒ
idₒ-unique nilₒ      = refl
idₒ-unique (skipₒ δ) = ⊥-elim (¬m<m (⊑ₒ→≤ δ))
idₒ-unique (keepₒ δ) = cong keepₒ (idₒ-unique δ)

-- or idₕ, for which direct proof is trickier.
idₕ-unique : (δ : n ⊑ₕ n) → δ ≡ idₕ
idₕ-unique {n} = subst {A = Σ _ CatOps}
  (λ { (_⊑_ , (id , _⦂_)) → (δ : n ⊑ n) → δ ≡ id})
  (λ i → Orth≡HIT i , CatOps-Orth≡HIT i)
  idₒ-unique
```

More extras
-----------

The most important operation thinning support is their action on variables.

```agda
data Var : ℕ → Type where
  vz :         Var (suc n)
  vs : Var n → Var (suc n)
```

Using each of the variants let us define the action:

```agda
thinₒ : n ⊑ₒ m → Var n → Var m
thinₒ nilₒ      ()
thinₒ (skipₒ δ) x      = vs (thinₒ δ x)
thinₒ (keepₒ δ) vz     = vz
thinₒ (keepₒ δ) (vs x) = vs (thinₒ δ x)

thinₛ : n ⊏ₛ m → Var n → Var m
thinₛ wkₛ       x      = vs x
thinₛ (skipₛ δ) x      = vs (thinₛ δ x)
thinₛ (keepₛ δ) vz     = vz
thinₛ (keepₛ δ) (vs x) = vs (thinₛ δ x)

thinₙ : n ⊑ₙ m → Var n → Var m
thinₙ idₙ        x = x
thinₙ (strict δ) x = thinₛ δ x
```

It's worth noticing that HIT forces to take into account the `keep≡id≡idₕ`
equality, so we cannot do silly stuff in `keepₕ` cases.

```agda
thinₕ : n ⊑ₕ m → Var n → Var m
thinₕ idₕ       x      = x
thinₕ (skipₕ δ) x      = vs (thinₕ δ x)
thinₕ (keepₕ δ) vz     = vz
thinₕ (keepₕ δ) (vs x) = vs (thinₕ δ x)

thinₕ (keep-id≡idₕ n i) vz     = vz
thinₕ (keep-id≡idₕ n i) (vs x) = vs x
```

Let us prove that these definitions are compatible.
First we need a simple lemma, that `thinₒ idₒ` is an identity function.

```agda
thin-idₒ : (x : Var n) → thinₒ idₒ x ≡ x
thin-idₒ {suc n} vz     = refl
thin-idₒ {suc n} (vs x) = cong vs (thin-idₒ x)
```

```agda
Action : ℕ → ℕ → (ℕ → ℕ → Type) → Type
Action n m _⊑_ = n ⊑ m → Var n → Var m

thinₙₜ : n ⊑ₙ m → Var n → Var m
thinₙₜ {n} {m} = subst (Action n m) Orth≡NonStr thinₒ

Strict→Orth-thin : (δ : n ⊏ₛ m) (x : Var n) → thinₒ (Strict→Orth δ) x ≡ thinₛ δ x
Strict→Orth-thin wkₛ       x      = cong vs (thin-idₒ x)
Strict→Orth-thin (skipₛ δ) x      = cong vs (Strict→Orth-thin δ x)
Strict→Orth-thin (keepₛ δ) vz     = refl
Strict→Orth-thin (keepₛ δ) (vs x) = cong vs (Strict→Orth-thin δ x)

NonStr→Orth-thin : (δ : n ⊑ₙ m) (x : Var n) → thinₒ (NonStr→Orth δ) x ≡ thinₙ δ x
NonStr→Orth-thin idₙ        x = thin-idₒ x
NonStr→Orth-thin (strict δ) x = Strict→Orth-thin δ x

thinₙₜ≡thinₙ-pointwise : (δ : n ⊑ₙ m) (x : Var n) → thinₙₜ δ x ≡ thinₙ δ x
thinₙₜ≡thinₙ-pointwise {n} {m} δ x
  = transportRefl (thinₒ (NonStr→Orth (transp (λ i → n ⊑ₙ m) i0 δ)) (transp (λ j → Var n) i0 x))
  ∙ cong₂ thinₒ (cong NonStr→Orth (transportRefl δ)) (transportRefl x)
  ∙ NonStr→Orth-thin δ x

thinₙₜ≡thinₙ : (thinₙₜ {n} {m}) ≡ thinₙ
thinₙₜ≡thinₙ i δ x = thinₙₜ≡thinₙ-pointwise δ x i

thinₒ≡thinₙ : (λ i → Action n m (Orth≡NonStr i)) [ thinₒ ≡ thinₙ ]
thinₒ≡thinₙ = toPathP thinₙₜ≡thinₙ
```

The HIT version is not much trickier, if any.

```agda
thinₕₜ : n ⊑ₕ m → Var n → Var m
thinₕₜ {n} {m} = subst (Action n m) Orth≡HIT thinₒ

HIT→Orth-thin : (δ : n ⊑ₕ m) (x : Var n) → thinₒ (HIT→Orth δ) x ≡ thinₕ δ x
HIT→Orth-thin idₕ       x      = thin-idₒ x
HIT→Orth-thin (skipₕ δ) x      = cong vs (HIT→Orth-thin δ x)
HIT→Orth-thin (keepₕ δ) vz     = refl
HIT→Orth-thin (keepₕ δ) (vs x) = cong vs (HIT→Orth-thin δ x)

HIT→Orth-thin (keep-id≡idₕ n i) vz     = refl
HIT→Orth-thin (keep-id≡idₕ n i) (vs x) = cong vs (thin-idₒ x)

thinₕₜ≡thinₕ-pointwise : (δ : n ⊑ₕ m) (x : Var n) → thinₕₜ δ x ≡ thinₕ δ x
thinₕₜ≡thinₕ-pointwise {n} {m} δ x
  = transportRefl (thinₒ (HIT→Orth (transp (λ i → n ⊑ₕ m) i0 δ)) (transp (λ j → Var n) i0 x))
  ∙ cong₂ thinₒ (cong HIT→Orth (transportRefl δ)) (transportRefl x)
  ∙ HIT→Orth-thin δ x

thinₕₜ≡thinₕ : (thinₕₜ {n} {m}) ≡ thinₕ
thinₕₜ≡thinₕ i δ x = thinₕₜ≡thinₕ-pointwise δ x i

thinₒ≡thinₕ : (λ i → Action n m (Orth≡HIT i)) [ thinₒ ≡ thinₕ ]
thinₒ≡thinₕ = toPathP thinₕₜ≡thinₕ
```

At the end we have three variants of thinnings with identity and composition, and which act on variables the same way.

Now, if we prove properties of these operations,
e.g. identity laws, composition associativity, or that composition and action commute, it would be enough
to prove these for the orthodox implementation, then we can simply transport the proofs.

In other words, whatever we prove about one structure will hold for two others.
