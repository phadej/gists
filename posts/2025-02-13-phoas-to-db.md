-------------------------------
title: PHOAS to de Bruijn conversion
author: Oleg Grenrus
tags: agda
-------------------------------

Recently I looked again at PHOAS, and once again I concluded it's nice
for library APIs, but so painful to do anything with inside those libraries.
So let convert to something else, like de Bruijn.

There are standalone source files if you just want to see the code:

- Agda: https://github.com/phadej/nbexp/blob/master/src/NbEXP/SelfContained/Conv.agda
- Haskell: https://github.com/phadej/nbexp/blob/master/src-hs/Conv.hs


How to convert PHOAS terms to de Bruijn terms?

The solution is hard to find.

You can cheat, [as mentioned by Roman on Agda mailing list https://lists.chalmers.se/pipermail/agda/2018/010033.html]:

<blockquote>
There is always a way to cheat, though. You can turn the PHOAS ->
untyped de Bruijn machinery into the PHOAS -> typed de Bruijn
machinery by checking that future contexts indeed extend past contexts
and throwing an error otherwise (which can't happed, because future
contexts always extend past contexts, but it's a metatheorem).
</blockquote>

In "Generic Conversions of Abstract Syntax Representation"
by Steven Keuchel and Johan Jeuring, authors also "cheat" a bit.
The "Parametrhic higher-order abstract syntax" section
ends with a somewhat disappointing

```agda
  where postulate whatever : _
```

Keuchel and Jeuring also mention "Unembedding Domain-Specific Languages"
by Robert Atkey, Sam Lindley and Jeremy Yallop;
where there is one unsatisfactory ⊥ (undefined in Haskell) hiding.

I think that for practical developments (say a library in Haskell),
it is ok to make a small short cut; but I kept wondering
isn't there is a way to make a conversion without cheating.

Well... it turns out that we cannot "cheat".
Well-formedness of PHOAS representation depends on parametricity,
and the conversion challenge seems to requires a theorem which there are no proof
in Agda.

In unpublished (?) work Adam Chlipala shows a way to do the
conversion without relying on postulates
http://adam.chlipala.net/cpdt/html/Intensional.html;
but that procedure requires an extra well formedness proof
of given PHOAS term.
 
This Agda development is a translation of that developement.

<div id="toc"></div>

Common setup
------------

Our syntax representations will be well-typed, so we need types:

```agda
-- Types
data Ty : Set where
  emp : Ty
  fun : Ty → Ty → Ty

Ctx : Set
Ctx = List Ty

variable
  A B C : Ty
  Γ Δ Ω : Ctx
  v : Ty → Set
```

de Bruijn syntax
-----------------------------------------------------------------------------

```agda
Var : Ctx → Ty → Set
Var Γ A = Idx A Γ -- from agda-np, essentially membership relation.

data DB (Γ : Ctx) : Ty → Set where
  var : Var Γ A → DB Γ A
  app : DB Γ (fun A B) → DB Γ A → DB Γ B
  lam : DB (A ∷ Γ) B → DB Γ (fun A B)
  abs : DB Γ emp → DB Γ A
```

Parametric Higher-order abstract syntax
-----------------------------------------------------------------------------

```agda
data PHOAS (v : Ty → Set) : Ty → Set where
  var : v A → PHOAS v A
  app : PHOAS v (fun A B) → PHOAS v A → PHOAS v B
  lam : (v A → PHOAS v B) → PHOAS v (fun A B)
  abs : PHOAS v emp → PHOAS v A

-- closed "true" PHOAS terms.
PHOAS° : Ty → Set₁
PHOAS° A = ∀ {v} → PHOAS v A
```

de Bruijn to PHOAS
-----------------------------------------------------------------------------

This direction is trivial.
An anecdotal evidence that de Bruijn representation is easier to
transformation on.

```agda
phoasify : NP v Γ → DB Γ A → PHOAS v A
phoasify γ (var x)   = var (lookup γ x)
phoasify γ (app f t) = app (phoasify γ f) (phoasify γ t)
phoasify γ (lam t)   = lam λ x → phoasify (x ∷ γ) t
phoasify γ (abs t)   = abs (phoasify γ t)
```

Interlude: Well-formedness of PHOAS terms
-----------------------------------------------------------------------------

dam Chlipala defines an equivalence relation between two PHOAS terms,
`exp_equiv` in `Intensional`, `wf` in CPDT book).
e only need a single term well-formedness so can do a little less

The goal is to rule out standalone terms like

```agda
module Invalid where
  open import Data.Unit using (⊤; tt)

  invalid : PHOAS (λ _ → ⊤) emp
  invalid = var tt
```

Terms like `invalid` cannot be values of `PHOAS°`, as all
values of "`v`" inside `PHOAS°` have to originated from `lam`-constructor abstractions.
We really should keep `v` parameter free, i.e. parametric, when *constructing*
`PHOAS` terms.

The idea is then to simply to track which variables (values of `v`) are intoduced
by lambda abstraction.

```
data phoasWf {v : Ty → Set} (G : List (Σ Ty v)) : {A : Ty} → PHOAS v A → Set
 where
  varWf : ∀ {A} {x : v A}
    → Idx (A , x) G
    → phoasWf G (var x)
  appWf : ∀ {A B} {f : PHOAS v (fun A B)} {t : PHOAS v A}
    → phoasWf G f
    → phoasWf G t
    → phoasWf G (app f t)
  lamWf : ∀ {A B} {f : v A → PHOAS v B}
    → (∀ (x : v A) → phoasWf ((A , x) ∷ G) (f x))
    → phoasWf G (lam f)
  absWf : ∀ {A} {t : PHOAS v emp}
    → phoasWf G t
    → phoasWf G (abs {A = A} t)

-- closed terms start with an empty G
phoasWf° : PHOAS° A → Set₁
phoasWf° tm = ∀ {v} → phoasWf {v = v} [] tm
```

A meta theorem is then that all PHOASᵒ terms are well-formed, i.e.

```agda
meta-theorem-proposition : Set₁
meta-theorem-proposition = ∀ {A} (t : PHOAS° A) → phoasWf° t
```

As far as I'm aware this proposition cannot be proved nor refuted in Agda.


de Bruijn to PHOAS translation creates well-formed PHOAS terms.
-----------------------------------------------------------------------------

As a small exercise we can show that `phoasify` of closed de Bruijn
terms creates well-formed PHOAS terms.

```agda
toList : NP v Γ → List (Σ Ty v)
toList []       = []
toList (x ∷ xs) = (_ , x) ∷ toList xs

phoasifyWfVar : (γ : NP v Γ) (x : Var Γ A) → Idx (A , lookup γ x) (toList γ)
phoasifyWfVar (x ∷ γ) zero    = zero
phoasifyWfVar (x ∷ γ) (suc i) = suc (phoasifyWfVar γ i)

phoasifyWf : (γ : NP v Γ) (t : DB Γ A) → phoasWf (toList γ) (phoasify γ t)
phoasifyWf γ (var x)   = varWf (phoasifyWfVar γ x)
phoasifyWf γ (app f t) = appWf (phoasifyWf γ f) (phoasifyWf γ t)
phoasifyWf γ (lam t)   = lamWf λ x → phoasifyWf (x ∷ γ) t
phoasifyWf γ (abs t)   = absWf (phoasifyWf γ t)

phoasifyWf° : (t : DB [] A) → phoasWf° (phoasify [] t)
phoasifyWf° t = phoasifyWf [] t
```

PHOAS to de Bruijn
-----------------------------------------------------------------------------

The rest deals with the opposite direction.

In Intensional Adam Chlipala uses `v = λ _ → ℕ` instatiation
to make the translation.

I think that in the typed setting using `v = λ _ → Ctx` turns out nicer.

The idea in both is that we instantiate PHOAS variables
to be de Bruijn levels.

```agda
data IsSuffixOf {ℓ} {a : Set ℓ} : List a → List a → Set ℓ where
  refl : ∀ {xs} → IsSuffixOf xs xs
  cons : ∀ {xs ys} → IsSuffixOf xs ys → ∀ {y} → IsSuffixOf xs (y ∷ ys)
```

We need to establish well-formedness of PHOAS expression in relation to some context `Γ`

Note that variables encode de Bruijn levels, thus the
contexts we "remember" in variables should be the suffix of that outside context.

```agda
wf : (Γ : Ctx) → PHOAS (λ _ → Ctx) A → Set
wf {A = A} Γ (var Δ)         = IsSuffixOf (A ∷ Δ) Γ
wf         Γ (app f t)       = wf Γ f × wf Γ t
wf         Γ (lam {A = A} t) = wf (A ∷ Γ) (t Γ)
wf         Γ (abs t)         = wf Γ t
```

And if `(A ∷ Δ)` is suffix of context `Γ`, we can convert the evidence to the
de Bruijn index (i.e. variable):

```agda
makeVar : IsSuffixOf (A ∷ Δ) Γ → Var Γ A
makeVar refl     = zero
makeVar (cons s) = suc (makeVar s)
```

Given the term is well-formed in relation to context Γ
we can convert it to de Bruijn representation.

```agda
dbify : (t : PHOAS (λ _ → Ctx) A) → wf Γ t → DB Γ A
dbify         (var x)   wf        = var (makeVar wf)
dbify         (app f t) (fʷ , tʷ) = app (dbify f fʷ) (dbify t tʷ)
dbify {Γ = Γ} (lam t)   wf        = lam (dbify (t Γ) wf)
dbify         (abs t)   wf        = abs (dbify t wf)
```

What is left is to show that we can construct `wf` for all `phoasWf`-well-formed terms.

Adam Chlipala defines a helper function:

```agda
makeG′ : Ctx → List (Σ Ty (λ _ → Ctx))
makeG′ [] = []
makeG′ (A ∷ Γ) = (A , Γ) ∷ makeG′ Γ
```

However for somewhat technical reasons, we rather define

```agda
expand : (Γ : Ctx) → NP (λ _ → Ctx) Γ
expand []      = []
expand (_ ∷ Γ) = Γ ∷ expand Γ
```

and use `expand` with previously defined `toList` to define our version of `makeG`:

```agda
makeG : Ctx → List (Σ Ty (λ _ → Ctx))
makeG Γ = toList (expand Γ)
```

`makeG` and `makeG′` are the same:

```agda
toList∘expand≡makeG : ∀ Γ → makeG Γ ≡ makeG′ Γ
toList∘expand≡makeG []      = refl
toList∘expand≡makeG (A ∷ Γ) = cong ((A , Γ) ∷_) (toList∘expand≡makeG Γ)
```

Then we can construct `wf` for all `phoasWf`:

```agda`
wfWfVar : Idx (A , Δ) (makeG Γ) → IsSuffixOf (A ∷ Δ) Γ
wfWfVar {Γ = B ∷ Γ} zero    = refl
wfWfVar {Γ = B ∷ Γ} (suc i) = cons (wfWfVar i)

wfWf : (t : PHOAS (λ _ → Ctx) A) → phoasWf (makeG Γ) t → wf Γ t
wfWf         (var x)   (varWf xʷ)    = wfWfVar xʷ
wfWf         (app f t) (appWf fʷ tʷ) = wfWf f fʷ , wfWf t tʷ
wfWf {Γ = Γ} (lam f)   (lamWf fʷ)    = wfWf (f Γ) (fʷ Γ)
wfWf         (abs t)   (absWf tʷ)    = wfWf t tʷ
```

And finally we define `dbifyᵒ` for all well-formed `PHOASᵒ` terms.

```agda
dbify° : (t : PHOAS° A) → phoasWf° t → DB [] A
dbify° t w = dbify t (wfWf t w)
```

Bonus section
-----------------------------------------------------------------------------

We can show that converting closed de Bruijn term to PHOAS and back
is an identity function:

```
bonus-var : (x : Var Γ A) → x ≡ makeVar (wfWfVar (phoasifyWfVar (expand Γ) x))
bonus-var {Γ = A ∷ Γ} zero    = refl
bonus-var {Γ = A ∷ Γ} (suc i) = cong suc (bonus-var i)

bonus : (t : DB Γ A)
      → t ≡ dbify (phoasify (expand Γ) t)
              (wfWf (phoasify (expand Γ) t) (phoasifyWf _ t))
bonus (var x)   = cong var (bonus-var x)
bonus (app f t) = cong₂ app (bonus f) (bonus t)
bonus (lam t)   = cong lam (bonus t)
bonus (abs t)   = cong abs (bonus t)

bonus° : ∀ (t : DB [] A) → t ≡ dbify° (phoasify [] t) (phoasifyWf° t)
bonus° t = bonus t
```
