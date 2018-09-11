---
title: Linear Lambda Calculus in Agda
author: Oleg Grenrus
tags: linear, agda
---

Haskell by day, Agda by Night.

In comparison with
[other implementation found by googling](https://github.com/wenkokke/SubstructuralLogicsInAgda/blob/master/src/LinearLogic.agda)
this approach feels simpler.

Philip Wadler wrote a paper [A taste of linear logic](https://homepages.inf.ed.ac.uk/wadler/papers/lineartaste/lineartaste-revised.pdf) (1993),
if you want to learn what this is about.

You can compare the code below with Oleg Kiselyov's [LinearLC.hs](http://okmij.org/ftp/tagless-final/course/LinearLC.hs),
or with *Embedding a Full Linear Lambda Calculus in Haskell*
by
Jeff Polakow ([Available via OpenTOC](http://www.sigplan.org/OpenTOC/haskell15.html))
and *Embedding session types in Haskell* by
Sam Lindley and J. Garrett Morris ([Available via OpenTOC](http://www.sigplan.org/OpenTOC/haskell16.html)).
(Note: my encoding isn't HOAS, but uses de Bruijn indices; encodings above are HOAS, but also use de Bruijn indices to track linearity).

This time it's only code, your comments welcome:
[reply to my tweet](https://twitter.com/phadej/status/1017180233219411968),
[email me](mailto:oleg.grenrus@iki.fi)
or find me on IRC (`phadej` on Freenode).

```agda
-- An encoding of Linear Lambda Calculus
--
-- The idea is from Conor McBride's /I Got Plenty o’ Nuttin’/
-- The variables stay in the context,
-- but "consuming" them uses their multiplicity.
module LLC where

open import Data.Nat
open import Data.List

-- Types
data Ty : Set where
  A   : ℕ → Ty  -- "atomic" propositions
  _⊸_ : Ty → Ty → Ty
  _⊗_ : Ty → Ty → Ty

infixr 0 _⊸_
infixr 1 _⊗_

-- None-One-Tons Rig
data R : Set where
  none : R
  one  : R
--  tons : R -- Add if !_ type is introduced.
             -- Left as an exercise

-- Context
Ctx = List Ty

-- Multiplicities of the context
data M : Ctx → Set where
   []  : M []
   _∷_ : R → {t : Ty} → {ts : Ctx} → M ts → M (t ∷ ts) 

infixr 5 _∷_

-- Variable of type t
data Var (t : Ty) : (Γ : Ctx) → M Γ → M Γ → Set where
  -- variable consumes multiplicity
  zero  : ∀ {Γ m}
    → Var t (t ∷ Γ) (one ∷ m) (none ∷ m)
  -- somewhere else in the context
  succ  : ∀ {t′ Γ m n p}
    → Var t Γ n p
    → Var t (t′ ∷ Γ) (m ∷ n) (m ∷ p)

data Term : (Γ : Ctx) → M Γ → M Γ → Ty → Set where
  var : ∀ {a Γ m n}
    → Var a Γ m n
    → Term Γ m n a

  -- linear implication
  app : ∀ {a b Γ m n p}
    → Term Γ m n (a ⊸ b)
    → Term Γ n p a
    → Term Γ m p b
  lam : ∀ {a b Γ m n}
    → Term (a ∷ Γ) (one ∷ m) (none ∷ n) b
    → Term Γ m n (a ⊸ b)

  -- tensor (pair)
  tensI : ∀ {a b Γ m n p}
    → Term Γ m n a
    → Term Γ n p b
    → Term Γ m p (a ⊗ b)
  tensE : ∀ {a b c Γ m n p}
    → Term Γ m n (a ⊗ b)
    → Term (a ∷ b ∷ Γ) (one ∷ one ∷ n) (none ∷ none ∷ p) c
    → Term Γ m p c

-- Variable helpers
var0 : ∀ {a Γ m} → Term (a ∷ Γ) (one ∷ m) (none ∷ m) a
var0 = var zero

var1 : ∀ {a x Γ m xᵣ}
     → Term
       (x ∷ a ∷ Γ)
       (xᵣ ∷ one ∷ m)
       (xᵣ ∷ none ∷ m)
       a
var1 = var (succ zero)

var2 : ∀ {a x y Γ m xᵣ yᵣ}
     → Term
       (x ∷ y ∷ a ∷ Γ)
       (xᵣ ∷ yᵣ ∷ one ∷ m)
       (xᵣ ∷ yᵣ ∷ none ∷ m)
       a
var2 = var (succ (succ zero))

var3 : ∀ {a x y z Γ m xᵣ yᵣ zᵣ}
     → Term (x ∷ y ∷ z ∷ a ∷ Γ)
       (xᵣ ∷ yᵣ ∷ zᵣ ∷ one ∷ m)
       (xᵣ ∷ yᵣ ∷ zᵣ ∷ none ∷ m)
       a
var3 = var (succ (succ (succ zero)))

--  Examples

ClosedTerm = Term [] [] []

-- (A ⊸ B ⊸ C) ⊸ (B ⊸ A ⊸ C)
-- or using naturals for atomic propositions:
-- (0 ⊸ 1 ⊸ 2) ⊸ (1 ⊸ 0 ⊸ 2)
--
-- λ f x y ⊸ f y x
exFlip : ClosedTerm ((A 0 ⊸ A 1 ⊸ A 2) ⊸ (A 1 ⊸ A 0 ⊸ A 2))
exFlip = lam (lam (lam (app (app var2 var0) var1)))

-- λ f x y ⊸ f (x , y)
exCurry : ClosedTerm ((A 0 ⊗ A 1 ⊸ A 2) ⊸ (A 0 ⊸ A 1 ⊸ A 2))
exCurry = lam (lam (lam (app var2 (tensI var1 var0))))

-- λ f p ⊸ let (x , y) = p in f x y
exUncurry : ClosedTerm ((A 0 ⊸ A 1 ⊸ A 2) ⊸ A 0 ⊗ A 1 ⊸ A 2)
exUncurry = lam (lam (tensE var0 (app (app var3 var0) var1)))

-- type error example:
--
-- We cannot write
-- λ f x ⊸ f x x
-- (but we can in STLC: λ f x → f x x)
exJoin : ClosedTerm ((A 0 ⊸ A 0 ⊸ A 1) ⊸ A 0 ⊸ A 1)
exJoin = lam (lam (app (app var1 var0) {!!}))
-- Goal:
--
-- Term
--   (A 0 ∷ (A 0 ⊸ A 0 ⊸ A 1) ∷ [])
--   (none ∷ none ∷ [])
--   (none ∷ none ∷ [])
--   (A 0)
```
