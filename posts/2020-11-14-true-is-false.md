---
title: true is false
author: Oleg Grenrus
tags: agda
---

```agda
{-# OPTIONS --cubical --safe #-}
module TrueFalse where

open import Cubical.Foundations.Everything
open import Cubical.Data.Bool

-- The following is basic stuff
true≡true : true ≡ true
true≡true = refl

-- in Cubical Agda _≡_ isn't a primitive, but a (constant) path
true≡true₂ : Path Bool true true
true≡true₂ = refl

-- but we can also have dependent paths.
eq-Bool : Path Type₀ Bool Bool
eq-Bool = refl

-- i.e. paths which endpoints are values of
-- types of another path by another path endpoints
-- (and "sadly", we have to eta-expand the path)
true≡true₃ : PathP (λ i → eq-Bool i) true true
true≡true₃ = refl

-- Additionally, Cubical Agda allows to say that
-- Booleans are equal via the not-isomorphism
-- (this is notEq in cubical library)
eq-not-Bool : Bool ≡ Bool
eq-not-Bool = isoToPath (iso not not notnot notnot)

-- And then, one can write what may look like non-sense
true≡false : PathP (λ i → eq-not-Bool i) true false
true≡false = toPathP refl

-- "Unfortunately" Agda makes that all a bit to explicit,
-- otherwise you could see just
--
--   true≡false : PathP ... true false
--   true≡false = ... refl
--
-- and get very confused.
-- Devil is in the details (of dependent paths).
```
