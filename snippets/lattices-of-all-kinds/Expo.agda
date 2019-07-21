module Expo where

open import Data.Nat
open import Data.Vec
open import Data.Product

exp : ℕ → ℕ → ℕ
exp n zero    = 1
exp n (suc m) = n * exp n m

concatMap : ∀ {n m} {A B : Set} → Vec A n → (A → Vec B m) → Vec B (n * m)
concatMap []       f = []
concatMap (x ∷ xs) f = f x ++ concatMap xs f

pairs : ∀ {n m} {A B : Set} → Vec A n → Vec B m → Vec (Vec B n) (exp m n)
pairs []       bs = [] ∷ []
pairs (_ ∷ as) bs = concatMap bs λ b → Data.Vec.map (b ∷_) rec 
  where
    rec = pairs as bs

three : Vec ℕ 3
three = 1 ∷ 2 ∷ 3 ∷ []
