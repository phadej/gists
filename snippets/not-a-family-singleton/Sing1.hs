{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Sing1 where

import Data.Kind (Type)
import Data.Nat (Nat (..))
import qualified Data.Type.Nat as N

type family Sing (a :: k) :: Type
type instance Sing (n :: Nat) = N.SNat n

class SingI a where
    sing :: Sing a

instance N.SNatI n => SingI (n :: Nat) where
    sing = N.snat

-- |
--
-- >>> ex01
-- SS
--
-- >>> N.snatToNatural ex01
-- 2
--
ex01 :: Sing N.Nat2
ex01 = sing @N.Nat2
