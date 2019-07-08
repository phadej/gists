{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Take2 where

import Data.Kind (Type, Constraint)
import Data.Nat (Nat (..))

import qualified Data.Type.Nat as N

type family Sing (a :: k) :: Type
type instance Sing (n :: Nat) = N.SNat n

type family SingKind (k :: Type) :: k -> Type
type instance SingKind  N.Nat = N.SNat

class SingI (a :: k) where
    sing :: SingKind k a

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
ex01 = sing @_ @N.Nat2

-------------------------------------------------------------------------------
-- List singleton
-------------------------------------------------------------------------------

data SList :: (x -> Type) -> [x] -> Type where
    SNil  :: SList s '[]
    SCons :: s x -> SList s xs -> SList s (x ': xs)

type instance SingKind [x]     = SList (SingKind x)
type instance Sing (xs :: [k]) = SList (SingKind k) xs

instance SingI '[] where sing = SNil
instance (SingI x, SingI xs) => SingI (x ': xs) where
    sing = SCons (sing @_ @x) (sing @_ @xs)
