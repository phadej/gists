{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Take1 where

import Data.Kind (Type)
import qualified Data.Type.Nat as N

import Sing1

-------------------------------------------------------------------------------
-- List singleton
-------------------------------------------------------------------------------

data SList :: [x] -> Type where
    SNil  :: SList '[]
    SCons :: Sing x -> SList xs -> SList (x ': xs)

type instance Sing (xs :: [k]) = SList xs

instance SingI '[] where
    sing = SNil

instance (SingI x, SingI xs) => SingI (x ': xs) where
    sing = SCons (sing @x) (sing @xs)

ex02 :: Sing [N.Nat1, N.Nat2]
ex02 = sing @[N.Nat1, N.Nat2]
