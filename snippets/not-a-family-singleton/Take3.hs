{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Take3 where

import Data.Kind (Type, Constraint)
import qualified Data.Type.Nat as N

import Sing1i

-------------------------------------------------------------------------------
-- List singleton
-------------------------------------------------------------------------------

data SList :: (x -> Constraint) -> [x] -> Type where
    SNil  :: SList s '[]
    SCons :: s x => SList s xs -> SList s (x ': xs)

deriving instance Show (SList s xs)

class SListI (s :: k -> Constraint) (xs :: [k]) where
    slist :: SList s xs

instance SListI s '[] where
    slist = SNil

instance (s x, SListI s xs) => SListI s (x ': xs) where
    slist = SCons slist

type instance Sing (xs :: [k]) = SList SingI xs
instance SListI SingI xs => SingI (xs :: [k]) where
    sing = slist 

-- |
--
-- >>> ex03
-- SCons (SCons SNil)
ex03 :: Sing [N.Nat1, N.Nat2]
ex03 = sing @[N.Nat1, N.Nat2]

