{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Either where

import Data.Kind (Type, Constraint)
import qualified Data.Type.Nat as N

import Sing1ii

data SEither :: (x -> Constraint) -> (y -> Constraint) -> Either x y -> Type where
    SLeft  :: l x => SEither l r ('Left x)
    SRight :: r y => SEither l r ('Right y)

deriving instance Show (SEither l r e)

class           SEitherI l r e          where seither :: SEither l r e
instance l x => SEitherI l r ('Left x)  where seither = SLeft
instance r y => SEitherI l r ('Right y) where seither = SRight

-------------------------------------------------------------------------------
-- Singleton instances
-------------------------------------------------------------------------------

type instance Sing (e :: Either x y) = SEither SingI SingI e
instance SEitherI SingI SingI e => SingI (e :: Either x y) where
    sing = seither

-- |
--
-- >>> ex03
-- SLeft
--
ex03 :: Sing ('Left N.Nat2)
ex03 = sing @('Left N.Nat2)
