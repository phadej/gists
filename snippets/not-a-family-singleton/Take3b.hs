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
module Take3b where

import Data.Kind (Type, Constraint)
import Data.Proxy
import qualified Data.Type.Nat as N

import Sing1ii

-------------------------------------------------------------------------------
-- List singleton
-------------------------------------------------------------------------------

data SList :: (x -> Constraint) -> [x] -> Type where
    SNil  :: SList s '[]
    SCons :: (s x, SListI s xs) => SList s (x ': xs)

deriving instance Show (SList s xs)

class SListI (s :: k -> Constraint) (xs :: [k]) where
    slist :: SList s xs

instance SListI s '[] where
    slist = SNil

instance (s x, SListI s xs) => SListI s (x ': xs) where
    slist = SCons

type instance Sing (xs :: [k]) = SList SingI xs
instance SListI SingI xs => SingI (xs :: [k]) where
    sing = slist 

-- |
--
-- >>> ex02
-- SCons
ex02 :: Sing [N.Nat1, N.Nat2]
ex02 = sing @[N.Nat1, N.Nat2]

-------------------------------------------------------------------------------
-- Induction
-------------------------------------------------------------------------------

induction
    :: forall c f xs. SListI c xs
    => Proxy c
    -> f '[]
    -> (forall y ys. (c y, SListI c ys) => f ys -> f (y ': ys))
    -> f xs
induction pc n c = case slist :: SList c xs of
    SNil  -> n
    SCons -> c (induction pc n c)

class SListI c xs => Induction (c :: k -> Constraint) (xs :: [k]) where
    ind :: Proxy c
        -> f '[]
        -> (forall y ys. (c y, Induction c ys) => f ys -> f (y ': ys))
        -> f xs

instance Induction c '[] where
    ind _ n _ = n

instance (c x, Induction c xs) => Induction c (x ': xs) where
    ind p n c = c (ind p n c)

slist' :: forall c xs. Induction c xs => SList c xs
slist' = ind (Proxy :: Proxy c) SNil (\_ -> SCons)
