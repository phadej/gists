---
title: Not a family singleton
author: Oleg Grenrus
tags: fancy-types
---

```haskell
{- cabal:
build-depends:
  , base   ^>=4.10 || ^>=4.11
  , fin    ^>=0.0.3
  , vec    ^>=0.1.1
  , tagged ^>=0.8.6
  , text   ^>=1.2.3.0

ghc-options:        -Wall -pgmL markdown-unlit
build-tool-depends: markdown-unlit:markdown-unlit ^>=0.5.0
-}
```

Next a `{-# LANGUAGE Dependent #-}` collection of extensions...

```haskell
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
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures -Wno-unused-imports #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
```

... and imports

```haskell
module Main where

import Control.Monad (forM)
import Data.Bifunctor (first)
import Data.Kind (Type, Constraint)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import Data.Type.Nat (Nat (..))
import Data.Vec.Lazy (Vec (..))
import GHC.Generics
import Text.Read (readMaybe)

import qualified Data.Fin as F
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Type.Nat as N
import qualified Data.Vec.Lazy as V
```


```haskell
type family Sing (a :: k) :: Type
type instance Sing (n :: Nat) = N.SNat n

class SingI a where
    sing :: Sing a  -- SingKind k a

instance N.SNatI n => SingI (n :: Nat) where
    sing = N.snat

-- >>> ex01
-- SS
--
-- >>> N.snatToNatural ex01
-- 2
ex01 :: Sing N.Nat2
ex01 = sing @N.Nat2
```

```{.haskell.ignore}
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

{-
type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllF c '[]       = ()
    AllF c (x ': xs) = (c x, AllF c xs)

class f (g x) => ComposeC f g x
instance (f (g x)) => ComposeC f g x

-- class (AllF c xAll (c :: k -> Constraint) (xs :: [k]) where

-- cannot write, Sing is TypeFamily...
instance AllF (ComposeC Show Sing) xs => Show (SList xs) where
    showsPrec _ SNil = showString "SNil"
    showsPrec d (SCons _ xs) = showParen (d > 10)
        $ showString "SCons <sing> "
        . showsPrec 11 xs
-}  
```

```haskell
{-
data SList :: (x -> Type) -> [x] -> Type where
    SNil  :: SList s '[]
    SCons :: s x -> SList s xs -> SList s (x ': xs)

type family SingKind (k :: Type) :: k -> Type
type instance SingKind  N.Nat = N.SNat
type instance SingKind [x]    = SList (SingKind x)

type instance Sing (xs :: [k]) = SList (SingKind k) xs

instance SingI '[] where sing = SNil
instance (SingI x, SingI xs) => SingI (x ': xs) where
    sing = SCons (sing @_ @x) (sing @_ @xs)
-}
```

```haskell
data SList :: (x -> Constraint) -> [x] -> Type where
    SNil  :: SList s '[]
    SCons :: s x => SList s xs -> SList s (x ': xs)

deriving instance Show (SList s xs)

-- type family SingKind (k :: Type) :: k -> Type
-- type instance SingKind  N.Nat = N.SNat
-- type instance SingKind [x]    = SList (SingKind x)


{-
instance SingI '[] where sing = SNil
instance (SingI x, SingI xs) => SingI (x ': xs) where
    sing = SCons (sing @xs)
-}

class SListI (s :: k -> Constraint) (xs :: [k]) where
    slist :: SList s xs

instance SListI s '[] where
    slist = SNil

instance (s x, SListI s xs) => SListI s (x ': xs) where
    slist = SCons slist

type instance Sing (xs :: [k]) = SList SingI xs
instance SListI SingI xs => SingI (xs :: [k]) where
    sing = slist 

-- | >>> ex02
-- SCons (SCons SNil)
ex02 :: Sing [N.Nat1, N.Nat2]
ex02 = sing @[N.Nat1, N.Nat2]
```

---

```haskell
main :: IO ()
main = return ()
```
