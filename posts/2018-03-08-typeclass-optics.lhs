---
title: Typeclass Optics
author: Oleg Grenrus
tags: lens
---

This is a short gist for *Iceland_Jack*. Thanks for nerd-snipping me on [Twitter](https://twitter.com/Iceland_jack/status/971775520605134848)

This is literate Haskell file. Note, **no extensions**, plain (GHC-ish) Haskell2010.

```haskell
module TypeclassOptics where

import Data.Bifunctor (first)
import Control.Category (Category, (>>>))
import qualified Control.Category as C
```

For simplicity of presentation I will consider only simple optics, not optic families.
Optics form sub-categories (I only started reading [a book](https://twitter.com/Iceland_jack/status/971154407223066625) to understand what kind!?).

Type classes
-----------

We define classes which let's construct various optics.
Let's define `Iso`, `Lens`, `Prism` and `Affine`, they will illustrate enough of hierarchy:

```haskell
class Category optic => Iso optic where
    iso :: (s -> a) -> (a -> s) -> optic s a

class Iso optic => Lens optic where
    lens :: (s -> a) -> (a -> s -> s) -> optic s a
    lens sa ass = iso (\s -> (sa s, s)) (uncurry ass) >>> _1
    
    _1 :: optic (a, c) a
    _1 = lens fst $ \a (_, c) -> (a, c)

class Iso optic => Prism optic where
    prism' :: (a -> s) -> (s -> Maybe a) -> optic s a
    prism' as sma = iso (\s -> maybe (Left s) Right (sma s))
                        (either id as)
                >>> _Right

    _Right :: optic (Either b a) a
    _Right = prism' Right (either (const Nothing) Just)

class (Lens optic, Prism optic) => Affine optic
```

ASetter
-------

Next we can define basic operations, for example `over`. We need
concrete `ASetter` for that:

```haskell
over :: ASetter s a -> (a -> a) -> s -> s
over (ASetter o) = o
````

where

```haskell
newtype ASetter s a = ASetter ((a -> a) -> s -> s)

instance Category ASetter where
    id = ASetter id
    ASetter bc . ASetter ab = ASetter (ab . bc)

instance Iso ASetter where
    iso sa as = ASetter $ \aa -> as . aa . sa

instance Lens ASetter where
    _1 = ASetter $ \aa -> first aa

instance Prism ASetter where
    _Right = ASetter $ \aa -> either Left (Right . aa)

instance Affine ASetter
```

And we can run few examples:

```haskell
-- (Right "raboof",42)
ex1 :: (Either Bool String, Int)
ex1 = over (_1 >>> _Right) reverse (Right "foobar", 42)

-- (Left False,1)
ex2 :: (Either Bool String, Int)
ex2 = over (_1 >>> _Right) reverse (Left False, 1)
```

AGetter
-------

Another basic operation is `view`. That's defined only for `AGetter`s:

```haskell
view :: AGetter s a -> s -> a
view (AGetter o) = o
```

where

```haskell
newtype AGetter s a = AGetter (s -> a)

instance Category AGetter where
    id = AGetter id
    AGetter bc . AGetter ab = AGetter (bc . ab)

instance Iso AGetter where
    iso sa _ = AGetter sa

instance Lens AGetter where
    _1 = AGetter fst
```

Note that we don't have `Prism` or `Affine` instance for `Getter`s (i.e. `Iso`, `Lens`, but not `Prism`!).
(We could have different folding `view`, which would have - that variant is in `lens`).

And the example:

```haskell
-- "foobar"
ex3 :: String
ex3 = view _1 ("foobar", True)
```

Error messages
--------------

We could used `(->)` directly as `AGetter`, but it helps with error messages:

``` {.haskell .ignore}
view _Right (Left True)
```

```
No instance for (Prism AGetter) arising from a use of ‘_Right’
```

After few steps we have **profit!**.

I want to conclude with a reference to [*The evolution of a Haskell Programmer*](https://www.willamette.edu/~fruehr/haskell/evolution.html),
keep that in mind when you enable `{-# LANGUAGE KitchenSink #-}` ;)

---

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)
