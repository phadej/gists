---
title: Solution to servant API type indentation problem
author: Oleg Grenrus
tags: servant
---

This post is a literate Haskell file: so there are few imports, and `{-# LANGUAGE TypedKitchenSink #-}`.
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
module ListApi where
import Data.Text (Text)
import Data.Type.Equality
import Generics.SOP
import Generics.SOP.TH
import Servant
```

Problem
-------

How you indent your servant api types?

```haskell
type API1 = "foo" :> Get '[JSON] Int
       :<|> "bar" :> QueryParam "q" Text :> Get '[JSON] Text
       :<|> "quu" :> ReqBody '[JSON] Text :> Post '[JSON] Bool

type API2 =
    "foo" :> Get '[JSON] Int
    :<|> "bar" :> QueryParam "q" Text :> Get '[JSON] Text
    :<|> "quu" :> ReqBody '[JSON] Text :> Post '[JSON] Bool

type API3 =
         "foo" :> Get '[JSON] Int
    :<|> "bar" :> QueryParam "q" Text :> Get '[JSON] Text
    :<|> "quu" :> ReqBody '[JSON] Text :> Post '[JSON] Bool
```

There seems to be an unsolvable aesthetic vs. diff-friendly problem!

Solution
--------

Let's add a type-family!

```haskell
type family ListApi (es :: [*]) :: * where
    ListApi '[e]      = e
    ListApi (e ': es) = e :<|> ListApi es
```

With the help of `ListApi`, we can reduce the indentation problem to how
we indent (type-level) lists:

```haskell
type API' =
   '[ "foo" :> Get '[JSON] Int
    , "bar" :> QueryParam "q" Text :> Get '[JSON] Text
    , "quu" :> ReqBody '[JSON] Text :> Post '[JSON] Bool
    ]

type API = ListApi API'
```

The `API` and previously defined types are the same:

```haskell
proof :: API :~: API1
proof = Refl
```

Follow-up
---------

Yet to write the `Server API`, we still need to use `:<|>`.
The problem is not solved properly yet.
There is [generic client](http://hackage.haskell.org/package/servant-client-0.10/docs/Servant-Client-Generic.html) in `servant-client` ([PR#640](https://github.com/haskell-servant/servant/pull/640)),
and we can use the same idea for server part too: we need one more type family
and `generics-sop`:

```haskell
type family ServerMap (xs :: [*]) :: [*] where
    ServerMap '[]       = '[]
    ServerMap (x ': xs) = Server x ': ServerMap xs

newtype Ser x = Ser (Server x)

listApiServer
    :: forall a x xs.
       (Generic a, SListI xs, Code a ~ '[ServerMap (x ': xs)])
    => Proxy (x ': xs) -> a -> Server (ListApi (x ': xs))
listApiServer _ =
    step1 . step0 (shape :: Shape (x ': xs)) . unZ . unSOP . from
  where
    step0 :: forall ys. Shape ys -> NP I (ServerMap ys) -> NP Ser ys
    step0 ShapeNil      Nil         = Nil
    step0 (ShapeCons s) (I x :* ys) = Ser x :* step0 s ys

    step1 :: forall y ys. NP Ser (y ': ys) -> Server (ListApi (y ': ys))
    step1 (Ser x :* Nil)         = x
    step1 (Ser x :* ys@(_ :* _)) = x :<|> step1 ys

    -- this doesn't work:
    -- inAsingleStep
    --     :: Shape ys
    --     -> NP I (ServerMap (y ': ys))
    --     -> Server (ListApi (y ': ys))
    -- inAsingleStep ShapeNil (I x :* Nil) = Nil
```

This function is written in the infamous *banzai mode*, to convince GHC the program is well-typed.
The `Proxy` argument is not obviously required, but the following code would be ambiguous without it.
The `Shape` parameter in `step0` is needed because GHC cannot reason backwards
`ServerMap xs ~ '[]` &rightarrow; `xs ~ '[]`
(`ServerMap` is injective only shallowly, `Server` isn't injective!).
We need two steps, because going directly from `NP I (ServerMap ys)` to `Server (ListApi ys)`
would require to deal with two type-families at once, and GHC had
Also, `step1` highlights the question: should we have a type for an identity element
of `:<|>` operation?

An example
----------

With the help of `ListApi` and `listApiServer`,
we could define the server implementation in a neat way, IMO:

```haskell
api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

data ApiImpl m = ApiImpl
    { apiFoo :: m Int
    , apiBar :: Maybe Text -> m Text
    , apiQuu :: Text -> m Bool
    }

deriveGeneric ''ApiImpl

check :: Code (ApiImpl Handler) :~: '[ServerMap API']
check = Refl

app :: Application
app = serve api $ listApiServer api' impl
  where
    impl :: ApiImpl Handler
    impl = ApiImpl {..}

    apiFoo = return 42

    apiBar Nothing  = return "empty"
    apiBar (Just x) = return x

    apiQuu t = return (t == "works?")
```

If you experiment with the code and try to change the types in `ApiImpl` fields,
then the `check` "test" will catch them.
It seems, that errors generated from type-checking `check` are way more to the
point than type-errors in "classic" `serve api $ endpoint1 :<|> endpoint2 :<|>
...` approach.

I tend to have small wrappers around actual implementation of endpoints,
(i.e. I do write `apiFoo` which calls `businessFoo`, which knows little or
nothing about the web) so the `ApiImpl` is the only additional boilerplate.

Would this be useful? Give it a spin! Please comment either directly to me
([tweet](https://twitter.com/intent/tweet?url=http%3A%2F%2Foleg.fi%2Fgists%2Fposts%2F2017-03-15-solution-to-servant-type-indentation-problem.html&via=phadej&text=ListApi%20is%20...&hashtags=servant)) or to
[`servant` issue](https://github.com/haskell-servant/servant/issues/713).

---

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
*Main> :l servant-server-type.lhs
```
fetch the source from
[https://gist.github.com/phadej/06e0ce32e790fdab63b119fcbad357f5](https://gist.github.com/phadej/06e0ce32e790fdab63b119fcbad357f5)

P.S. Someone, please fix `DataKinds` syntax highlighting in whatever `pandoc` uses!
