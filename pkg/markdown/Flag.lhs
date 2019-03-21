---
title: Flag, a tagged Bool
author: Oleg Grenrus
tags: engineering
---

This posts complements two other recent blogs:
[*Code smell: Boolean blindness*](https://runtimeverification.com/blog/code-smell-boolean-blindness/)
by Thomas Tuegel
and 
[*Ceci n'est pas un default*]( https://gallais.github.io/blog/ceci-pas-default)
by Guillaume Allais. 

Thomas writes

<blockquote>
The popular term “boolean blindness” refers to the information lost by functions that operate on Bool when richer structures are available
</blockquote>

and indeed, in one project I wrote a function with a very horrible type:

```{.haskell .ignore}
doTopo
    :: UseColors
    -> Bool
    -> Bool
    -> PlanJson
    -> Bool
    -> Bool
    -> IO ()
```

I could add comments, but those are unchecked by a compiler.
I could use `Maybe` or some other richer type,
but  these `Bool`s are coming from command line arguments, which
are just single bit of information. What else can I do?

[Phantom types](2015-01-31-phantom-types.html) is one possible solution.

Phantom type
------------

After small module header...

```haskell
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass #-}
-- | Because 'Flag' constructor isn't exposed,
-- we have to explicitly 'toFlag' and 'fromFlag'.
-- That way it's less likely we mix up bare Booleans.
module Flag (
    -- * Flag
    Flag, toFlag, fromFlag,
    -- * HasDefault
    HasDefault,
    -- * optparse-applicative
    showHide,
    switchA,
    switchB,
    ) where

import           Control.Applicative ((<|>))
import           Data.Proxy (Proxy (..))
import           Data.Semigroup (Semigroup (..))
import           Data.Singletons.Bool
import qualified Options.Applicative as O
```

... we can define `Flag`:

```haskell
newtype Flag t = MkFlag Bool

toFlag :: t -> Bool -> Flag t
toFlag _ = MkFlag

fromFlag :: t -> Flag t -> Bool
fromFlag _ (MkFlag b) = b
```

A `Flag` is just a `Bool`. But `Flag Foo` and `Flag Bar` are different types!

As the module export list already revealed,
the `MkFlag` constructor is not exported. The only way to construct and match
on `Flag` is to use `toFlag` and `fromFlag` functions.
Both functions take an additional `t` argument. The idea is to define a
data type per flag and use the single value as a proxy for a type.

```haskell
data ShowBuiltin = ShowBuiltin
data ShowGlobal  = ShowGlobal
data TopoReverse = TopoReverse
data ShowFlags   = ShowFlags
```

We define a nullary constructor, so the type can act as its own `Proxy`.
Using `Flag`, we can make `doTopo` signature
much more comprehensible:

```{.haskell .ignore}
doTopo
    :: UseColors
    -> Flag ShowBuiltin
    -> Flag ShowGlobal
    -> PlanJson
    -> Flag TopoReverse
    -> Flag ShowFlags
    -> IO ()
doTopo ... =
    -- here the redundancy is good, it will help catch errors
    if fromFlag ShowBuiltin showBuiltin
    then ...
    else ...
```

Now the type signature is much more self-explanatory, and it's impossible (or
at least quite hard) to mixup different flags. And note, this is completely
`Haskell98` solution. So far we didn't need any extensions.

An alternative: record type
---------------------------

Another alternative would been to define a helper structure,

```{.haskell .ignore}
data TopoOpts = TopoOpts
    { toUseColors   :: UseColors
    , toShowBuiltin :: Bool
    , toShowGlobal  :: Bool
    , toPlanJson    :: PlanJson
    , toTopoReverse :: Bool
    , toShowFlags   :: Bool
    }
```

especially with [`RecordWildCards`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecordWildCards) language extension, it's convinient to use.
Also with this approach it's harder to mixup different `Bool`s.
Both `Flag` and a **record** name Booleans.

For whoever is wondering, the `UseColors` type is a three way setting: `always`,
`never` or `auto`, so I wrote a special type for it.  One could write own type
for each Boolean flag too. I didn't explore that alternative. We'd need to
`case`, as there aren't natural candidate to proxy in `fromFlag` variant (which
can be made with `Generics`: but one have to be careful to define values
in order as in `data Bool = False | True`!).


Make phantom relevant
---------------------

It depends which approach, `Flag` or a record, is more convinient (or even both together!).
`Flag` approach let us do some nice extra stuff, thanks to a type-level tag.
But we have to departure the Haskell98-land.
Inspired by Guillaume's post, we can do a little of dependent-ish programming.
I use my [`singleton-bool`](http://hackage.haskell.org/package/singleton-bool) library here.

First we define `HasDefault` class.

```haskell
-- | Has default boolean value.
class SBoolI def => HasDefault (def :: Bool) t | t -> def

-- | Flag with a default value.
def :: forall t def. HasDefault def t => t -> Flag t
def t = toFlag t (reflectBool (Proxy :: Proxy def))
```

The [`FunctionalDependencies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#functional-dependencies)
(and not [`TypeFamilies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-family-declarations))
are used so we can use [`DeriveAnyClass`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-any-other-class)
extension:

```haskell
data MyFlag = MyFlag deriving (HasDefault 'True)
```

Now the phantom type isn't really-really phantom anymore,
`HasDefault` constraint makes it "relevant".

An example: using with optparse-applicative
-------------------------------------------

We can use `HasDefault` to implicitly wire-in things, for example
when making [`optparse-applicative`](http://hackage.haskell.org/package/optparse-applicative)
parsers. Remember, `Flag`s often come from command line arguments.

I'd like to use specialised parser for `Show*` flags.
Now we can use the fact that we know the default value:
And the default value is specified close to flag declaration (visible in Haddocks!),
and not hidden somewhere in the command line parser code. That's a win.
So we can write quite generic `showHide` parser combinator:

```haskell
showHide :: HasDefault def t => t -> String -> String -> O.Parser (Flag t)
showHide t n d =
    O.flag' (toFlag t True) (O.long ("show-" ++ n) <> O.help d)
    <|> O.flag' (toFlag t False) (O.long ("hide-" ++ n))
    <|> pure (def t)
```

*Note:* we can go even further: we could define a `HasParser` class
and with a little [`DerivingVia`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-via) magic
parsers could be completely derivable. I leave that as an exercise :)

Another use case: super power [`switch`](http://hackage.haskell.org/package/optparse-applicative-0.14.3.0/docs/Options-Applicative-Builder.html#v:switch).
We can make GHC verify that the default value is `False`,
so if we decide to change the default, we won't forget to fix the cli parsers:

```haskell
switchA :: HasDefault 'False t => t -> String -> String -> O.Parser (Flag t)
switchA t n d = fmap (toFlag t) $ O.switch $ O.long n <> O.help d
```

Or we can make switch flip automatically:

```haskell
switchB :: HasDefault def t => t -> String -> String -> O.Parser (Flag t)
switchB t n d = fmap (toFlag t) $ O.flag v (not v) $ O.long n' <> O.help d
  where
    n' = if v then "no-" ++ n else n
    v  = fromFlag t (def t)
```

Careful reader would notice that `showHide`, `switchA` and `switchB`
are *string-ly typed*. But that again, *depends*; not bad in this case.
In the real codebase `showHide` is used like:

```{.haskell .ignore}
<$> showHide ShowBuiltin "builtin" "Show / hide packages in global (non-nix-style) package db"
<*> showHide ShowGlobal  "global"  "Show / hide packages in nix-store"
```

Wrapping in additional `newtype` won't help; adding `IsString` instance
would make newtype not any better than plain `String`.
On the other hand, if we use `DerivingVia` to derive parsers,
whole problem would go away.

Conclusion
----------

To conclude, 
[phantom types](2015-01-31-phantom-types.html) are quite useful,
and if you spice them with a little of type-level programming, you won't need
to write boilterplate and errorprone code.
