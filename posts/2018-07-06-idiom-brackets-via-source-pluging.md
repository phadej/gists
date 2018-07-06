---
title: Idiom brackets via source plugin
author: Oleg Grenrus
---

Today I experimented with upcoming GHC-8.6 feature: Source Plugins.  In GHC 8.6
plugin functionality is extended, so there are more hooks where we can add own
processing of the AST: after parsing, renaming or type-checking. For idiom
brackets we'll change the AST right after the parsing step.

Idiom brackets (introduced in *Applicative programming with effects* paper)
is a syntax extension to write `Applicative` expressions without
drowning into `<*>`.
[The Strathclyde Haskell Enhancement](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/idiom.html)
also supports idiom brackets, but with different syntax.
The point my plugin tries to solve: a lot of preprocessors can be built using
GHC for the boring parts (like parsing).

As nobody (?) writes their lists as `([1, 2, 3])`,
we can steal that syntax!
(Note: I don't know how to ignore, or lift pure values, ideas welcome)

```haskell
{-# OPTIONS -fplugin=IdiomsPlugin #-}
-- | Hack idiom-brackets using Source Plugin.
--
module Main (main) where

main :: IO ()
main = do
    -- Just "foobar"
    print ([ mappend (Just "foo") (Just "bar") ])
    -- Nothing
    print ([ const (Just "foo") Nothing ])
    -- Just 3
    print ([ (+) (Just 1) (Just (2 :: Int)) ])

    -- Just True
    print ([ Just True || Just False ])

    -- [1,2,3], because `pure 1` can be specialised to [1]
    -- and non singleton lists are not transformed
    print $ ([ 1 ]) ++ ([2, 3]) ++ ([])
```

Implementation
--------------

The implementation is on GitHub: https://github.com/phadej/idioms-plugins

It's not complicated, little over hundred lines right now. Some highlights:

- [Scrap Your Boilerplate](http://hackage.haskell.org/package/syb)
  let us transform only the matching intersting nodes of AST by

    ```haskell
    transform = SYB.everywhereM (SYB.mkM transform') where
        transform' :: LHsExpr GhcPs -> GHC.Hsc (LHsExpr GhcPs)
        transform' ... = ...
    ```

- We can print legin looking warnings by

    ```haskell
    dflags <- GHC.getDynFlags
    liftIO $ GHC.putLogMsg
        dflags GHC.NoReason Err.SevWarning l (GHC.defaultErrStyle dflags) 
        $ text "expression" <+> ppr x <+> text "looks weird"
    ```

    where pretty-printing combinators are defined in `Outputable` module.
    And more generally, a lot of things can be converted to `String` with
    `showPpr dflags theThing` (not `Show`).

I invite you to look through the implementation, think about what else
Source Plugins can be used for, and implementing those ideas!

Please ask me questions (e.g. on [twitter I'm @phadej](https://twitter.com/phadej)),
if I cannot answer them myself, I'll find someone who can!
