---
title: More QualifiedDo examples
author: Oleg Grenrus
---

*Qualified do-notation*, [`QualifiedDo`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html), is a nice syntactical extension in GHC. Probably the best its property is that it changes semantics only locally, by using explicit "annotation": by qualifying the `do` keyword[^applicativedo]. This means that enabling the extension doesn't change meaning of other & existing code.

I'll give two examples of `QualifiedDo` applications.

First example: COMPLETE pattern synonyms
----------------------------------------

GHC had long had [`PatternSynonyms`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html). One use case for pattern synonyms is to provide backward compatibility when data type constructors change: preserving old constructor names and arguments as a compatibility pattern synonym.

For example, we used to have `data Solo = Solo a`. Recently the constructor was renamed to `MkSolo` to avoid name punning. To not break all the code using `Solo` constructor there compatibility pattern synonym was added:

```haskell
pattern Solo :: a -> Solo
pattern Solo x = MkSolo x
{-# COMPLETE Solo #-}
```

The `COMPLETE` pragma says that a pattern match using `Solo` pattern synonym is complete, so we wouldn't get incomplete pattern match warnings[^complete].

But `COMPLETE` support is (ironically) incomplete. If we have a `do` block like

```haskell
broken :: Monad m => m (Solo a) -> m a
broken s = do
    Solo x <- s
    return x
```

the GHC will **error** because we don't have `MonadFail` instance (to desugar *incomplete* pattern match: `Could not deduce (MonadFail m)`, that is GHC issue [#15681](https://gitlab.haskell.org/ghc/ghc/-/issues/15681)). There are various workarounds, but I don't remember anyone mentioning `QualifiedDo`.

If we write a small helper module

```haskell
module M ((>>=), (>>), fail) where

import Prelude ((>>=), (>>), Monad, String, error)
import GHC.Stack

fail :: (Monad m, HasCallStack) => String -> m a
fail = error
```

we can change `broken` into something which `works`:

```haskell
import qualified M

works :: Monad m => m (Solo a) -> m a
works s = M.do
    Solo x <- s
    return x
```

Now if GHC needs to `fail`, it will simply `error`.

I hope that it's obvious that this is a band-aid: if you are relying on `fail` doing something useful (e.g. in `Maybe`), this will obviously break your program. But as `QualifiedDo` usage is explicitly annotated it's not a spooky action at the distance. And `HasCallStack` annotation should help you find the mistakes if any happen.

Second example: zero-overhead effects
-------------------------------------

At work I have been (adjacently) working with the code building on top of [`io-sim`](https://hackage.haskell.org/package/io-sim). TL;DR you write your code using (a lot of) type-classes, and then can either run your code in real `IO` (production) or in a simulator `IOSim` (for tests). But I'm getting slightly anxious thinking about having all I/O code being abstracted using type-classes making the true `IO` case potentially go slow. (This is `mtl`-like take on effect handling, but even [`effectful`](https://hackage.haskell.org/package/effectful) or something based on delimited continuations aren't zero-overhead: the overhead is there, just smaller).

What we truly want is a complete specialisation of effect-related type-classes, so there aren't any abstraction bits left when the use case is concrete (in `mtl` approach we can theoretically get there, but not in practice. In `effectful` or delimited-continuations a small cost is always there, but it doesn't rely that much on compiler optimising well).

Most likely, if your code isn't pushing both the I/O and CPU utilization at the same time, either approach will work ok. Compare that to data science done in Python: Python is a quite slow glue language, but it's combining bigger fast running "primitive" blocks. So if there is very little glue code, and the most work is done inside the abstracted primitives, the glue being tacky doesn't matter.

But can we do better?

In GHC we can do better using staging i.e. Typed Template Haskell (TTH). At first I was worried that TTH syntactic overhead will be off-putting until I remembered that `QualifiedDo` extension exists!

We can write code like:

```haskell
import qualified SIO

example :: SIO.SIO i m => i FilePath -> m ()
example fn = SIO.do
  contents <- SIO.readFile fn
  SIO.putStr contents
```

that looks like normal Haskell. If we were forced to use `>>=` like operator explicitly, e.g. writing

```haskell
example' :: SIO.SIO i m => i FilePath -> m ()
example' fn =
  SIO.readFile fn >>>= \contents ->
  SIO.putStr contents
```

it wouldn't be as nice.

The `SIO` type class has the part which looks almost like `Monad`, but not exactly:

```haskell
class SIO i m | m -> i where
  (>>=)    :: m a -> (i a -> m b) -> m b
```

The "pure" values are wrapped inside type constructor `i` (for identity).

The `readFile` and `putStr` are also in the same type-class (could be different, doesn't really matter):

```haskell
  readFile :: i FilePath -> m ByteString
  putStr   :: i ByteString -> m ()
```

We can have concrete instances, like `IO` (or actually `IOSim`) for tests:

```haskell
instance SIO Identity IO where
  (>>=) :: forall a b. IO a -> (Identity a -> IO b) -> IO b
  (>>=) = coerce (bindIO @a @b)

  readFile = coerce BS.readFile
  putStr = coerce BS.putStr
```

But because we are liberated from the restricting shape of the `Monad` type class, we can have instance for `CodeQ` from `template-haskell`:

```haskell
newtype CodeIO a = CodeIO { unCodeIO :: CodeQ (IO a) }

instance SIO CodeQ CodeIO where
  m >>= k     = CodeIO
    [|| bindIO $$(unCodeIO m) (\x -> $$(unCodeIO (k [|| x ||]))) ||]
  readFile fn = CodeIO [|| BS.readFile $$fn ||]
  putStr bs   = CodeIO [|| BS.putStr $$bs ||]
```

Then in our main production module we can splice the example in like

```haskell
spliced :: FilePath -> IO ()
spliced fn = $$(SIO.unCodeIO $ SIO.do
    example [|| fn ||]
    example [|| fn ||])
```

and the generated code has no effect handling abstractions; in fact not even a `Monad`, as we used `thenIO` and `bindIO` building blocks:

```haskell
spliced fn_a3kY =
    (GHC.Base.thenIO
       ((GHC.Base.bindIO (Data.ByteString.readFile fn_a3kY))
          (\ x_a3m2 -> Data.ByteString.putStr x_a3m2)))
      ((GHC.Base.bindIO (Data.ByteString.readFile fn_a3kY))
         (\ x_a3m3 -> Data.ByteString.putStr x_a3m3))
```

We have a precise control (but also a responsibility) to control the inlining of building blocks (i.e. if we want `example` let-bound first and then called twice, we must do that manually: power comes with responsibility). This is either a pro or con, depending on your POV. I think this is a pro if you go this far caring about the performance. If GHC Haskell had a type-class like mechanism with full monomorphisation guarantee, we'd would still like to to control inlining.

You may also worry that "wont staging generate *a lot of code*". Yes it will, but so would full monomorphisation (of templates in C++ or traits in Rust). It's a behaviour we arguably want, but it's GHC which may be worried and don't do too good job. With staging we could also do modular code-generation too, making layered type-class hierarchy, generating i.e. "pre-splicing" intermediate layers  (layers like in [three layer cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)).

Conclusion
----------

[`QualifiedDo`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html) is a neat GHC extension. We saw two more examples of its usage, where we want something like regular `Monad` desugaring, but which doesn't fit the `Monad` type-class.
I also think we could have more of `Qualified*` syntactic extensions.

[^applicativedo]: In comparison [`ApplicativeDo`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/applicative_do.html) applies globally. These design choices are probably not-so-intentional. For `QualifiedDo` it would require some additional setting to change *all* do statements in the source module (like `-fplugin` takes a module name). OTOH `ApplicativeDo` main motivation (using it with [`haxl`](https://hackage.haskell.org/package/haxl)) was to use it globally. But if you want to use it only in *some* `do` statement, you can't. Similarly `OverloadedStrings` applies to all string literals, and in the same way for all of them. Compare to Python which has kind of "QualifiedStrings" with string literals very differently: imagine writing `T."this is text"` but still having `"this is string" :: String`, without any type-class resolution.

[^complete]: GHC doesn't try to reason about completeness through pattern synonyms: you may want to keep a pattern synonym group intentionally incomplete (so extending an otherwise abstract type with new ones isn't a breaking change), or to tell that something is complete (due to invariant you maintain, but GHC has no chance figuring out).
