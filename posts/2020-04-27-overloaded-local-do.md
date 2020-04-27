---
title: "overloaded-0.2.1: Overloaded:Do"
author: Oleg Grenrus
---

The `Overloaded:Do` is one of the new features of
recent [overloaded 0.2.1](https://hackage.haskell.org/package/overloaded-0.2.1)
release. `overloaded` package uses source plugins to reinterpret syntax
in different ways.

It solves the same problem as
[Local Do ghc-proposal](https://github.com/tweag/ghc-proposals/blob/local-do/proposals/0000-local-do.rst),
but the details differ.

GHC Pipeline
------------

Before diving into differences between proposal and `overloaded` approach,
we should recall the part of GHC compilation pipeline.
We are interested in the frontend steps.

- After parsing the AST with `RdrNames` (essentially just strings),
  is *renamed* to AST with `Name`s (resolved names, contain package and module names).
  The resulting AST is made *well-scoped*.
- Then that well-scoped surface syntax AST is *type-checked* and transformed
  into Core expressions.

The desugaring of built-in constructs (e.g. `do` notation) happens
in type-checking phase. This allows reporting better type-errors
(referring to the original source).

On the other hand, `OverloadedStrings` features are desugared already
during *renaming*. The AST string literal node is expanded into
`fromString ...`, where `fromString` is the [`Data.String.fromString`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html#v:fromString).

`overloaded` takes one step forward, and `Overloaded:String` can
be configured to desugar into some other combinator.

Similarly to `OverloadedStrings`, I'd expect `QualifiedDo` extension
to work in the same phase.

There is also `RebindableSyntax` extension. It works so,
that the AST for `do` statements is also populated with names it should be desugared to.
This is how I should have implemented `Overloaded:Do` too,
but currently I actually desugar statements manually in `OverloadedStrings`
way.

This is short overview of GHC pipeline parts relevant for syntax desugaring.
Let's next review the [Local Do ghc-proposal](https://github.com/tweag/ghc-proposals/blob/local-do/proposals/0000-local-do.rst).

Local Do ghc-proposal
---------------------

The idea is that we could write

```haskell
builder.do
   x <- u
   stmts
```

And it will be desugared into

```haskell
case builder of
    K { (>>=) = v } -> v u (\x -> builder.do { stmts })
```

instead of ordinary

```haskell
u Control.Monad.>>= \x -> do { stmts }
```

This would allow *locally* change how the do-notation is desugared,
instead of global `RebindableSyntax` approach.

Here `builder` is a value of record data type defined elsewhere as

```haskell
data NameIrrelevant = K { (>>=) :: sometype }
```

Informally one could think about `QuantifiedDo` desugaring into:

```haskell
case builder of K {..} -> do
    x <- u
    stmts
```

with `RebindableSyntax` semantics only for the outer `do` block.

I think this desugaring is *not elegant*. If you read the proposal,
you will notice that the `builder` must have *fully settled type T*,
which is obscure way to say *we should know the type of `builder` before
type-checking is done*.

Why so you may ask. This is an artifact of Haskell records using in desugaring.
In the desugaring, we *have to know* a field `Name`.
Note, the field name is not just a string `">>="`, but
it should be a `Name` of field of the **right type**.

We can use the same name for fields of different types
(e.g. when they are defined in different modules, without any extensions to Haskell98).
And these `Name`s are not the same after the renaming step.

This feels limiting.
[Vladislav Zavialov (int-index)](https://github.com/ghc-proposals/ghc-proposals/pull/216#issuecomment-614771416)
asked about more "dynamic" approach:

```haskell
data CustomDo m = CustomDo
  { (>>=) :: forall a b. m a -> (a -> m b) -> m b
  , return :: forall a. a -> m a
  }

mkCustomDo :: String -> CustomDo IO

main = do
  str <- getLine
  let customDo = mkCustomDo str
  customDo.do
    putStrLn "Hello"
    putStrLn "World"
```

If I'm reading the proposal right, here the `customDo` before `do` is not
an expression with fully settled type. You would need to write

```haskell
  (customDo :: CustomDo IO).do
    putStrLn "Hello"
    putStrLn "World"
```

which is just ugly. `Overloaded:Do` approach doesn't need such extra
annotation.

How Overloaded:Do is desugared
------------------------------

We need to find a way to desugar in
a single "global" way. And we have `builder` expression available.

You can arrive to this approach by thinking about

<blockquote>If field selectors are all different, is there an abstraction making them the same?</blockquote>

To which answer is yes: [`GHC.Records`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Records.html)
is one, but not (yet?) powerful enough.

Or you can think through *How I would write this in Agda* (or imaginary Dependent Haskell),
which is the way I thought about this originally.

Instead of a record with unique field names,
we can have a *dependent function*.

Using pseudo-syntax it would look like:

```haskell
data Method
    = Then -- ^ '>>'
    | Bind -- ^ '>>='
    ...

type family BuilderMethodType (m :: Method) :: Type where
    BuilderMethodType Then = ...
    BuilderMethodType Bind = ...
    ...

builder :: (m :: Method) -> BuilderMethodType m
builder Then = ...
builder Bind = ...
```

Then our running example

```haskell
builder.do
   x <- u
   stmts
```

would be desugared into

```haskell
builder GHC.QuantifiedDo.Bind u (\x -> builder.do { stmts } )
```

In simple examples this is possible to express in today's GHC Haskell.
We need a singleton of `Method`, and then we can write

```haskell
data SMethod :: Method -> Type where
    SBind :: SMethod 'Bind
    SThen :: SMethod 'Then

type family BuilderMethodType (method :: Method) :: Type where
    BuilderMethodType Then = Int -> Int -> Int
    BuilderMethodType Bind = TypeError ('Text "no bind for you")

builder :: SMethod m -> BuilderMethodType m
builder SThen = (+)
builder SBind = error "error"
```

By the way, is there a way to write something else than `error` for values of impossible types: `TypeError` or `Int ~ (a, b) => ...`.
Like `EmptyCase`, but there are nothing to `case` on.

Then we could use `do` notation as small addition calculator.
The `>>` will be replaced by `builder SThen` in:

```haskell
total = sugar.do
   1
   2 * 3
   4 * 5
```

This approach is currently doomed, we run into problems:
If we try to write builder for ordinary `Monad`s,
we'll need `RankNTypes` as values of a type family:

```haskell
type family MonadMethodType (method :: Method) :: Type where
    MonadMethodType Then = forall a b m. Monad m => m a -> m b -> m b
    ...
```

which will error with

```
• Illegal polymorphic type:
    forall a b (m :: * -> *). Monad m => m a -> m b -> m b
• In the equations for closed type family ‘MonadMethodType’
```

Ok, `TypeFamilies` don't work (yet?). Luckily we have `FunctionalDependencies`,
which is kind of the same, but sometimes helpfully different.
Type class definition is accepted, as there is nothing special.
We declare "type-family" and builder-value at the same time.
Note the similarity with [`GHC.Records.HasField`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Records.html#t:HasField).

```haskell
class MonadMethod (m :: Method) (ty :: Type) | m -> ty where
    monad :: SMethod m -> ty
```

but when we try to write instance (after enabling scary `ImpredicativeTypes`):

```
instance
    (ty ~ (forall a b m. Monad m => m a -> m b -> m b))
    => MonadMethod 'Then ty
  where
    monad SThen = _
```

GHC-8.8.3 says

```
ghc: panic! (the 'impossible' happened)
  (GHC version 8.8.3 for x86_64-unknown-linux):
	unusedInjTvsInRHS.injTyVarsOfType
```

Ok. let's try GHC-8.10.1:

```
• Found hole:
    _ :: forall a b (m :: * -> *). Monad m => m a -> m b -> m b
```

Nice! exactly what we want. The `>>` should fit that hole perfectly...
except it doesn't...

```haskell
instance
    (ty ~ (forall a b m. Monad m => m a -> m b -> m b))
    => MonadMethod 'Then ty
  where
    monad SThen = (>>)
```

errors with

```
• Couldn't match type ‘m0 a0 -> m0 b0 -> m0 b0’
                 with ‘forall a b (m :: * -> *). Monad m => m a -> m b -> m b’
  Expected type: ty
    Actual type: m0 a0 -> m0 b0 -> m0 b0
```

I run out of ideas how to make GHC-8.10 accept that.
I haven't tried whether GHC with [Quick Look Impredicativity](https://github.com/ghc-proposals/ghc-proposals/pull/274)
would accept that. Maybe it would, which will be nice.

The final solution used in `overloaded` is to not use `FunctionalDependencies` at all.
Then the instances can be written as

```haskell
instance
    (ty ~ m a -> m b -> m b, Monad m)
    => MonadMethod 'Then ty
  where
    monad SThen = (>>)
```

We can have type-variables in the context which are not used in the instance
head (`MonadMethod 'Then ty`), because they are "defined" by type-equality constraint.

Note: Instances don't need to be written for all methods,
or they can be guarded by `TypeError`. Therefore it's straight-forward
to have a desugaring forbidding `fail` or `>>=` (if you are using `ApplicativeDo`).
This is a benefit of a "type-approach".

Also the `overloaded` version doesn't use singletons, but rather `TypeApplications` and `AllowAmbiguousTypes`.
The `>>` is desugared into `builder @Then`.
This allows to write builders for ordinary `Monad` or `IxMonad` and many others.
And in fact, the above is only a sketch how `builder` could be implemented.
Any approach works as far as `builder @method` type application is legal.

With `overloaded` we had to diverge from the ideal solution.
But as GHC becomes smarter, we could improve.

Conclusion
----------

The [Local Do ghc-proposal](https://github.com/tweag/ghc-proposals/blob/local-do/proposals/0000-local-do.rst)
and [Overloaded:Do](https://github.com/tweag/ghc-proposals/blob/local-do/proposals/0000-local-do.rst)
both solve the same problem of locally altering how `do` notation is desugared.
The machinery is however slightly different.

I consider the proposed variant inelegant,
as it introduces (to my understanding) new *fully settled type* concept.
The `overloaded` approach shows it is not necessary.
Unfortunately the current GHC functionality
is not powerful enough to implement builders elegantly,
even the desugaring itself is very simple.
Surprisingly even the Local do ghc-proposal have extensive alternatives
section, there aren't anything in the Dependent Haskell spirit.
