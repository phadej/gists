---
title: Type-class design principles
author: Oleg Grenrus
tags: engineering
---

This blog post is long overdue.
I will try to explain my design principles behind the [`lattices-2`](https://hackage.haskell.org/package/lattices) changes (which I uploaded to Hackage over a year ago).
The following is not an absolute truth of how things should be done,
consider this mumblings as extended style guide.
For the record, I use four space indentation.

I will discuss how I think type-class hierarchies should be made.
I can think of different use cases f or type-classes:

1. Modelling abstract structures (e.g. `Functor`)
2. A mechanism for program synthesis (e.g. `FromJSON`)
3. Completely ad-hoc names

The last two are not related to `lattices`,
so let me discuss them first.

Ad-hoc names
------------

I'm not sure what to say about the third group.
It is somewhat hard to say whether something
is just an *ad-hoc name*.
My rule of thumb is that it is not useful to abstract over that class.
An example type-class in this group is `Default`:

```haskell
-- | 'def' is a sensible default value of type @a@
class Default a where
    def :: a
```

Would you write 

```haskell
someFunction :: Default a -> ... a
```

*Maybe*. More often `def` is used at concrete type, and you don't see
`Default a => ...` constraints in functions type signatures.

<blockquote>
It is completely fine to use type-classes for ad-hoc names,
<em>as long as you do <b>not</b> abstract over them</em>
</blockquote>

Worth mentioning is the [`HasField`](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Records.html) type-class.
I do think that when we will see function with type signatures like

```haskell
process :: (HasField "name" x String, HasField "age" x Int) => x -> IO ()
```

there is something wrong.

A mechanism for program synthesis
---------------------------------

Type-classes used for program synthesis are essentially ad-hoc names as well, but not quite.
What are they?  Examples include
- `aeson`; [`ToJSON`](https://hackage.haskell.org/package/aeson-1.5.4.0/docs/Data-Aeson.html#t:ToJSON) and
  [`FromJSON`](https://hackage.haskell.org/package/aeson-1.5.4.0/docs/Data-Aeson.html#t:FromJSON)
- `QuickCheck`: [`Arbitrary`](https://hackage.haskell.org/package/QuickCheck-2.14.1/docs/Test-QuickCheck-Arbitrary.html#t:Arbitrary)
- `servant-server`: [`HasServer`](https://hackage.haskell.org/package/servant-server-0.18/docs/Servant-Server.html#t:HasServer)
- and many others

There are people who say that `ToJSON`, `FromJSON` and `Arbitrary` should
not exist, but instead we should use explicitly named combinators.
This is one of the selling points of [`hedgehog`](https://hackage.haskell.org/package/hedgehog) compared to `QuickCheck`, for example.

These type classes are not ad-hoc in a sense of my definition above,
as they are often abstracted over:

```
-- | Decode a value of type @a@ from JSON 'Value'.
fromJSON :: FromJSON a => Value -> Result a
```

or

```
-- | The servant magic function, which turn handlers into WAI 'Application'.
serve :: HasServer api '[] => Proxy api -> Server api -> Application
```

The `aeson` (and `QuickCheck`) stuff can be done using explicit combinators,
but making a variant of `servant` using explicit combinators would not work at all.

You may say that these are completely different usecases, but
*in my opinion* they are the same, an application of
*program synthesis* or *generic programming* in a broad sense.

In the `servant` case it is easier to see.
The library defines an (extensible) domain specific language,
combinators to write your `type API = ...` which is then interpreted.
The different interpretation are given using `HasServer`, `HasClient` etc. type-classes.
A considerable amount of code is "written" for you by type class instance
resolution mechanism.

I haven't yet said anything about *laws*.
Note how in `servant`, `HasServer` and `HasClient` type-classes are unrelated, there are now laws tying them together.
They are even defined in different packages (`servant-server` and `servant-client`), with no dependencies between them.
There might be DSL constructs which are interpreted by `HasServer` but not by `HasClient`
(if you wonder, e.g. [`Raw`](https://hackage.haskell.org/package/servant-0.18/docs/Servant-API-Raw.html#t:Raw)).
and there isn't strict guarantee that a client built with `HasClient` will work flawlessly with `HasServer`, though it is hopefully a goal of maintainers for it to be so.

Now let return to `aeson`. There are two type-classes: `ToJSON` and `FromJSON`.
One way to look at this is:

- Things in kind `Type`, `Type -> Type` etc are *a domain specific language* for JSON serialisation.
- `FromJSON` gives building blocks for one interpretation,
- `ToJSON` for another.

We can write `HasServer Int ctx` instance for `servant-server`.
You probably won't do that though (what would that mean?).
Similarly, I think there are subsets of things in `Type`
which should and shouldn't have `ToJSON` and `FromJSON` instances.

You should write separate types to define the wire formats of your JSON APIs, *define your DSL*.
For example, your API may have an envelope for responses, like

```haskell
data Response a = ResponseOk ExtraBits a
                | ResponseErr ErrorMessage

instance ToJSON a => ToJSON (Response a) where
    ...
```

and this `Response` type is then a combinator in your data-definition DSL.
We often have object payloads,
where we can use (abuse?) Haskell record notation as a data-definition language for JSON (e.g. field names in records matter),
and use generic deriving (via `GHC.Generics`, Template Haskell, or something else).
This is something `hedgehog` cannot provide, as it doesn't have a type-class.
`GHC.Generics` and other metaprogramming approaches are often (always?) type-(class)-driven.

We could argue whether Haskell's `data` is a good data-definition language for JSON (or other wire formats).
I think it is a lot better than writing OpenAPI Schemas by hand in YAML (or in many cases even as Haskell data).
OpenAPI Schemas are by the way *another interpretation* of a "data definition DSL",
we can interpret `Type`s as JSON schemas as well. 

I need to stress, that here metaprogramming helps to not make many silly typo-like mistakes.
Not having to write a field name in `parseJSON`, `toJSON`, `toSchema` and possibly more cases is a noteworthy feature.
It is still a good idea to write tests:

- *roundtrip tests*, to check compatibility of implementations
- and *example comparison (golden) tests*, to notice systematic errors

especially as it is likely to be no more difficult than adding a one line entry to the list of types to test.

Take these three functions:

```
fromJSON :: FromJSON a => Value   -> Result a
toJSON   :: ToJSON a   => a       -> Value
toSchema :: ToSchema a => Proxy a -> Schema
```

what you think, do they give more or less compatibility guarantees,
when instantiated with `a = Int` than concrete

```
fromJSON_Int :: Value -> Result Int
toJSON_Int   :: Int -> Value
toSchema_Int :: Schema
```

Or to make it more obvious

```
fromJSON @Int :: Value -> Result Int
toJSON @Int   :: Int -> Value
toSchema @Int :: Schema
```

In my opinion, there is no difference.
You may object that explictly named combinators are more flexible,
as you may want to encode a type in different ways.
My answer to that is to use `newtype`s.
Remember that I said that you should have distinct types specifically describing your wire data.
You will need to convert internal data types to the "external" (wire) data types anyway
and [`coerce`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Coerce.html#v:coerce) will make `newtype` wrapping and unwrapping a non-issue.

There are cases when it is hard to say how some kind of data have to be serialized, for example, `UTCTime` or `Either a b`.
Not having an instance for "an interpretation purposes" is however worse than making an arbitrary choice.
If you disagree, you can define a `newtype`.

<blockquote>
Using type-classes to build interpreters for an embedded domains specific language is fine use case.
</blockquote>

where we have to understand DSLs in a very broad sense.

Note: you have to check how the interpretation is done,
these *synthesis type-classes* barely give any *equational reasoning* power.

We can improve an example from the previous section:

```haskell
process :: HasQueryData x => x -> IO ()
```

Here, `HasQueryData` can have more semantics than just having a name and an age,
It is a *domain specific* type-class.
(Also still pseudo-structural `HasField "queryData" x QueryData` is weaker than *nominal* `HasQueryData`).

Equality
--------

<blockquote>
<em>What you mean by "are equal"?</em>
- Anonymous type theorist
</blockquote>

The type-classes in the remaining group of *abstract structures* will have *laws* attached.
The laws are often some equalities, but what we mean by saying something have to be equal?

---

OLD:

asdad

My personal guidelines for the second one are: do whatever you need
to get job done. The code-generation machinery of `servant`
is one example of this use.
The classes and instances are a huge type-level prolog
program which gives you a server or a client or docs.
There is no point to argue whether `HasServer` should have some laws,
no, it just generates the code type-"code" tells you.
Similarly the classes driving GHC.Generics base derivation (`GNFData`, ...)
are the same. The pattern is same: there is type-level code (in Generics it's `Rep f`),
which is interpreted.

I consider classes like `Arbitrary` from `QuickCheck`,
and `FromJSON`&`ToJSON` from `aeson` etc. to be in this category as well.
Their main task is to reduce amount of code we write...
... or be ad-hoc name overloading.
Both `aeson` and `QuickCheck` can be used without ever writing
instances of above classes for your types.
Yet, admittely, the library interfaces are not so convenient if you
pick that path.

Another example of clearly ad-hoc overloading is
`class IsString a where fromString :: a`.
Controversial class. ad-hoc = people debate for ages over Internet
about its deep purpose.

The first group is more interesting. It contains classes Typeclassopedia
speaks about, for example in `base` we have:

- Functor, Foldable, Traversable,...
- Applicative, Monad, (Apply, Bind, Pointed), MonadFix, ...
- Semigroup, Monoid, (Default)

But then I e.g. maintain `lattice` library which
since `lattice-2` version has overgone hierarchy change from

```haskell
class MeetSemiLattice a where            class JoinSemiLattice where
    (/\) :: a -> a -> a                      (\/) :: a -> a -> a

         ||                                       ||
         \/                                       \/

class BoundedMeetSemiLattice a where     class BoundedJoinSemiLattice where
    top :: a                                 bottom :: a
```

with empty-member classes

```haskell
class (MeetSemiLattice a, JoinSemiLattice a) => Lattice a
class (BoundedMeetSemilattice a, BoundedJoinSemiLattice a) => BoundedLattice a
```

to a much simpler hierarchy:


```haskell
class Lattice a where
    (/\) :: a -> a -> a
    (\/) :: a -> a -> a

class Lattice a => BoundedMeetSemiLattice a where
    top :: a

class Lattice a => BoundedJoinSemiLattice where
    bottom :: a
```

I won't say that the latter i.e. current hierarchy is clearly better.
But I think it has better trade-offs in Haskell.

One of the simplest examples is an expression

```haskell
\x y -> x /\ (x \/ y)
```

this is an absorption law of `Lattice`.
In `lattice-2` hierarchy you **can** simplify it to just `\x y -> x`.
But in `lattices-1` you **cannot**. Why, you may wonder?
Let us see the inferred types.
In `lattice-2` the type is:

```haskell
(\x y -> x /\ (x \/ y)) :: Lattice => a -> a -> a
```

but in `lattice-1` the inferred type is just:

```haskell
(\x y -> x /\ (x \/ y)) :: (MeetSemiLattice a, JoinSemiLattice a) => a -> a -> a
```

Note how `*SemiLattice` classes are just new names for (commutative, idempotent) monoids.
Just from that type-signature *we don't know whether structures are compatible*.
Human could reason, maybe even add a comment, but it won't be checkable
by compiler: source of errors.

Similarly, if we had a more granular hierarchy where `Applicative f = (Pointed f, Apply f)`,
the

```haskell
point f <*> x  ==>  f <$> x
```

refactoring won't be safe. Try with `Map` from `containers`,
it has incompatible `Pointed` and `Apply` instances.
(footnote: I think Edward Kmett added them just to prove this point).

Thus "the best" granular type-hierarchy we can is to have

```haskell
class Functor f where fmap :: (a -> b) -> f a -> f b
class Functor f => Apply f where (<*>) :: f (a -> b) -> f a -> f b
class Apply f => Applicative f where pure :: a -> f a
```

You can make similar example about `Default`, `Semigroup` and `Monoid`:

```haskell
def <> x  =  x  -- is it?
```

(I don't remember, *maybe* `Default` instance says that if the
type is also an instance of `Monoid` it have to be `def = mempty`,
but again - different names, and not checked by compiler).

The distilled rules for avoiding this situations is:

1. Type-class must have at least one member
2. If (and hopefully when) type-class has laws, they refer only
   to members of the type-class in question or its superclasses.

Now we see that `lattices-1` failed the first rule.
If it didn't had `Lattice` class at all,
then if would fail second class as we'd have rules like
"If `MeetSemiLattice` is also `JoinSemiLattice` then...".

`Foldable` is kind-of outlier but not really.
It's the law of `Traversable` that `traverse (Const . f) = foldMap f`.
Because `Traversable` has own members, there we can require
`Foldable` and `Traversable` be compatible.

TODO: Think if there is `Functor` and `Foldable` but not `Traversable`.
If so, the

If the type is also a Functor instance, it should satisfy

can be amended with

If the type is also a Functor than it is (most likely) a Traversable
and `foldMap` behaviour is specified by `traverse`.

One workaround to the rules above is to provide new names,
then we could have

```haskell
class              Pointed f where point :: a -> f a
class Functor f => Apply f  where (<*>) :: f (a -> b) -> f a -> f b

class (Pointed f, Apply f) => Applicative f where
    pure :: a -> f a
    pure = point
```

Then, still

```haskell
point f <*> x = f <$> x
```

in invalid rewriting, but

```haskell
pure f <*> x = f <*> x
```

is. The result is unelegant, but that's the cost of "formalizing math"
in Haskell.
