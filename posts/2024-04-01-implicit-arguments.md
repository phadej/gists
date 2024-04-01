-------------------------
title: Implicit arguments
author: Oleg Grenrus
-------------------------

In programming languages with sophisticated type systems we easily run into inconvenience of providing many (often type) arguments explicitly.
Let's take a simple `map` function as an example:

```haskell
map :: forall a b. (a -> b) -> List a -> List b
```

If we had to always explicitly provide `map`'s arguments, write something like

```haskell
ys = map @Char @Char toLower xs
```

we would immediately give up on types, and switch to use some dynamically typed programming language.
It wouldn't be fun to state "the obvious" all the time.

Fortunately we know a way ([unification](https://en.wikipedia.org/wiki/Unification_(computer_science))) which can be used to *infer* many such argument. Therefore we can write 

```haskell
ys = map toLower xs
```

and the type arguments will be inferred by compiler. However we usually are able to be explicit if we want or need to be, e.g. with [`TypeApplications`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html) in GHC Haskell.

Beyond Hindley-Milner
---------------------



Conor McBride calls a following phenomenon ["Milner's Coincidence"](https://stackoverflow.com/a/13241158):

<blockquote>
<p>The Hindley-Milner type system achieves the truly awesome coincidence of four distinct distinctions</p>

<ul>
<li>terms vs types</li>
<li>explicitly written things vs implicitly written things</li>
<li>presence at run-time vs erasure before run-time</li>
<li>non-dependent abstraction vs dependent quantification</li>
</ul>

<p>We’re used to writing terms and leaving types to be inferred. . . and then erased. We’re used
to quantifying over type variables with the corresponding type abstraction and application happening silently and statically.</p>
</blockquote>

GHC Haskell type-system has been long far more expressive than vanilla Hindley-Milner, and the four distrinctions are already misaligned.

GHC developers are filling the cracks: For example we'll soon [^visqua] get a `forall a ->` (with an arrow, not a dot) quantifier, which is *erased* (irrelevant), *explicit* (visible) dependent quantification. Later we'll get `foreach a.` and `foreach a ->` which are retained (i.e. not-erased, relevant) implicit/explicit dependent quantification.

[^visqua]: [GHC-9.10.1 release notes (for alpha1)](https://www.haskell.org/ghc/blog/20240313-ghc-9.10.1-alpha1-released.html)
mention "Partial implementation of the [GHC Proposal #281](https://github.com/ghc-proposals/ghc-proposals/pull/281), allowing visible quantification to be used in the types of terms."

(Agda also has "different" quantifiers:
explicit `(x : A) -> ...` and implicit `{y : B} -> ...` dependent quantifiers, and [erased variants](https://agda.readthedocs.io/en/v2.6.4.3/language/runtime-irrelevance.html) look like `(x : @0 A) -> ...` and `(y : @0 B) -> ...`.)

In Haskell, if we have a term with implicit quantifier (`foo :: forall a. ...`), we can use `TypeApplications` syntax to apply the argument explicitly:

```haskell
bar = foo @Int
```

If the quantifier is explicit, we'll (eventually) write just

```haskell
bar = foo Int
```

or

```haskell
bar = foo (type Int)
```

for now.

Inferred type variables
-----------------------

That all is great, but consider we define a kind-polymorphic[^kind] type like

[^kind]: kind is type of types.

```haskell
type ProxyE :: forall k. k -> Type
data ProxyE a = MkProxyE
```

then when used at type level, `forall` behaves as previously, constructors

```haskell
ghci> :kind ProxyE Int
ProxyE Int :: Type

ghci> :kind ProxyE @Type Int
ProxyE @Type Int :: Type
```

The type of constructor `MkProxyE` is

```haskell
ghci> :type ProxyE
ProxyE :: forall k (a :: k). ProxyE @k a
```

So if we want to create a term of type `Proxy Int`, we need to provide both `k` and `a` arguments:

```haskell
ghci> :type ProxyE @Type @Int
ProxyE @Type @Int :: ProxyE @(Type) Int
```

we could also jump over `k`:

```haskell
ghci> :type MkProxyE @_ @Int
MkProxyE @_ @Int :: ProxyE @(*) Int
```

The above skipping over arguments is not convenient, luckily GHC has a feature, created for other needs, which we can (ab)use here.
There are [*inferred* variables](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#inferred-vs-specified-type-variables) (though the better name would be "very hidden"), these are arguments for which `TypeApplication` doesn't apply:

```haskell
type Proxy :: forall {k}. k -> Type
data Proxy a = MkProxy
```

This is the way `Proxy` is defined in `base` (but I renamed the constructor to avoid name ambiguity)

And while GHCi prints

```haskell
ghci> :type MkProxy @Int
MkProxy @Int :: Proxy @{Type} Int
```

the `@{A}` syntax is not valid Haskell, so we cannot explicitly apply inferred variables. Neither we can in types:

```haskell
ghci> :kind! Proxy @{Type}

<interactive>:1:10: error: parse error on input ‘Type’
```

I think this is plainly wrong, we should be able to apply these "inferred" arguments too.

The counterargument is that, *inferred* variables weren't meant to be "more implicit" variables.
As GHC manual explains, inferred variables are a solution to `TypeApplications` with inferred types.
We need to know the order of variables to be able to apply them; but especially in presence of type-class constraints the order is arbitrary. 

I'm not convinced, I think that ability to be fully explicit is way more important than a chance to write brittle code.

One solution, which I think would work, is simply to not generalise. This is controversial proposal, but as GHC Haskell is moving towards having fancier type system, something needs to be sacrificed. ([`MonoLocalBinds`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/let_generalisation.html#extension-MonoLocalBinds) is for local bindings, but I'd argue that should be for all bindings, not only local).

The challenge has been that library writes may not been aware of `TypeApplications`, but today they have no choice.
Changing from `foo :: forall a b. ...` to `foo :: forall b a. ...` may break some code (even though PVP doesn't explicitly write that down, that should be common sense).

So in the GHC manual example

```haskell
f :: (Eq b, Eq a) => a -> b -> Bool
f x y = (x == x) && (y == y)

g x y = (x == x) && (y == y)
```

the `g` would fail to type-check because there are unsolved type-variables. One way to think about this is that GHC would refuse to pick an order of variables. GHC could still generalise if there are no dictionary arguments, but on the other hand I don't think it would help much.
It might help more if GHC wouldn't specialise as much, then

```haskell
h = f
```

would type-check.

This might sound like we would need to write much many type signatures. I don't think that is true: it's already a best practice to write type signatures for type level bindings, and for local bindings we would mostly need to give signatures to function bindings.

This proposal subsumes [monomorphism restriction](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/monomorphism.html#extension-NoMonomorphismRestriction), recall that without type defaulting:

```haskell
-- turn off defaulting
default ()
fooLen = genericLength "foo"
```

will fail to compile with

```
Ambiguous type variable ‘i0’ arising from a use of ‘genericLength’
prevents the constraint ‘(Num i0)’ from being solved.
```

error. With `NoMonomophismRestriction` we have 

```haskell
ghci> :t fooLen
fooLen :: Num i => i
```

Let me summarise the above: If we could apply inferred variables, i.e. use curly brace application syntax, we would have complete explicit `forall a ->`, implicit `forall a.` and *more implicit* `forall {a}.` dependent quantifiers.
Currently the `forall {a}.` quantifier is incomplete: we can abstract, but we cannot apply.
We'll also need some alternative solution to `TypeApplicaitons` and inferred types.
We should be able to bind these variables explicitly in lambda abstractions as well: `\ a -> `, `\ @a -> ` and `\ @{a} -> ` respectively (see [`TypeAbstractions`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_abstractions.html?highlight=typeabstractions#type-abstractions-in-functions)).

Alternatives
------------

The three level explicit/implicit/impliciter arguments may feel complicated. Doesn't other languages have similar problems, how they solve them?

As far as I'm aware Agda and Coq resolve this problem by supporting applying implicit arguments *by name*:

```agda
-- using indices instead of parameters,
-- to make constructor behave as in Haskell
data Proxy : {k : Set} (a : k) -> Set1 where
  MkProxy : {k : Set} {a : k} -> Proxy a

t = MkProxy {a = true}
```

Just adding named arguments to Haskell would be a bad move. It would add another way where a subtle and well-meaning change in the library could break downstream. For example unifying the naming scheme of type-variables in the libraries, so they are always `Map k v` and not `Map k a` sometimes, as it is in `containers` which uses both variable namings.

We could require library authors to explicitly declare that bindings in a module can be applied by name (i.e. that they have thought about the names, and recognise that changing them will be breaking change). You would still be able to always explicitly apply implicit arguments, but sometimes you won't be able to use more convenient named syntax.

It is fair to require library authors to make adjustments so that (numerous) library users would be able to use a new language feature with that library. In a healthy ecosystem that shouldn't be a problem. Specifically it is extra fair, if the alternative is to make feature less great, as then people might not use it at all.

Infinite level of implicitness
------------------------------

Another idea is to embrace implicit, more implicit and even more implicit arguments. Agda has two levels: explicit and implicit, GHC Haskell has two and a half, why stop there?

If we could start fresh, we could pick Agda's function application syntax and have

```haskell
funE arg    -- explicit application
funI {arg}  -- explicit application of implicit argument
```

but additionally we could add

```haskell
funJ {{arg}}    -- explicit application of implicit² argument
funK {{{arg}}}  -- explicit application of implicit³ argument
...             -- and so on
```

With unlimited levels of implicitness we could define `Proxy` as

```haskell
type Proxy :: forall {k} -> k -> Type
data Proxy a where
    MkProxy :: forall {{k}} -> {a :: k} -> Proxy a
```

and use it as `MkProxy`, `MkProxy {Int}` or `MkProxy {{Type}} {Int} :: Proxy Int`.
Unlimited possibilities.

For what it is worth, the implementation should be even simpler than of named arguments.

But I'd be quite happy already if GHC Haskell had a way to explicitly apply any function arguments, be it three levels (ordinary, `@arg` and `@{arg}`) of explicitness, many or just two; and figured another way to tackle `TypeApplications` with inferred types.
