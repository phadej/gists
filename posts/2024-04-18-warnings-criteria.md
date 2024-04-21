------------------------------------------
title: What makes a good compiler warning?
author: Oleg Grenrus
------------------------------------------

Recently I came up with a criteria for a good warning to have in a compiler:

<blockquote>
If compiler makes a choice, or has to deal with some complication, it may well tell about that.
</blockquote>

That made me think about warnings I implemented into GHC over the years. They are fine.

Let us first understand the criteria better. It is better explained by an example which triggers few warnings:

```haskell
foo :: Char
foo = let x = 'x' in
      let x = 'y' in x
```

First warning is [`-Wname-shadowing`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wname-shadowing):

```
Shadow.hs:3:11: warning: [-Wname-shadowing]
    This binding for ‘x’ shadows the existing binding
      bound at Shadow.hs:2:11
  |
3 |       let x = 'y' in x
  |           ^
```

When resolving names (i.e. figuring out what textual identifiers refer to) compilers
have a choice what to do with duplicate names. 
The usual choice is to pick the closest reference, shadowing others.
But it's not the only choice, and not the only choice GHC does in similar-ish situations. e.g. module's top-level definition *do not shadow* imports; instead an ambiguous name error is reported. Also `\ x x -> x` is rejected (treated as a non-linear pattern), but `\x -> \x -> x` is accepted (two separate patterns, inner one shadows).
So, in a way, `-Wname-shadowing` reminds us what GHC does.

Another warning in the example is [`-Wunused-binds`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-binds):

```haskell
Shadow.hs:2:11: warning: [-Wunused-local-binds]
    Defined but not used: ‘x’
  |
2 | foo = let x = 'x' in
  |           ^
```

This a kind of warning that compiler might figure out in the optimisation passes
(I'm not sure if GHC always tracks usage, but IIRC GCC had some warnings triggered only when optimisations are on).
When doing usage analysis, compiler may figure out that some bindings are unused, so it doesn't need to generate code for them.
At the same time it may warn the user.

More examples
-------------

Let go through few of the [numerous warnings GHC can emit](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#warnings-and-sanity-checking).

[`-Woverflowed-literals`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Woverflowed-literals) causes a warning to be emitted if a literal will overflow.
It's not strictly a compiler choice, but a choice nevertheless in `base`'s `fromInteger` implementations.
For most types [^natural] the `fromInteger` is a total function with rollover behavior: `300 :: Word8` is `44 :: Word8`.
It could been chosen to not be total too, and IMO that would been ok if `fromInteger` were used *only* for desugaring literals.

[^natural]: With `-XNegativeLiterals` and `Natural`, `fromInteger` may result in run-time error though, for example:

    ```
    <interactive>:6:1: warning: [-Woverflowed-literals]
        Literal -1000 is negative but Natural only supports positive numbers
    *** Exception: arithmetic underflow
    ```

[`-Wderiving-defaults`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wderiving-defaults):
Causes a warning when both `DeriveAnyClass` and `GeneralizedNewtypeDeriving` are enabled and no explicit deriving strategy is in use.
This a great example of a choice compiler makes.
I actually don't remember which method GHC picks then,
so it's good that compiler reminds us that it is good idea to be explicit (using `DerivingStrategies`).

[`-Wincomplete-patterns`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wincomplete-patterns)
warns about places where a pattern-match might fail at runtime.
This a complication compiler has to deal with.
Compiler needs to generate some code to make all pattern matches complete.
An easy way would been to always implicitly default cases to all pattern matches, but that would have performance implications, so GHC checks pattern-match coverage,
and as a side-product may report incomplete pattern matches (or [`-Winaccesible-code`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Winaccessible-code))
[^pmcheck].

[^pmcheck]: Using [`-fmax-pmcheck-models`] we could *almost* turn off GHCs pattern-match coverage checker, which will make GHC consider (almost) all pattern matches as incomplete.
So `-Wincomplete-patterns` is kind of an example of a warning which is powered by an "optional" analysis is GHC.

[`-Wmissing-fields`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-fields)
warns you whenever the construction of a labelled field constructor isn’t complete, missing initialisers for one or more fields.
Here compiler needs to fill the missing fields with something, so it warns when it does.

[`-Worphans`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Worphans) gets an honorary mention.
Orphans cause so much incidental complexity inside the compiler, that I'd argue that `-Worphans` should be enabled by default (and not only in `-Wall`).

Bad warnings
------------

[`-Wmissing-import-lists`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-import-lists)
warns if you use an unqualified `import` declaration that does not explicitly list the entities brought into scope.
I don't think that there are any complications or choices compiler needs to deal with,
therefore I think this warning should been left for style checkers.
(I very rarely have import lists for modules from the same package or even project; and this is mostly a style&convenience choice).

[`-Wprepositive-qualified-module`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wprepositive-qualified-module)
is even more of an arbitrary style check. With `-Wmissing-import-lists` it is generally accepted that explicit import lists are better for compatibility
(and for GHCs recompilation avoidance).
Whether you place `qualified` before or after the module name is a style choice. I think this warning shouldn't exist in GHC.
(For the opposite you'd need a style checker to warn if `ImportQualifiedPost` is enabled anywhere).

Note, while [`-Wtabs`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wtabs) is also mostly a style issue,
but the compiler has to make a choice how to deal with them.
Whether to always convert tabs to 8 spaces, convert to next 8 spaces boundary, require indentation to be exactly the same spaces&tabs combination.
All choices are sane (and I don't know which one GHC makes), so a warning to avoid tabs is justified.

Compatibility warnings
----------------------

Compatibility warnings are usually good also according to my criteria.
Often it is the case that there is an old and a new way of doing things.
Old way is going to be removed, but before removing it, it is deprecated.

[`-Wsemigroup`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wsemigroup)
warned about `Monoid` instances without `Semigroup` instances. (A warning which you shouldn't be able to trigger with recent GHCs).
Here we could not switch to new hierarchy immediately without breaking some code,
but we could check whether the preconditions are met for awhile.

[`-Wtype-equality-out-of-scope`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wtype-equality-out-of-scope)
is somewhat similar. For now, there is some compatibility code in GHC, and GHC warns when that fallback code path is triggered.

My warnings
-----------

One of the warning I added is [`-Wmissing-kind-signatures`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-kind-signatures).
For long time GHC didn't have a way to specify kind signatures until [`StandaloneKindSignatures`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-StandaloneKindSignatures) were added in GHC-8.10.
Without kind signatures GHC must *infer* kind of a data type or type family declaration. With kind signature it could just check against given kind (which is a technically a lot easier).
So while the warning isn't actually implemented so, it could be triggered when GHC notices it needs to infer a kind of a definition.
In the implementation the warning is raised *after* the type-checking phase, so the warning can include the inferred kind.
However, we can argue that when inference fails, GHC could also mention that the kind signature was missing.
Adding a kind signature often results in better kind errors (c.f. adding a type signature often results in a better type error when something is wrong).

The [`-Wmissing-poly-kind-signatures`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-poly-kind-signatures) warning seems like a simple restriction of above,
but it's not exactly true. There is another problem GHC deals with.
When GHC infers a kind, there might be unsolved meta-kind variables left, and GHC has to do something to them.
With [`PolyKinds`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-PolyKinds) extension on, GHC *generalises* the kind. For example when inferring a kind of `Proxy` as in

```haskell
data Proxy a = Proxy
```

GHC infers that the kind is `k -> Type` for some `k` and with `PolyKinds` it generalises it to `type Proxy :: forall {k}. k -> Type`.
Another option, which GHC also may do (and does when `PolyKinds` are not enabled) is to *default* kinds to `Type`, i.e. `type Proxy :: Type -> Type`.
There is no warning for kind defaulting, but arguable there should be as defaulted kinds may be wrong.
(Haskell98 and Haskell2010 don't have a way to specify kind signatures; that is clear design deficiency; which was first resolved by [`KindSignatures`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/kind_signatures.html) and finally more elegantly by `StandaloneKindSignatures`).

There is defaulting for type variables, and (in some cases) GHC warns about them. You probably have seen `Defaulting the type variable ‘a0’ to type ‘Integer’` warnings caused by [`-Wtype-defaults`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wtype-defaults).
Adding `-Wkind-defaults` to GHC makes sense, even only for uniformity between (types of) terms and types;
or arguably nowadays it is a sign that you should consider enabling `PolyKinds` in that module.


About errors
------------

The warning criteria also made me think about the following: the error *hints* are by necessity imprecise.
If compiler knew exactly how to fix an issue, maybe it should just fix it and instead only raise a warning.

GHC has few of such errors.
For example when using a syntax guarded by an extension.
It can be argued (and IIRC was recently argued in discussions around GHC language editions) that another design approach would be simply accept new syntax,
but just warn about it.
The current design approach where extensions are "feature flags" providing some forward and backward compatibility is also defendable.

Conversely, if there is a case where compiler kind-of-knows what the issue is,
but the language is not powerful enough for compiler to fix the problem on its own,
the only solution is to raise an error.
Well, there is another: (find a way to) extend the language to be more expressive, so compiler could deal with the currently erroneous case.
Easier said than done, but in my opinion worth trying.

An example of above would be [`-Wmissing-binds`](https://github.com/ghc-proposals/ghc-proposals/issues/544#issuecomment-1300407772) .
Currently writing a type signature without a corresponding binding is a hard error.
But compiler could as well fill it in with a dummy one,  That would complement `-Wmissing-methods` and `-Wmissing-fields`.
Similarly for types, a standalone kind signature tells the compiler already a lot about the type even without an actual definition:
the rest of the module can treat it as an opaque type.

Another example is briefly mentioned making module-top-level definitions shadow imports.
That would make adding new exports (e.g. to implicitly imported `Prelude`) less affecting. 
While we are on topic of names, GHC could also report early when imported modules have ambiguous definitions, e.g.

```haskell
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Lazy as Lazy
```

doesn't trigger any warnings. But if you try to use `Lazy.unpack` you get an ambiguous occurrence error.
GHC already deals with the complications of ambiguous names, it could as well have an option to report them early. 

Conclusion
----------

<blockquote>
If compiler makes a choice, or has to deal with some complication, it may well tell about that.
</blockquote>

Seems like a good criteria for a good compiler warning.
As far as I can tell most warnings in GHC pass it; but I found few "bad" ones too.
And also identified at least one warning-worthy case GHC doesn't warn about.
