---
title: Church encoding of linear types
author: Oleg Grenrus
tags: linear
---

It's common knowledge that we can encode ADTs using so called
[Church-encoding](https://en.wikipedia.org/wiki/Church_encoding).
It's more correctly called Böhm-Berarducci encoding, as
they showed how to *systematically* make a translation in a typed setting in 1985.[^okmij]

[^okmij]: See [Beyond Church encoding: Boehm-Berarducci isomorphism of algebraic data types and polymorphic lambda-terms](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html)

```haskell
Either a b ≡ forall r. (a -> r) -> (b -> r) -> r
```

or

```
    (a, b) ≡ forall r. (a -> b -> r)
```

or

```haskell
        () ≡ forall r. r -> r
```

Sometimes these alternative variants are useful: in some mainstream
languages there are no sum types, but higher order functions can be
at least simulated. Thus: visitor pattern.

However, this blog post is not about visitor pattern, but about linear lambda calculus.
The corresponding BB-encodings are easily derived for "normal" types formed
with `×` (pair), `+` (`Either`) and recursion. Let's see how similar encodings can be
derived for types in linear lambda calculus.

<div id="toc"></div>

Short review of Böhm-Berarducci encoding
----------------------------------------

Based on [Beyond Church encoding: Boehm-Berarducci isomorphism of algebraic data types and polymorphic lambda-terms](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html).

Given a type

```haskell
data D = C1 Int
       | C2 D
       | C3 D D
```

we can define a corresponding type

```
data DDict r = DDict { c1 :: Int -> r
                     ; c2 :: r -> r
                     ; c3 :: r -> r -> r
                     }
```

A version Böhm-Berarducci encoding could be

```
type D' = forall r. DDict r -> r
```

We can curry the `DDict` record, ending up only term and type applications and abstractions:

```
type D'' = forall r. (Int -> r) -> (r -> r) -> (r -> r -> r) -> r
```

`D''` is the Böhm-Berarducci encoding of `D`.

More over, we can also define an initial algebra of `D`:

```
data DF a = C1F Int
          | C2F a
          | C3F a a
```

and we notice that `DDict r` and `DF r -> r` are isomorphic. Therefore
we get third way to represent `D`:

```
type D''' = forall r. (DF r -> r) -> r
```

For this post we won't think about recursive types.
An algebra types of non-recursive types have a phantom argument `r`,
so for non-recursive type `X` we'll have

```
X ≡ forall r. (X -> r) -> r
```

Then we can convert `X -> r` to something simpler.
In non-linear setting, we can expand both products and sums
to use only functions. Let's see what we can do in linear lambda calculus.

Linear basics
-------------

Linear lambda calculus or linear logics come in many flavours.
We will be discussing *a higher order intuitionistic linear logic*,
with ⊸, ⊗, ⊕, &, 1, 0, ⊤, and ∀ connectives.
One can think that ⊗ is a linear version ×,
and ⊕ is a linear version of +, but it's not that simple, which I'll try
to show in this blog post.

The linear type-system (or logic) rules are presented with *sequents*.  I'm
lazy, and write them in Unicode-ASCII-diagrams (Haskell highlighting works
reasonably well!) instead of LaTeX.  The simplest rule is following,
saying that if we have a thing in a context, we can use it to conclude
our proof/program:

```

------- ID
 A ⊢ A
```

We will write the *proof terms* too, using heavily Haskell-inspired syntax.

```

--------------- ID
 a : A ⊢ a : A
```

Another general rule is `CUT`, but we won't need it: Cut is admissible.
The rest of the rules are *right* and *left sequents* for particular
combinators. In right rules the connective is on the right of turnstile (⊢),
in the left the connective is on the left, respectively. (I'd like to use ⟹, to
highlight that this is a sequent calculus, not a natural deduction; but long arrows
often render funny in mono-space fonts/views).

Next we'll need to go through three connectives: ⊗, ⊸ and ∀,
before we can meaningfully talk about BB-like-encoding.

times, ⊗
--------

The linear times is probably the most simple connective, illustrating the
linearity. Its right rule is

```
 Γ ⊢ a : A    Δ ⊢ b : B
------------------------ ⊗R
 Γ;Δ ⊢ Pair a b : A ⊗ B
```

To construct a `Pair`: we split the context into two disjoint parts Γ and Δ,
and use Γ to construct the first half of a pair, and Δ to construct the second
half of a pair.  Thus we cannot write a term for `A ⊢ A ⊗ A` in linear logic.

The left rule is trickier:

```
 Γ; a : A; b : B ⊢ c : C
------------------------------------------------ ⊗L
 Γ; ab : A ⊗ B   ⊢ case ab of Pair a b -> c : C
```

We `case` on a `Pair` to get its halves `a` and `b` into the scope, and then continue.
We cannot have `fst` and `snd`, as those would "use" the whole pair,
forgetting other half, violating linearity.

linear implication, ⊸
---------------------

Linear implication uses the premise exactly once. Something you would hope
politicians to use more (neither omitting the facts, nor repeating the same
things over and over again).

The right rule for linear implication is similar to introduction
rule of normal implication

```
 Γ; a : A ⊢  b : B
---------------------- ⊸R
 Γ ⊢ λ a -> b : A ⊸ B
```

The left rule is again tricky. When we have a linear implication
in the context, we can split the rest of the context into two halves.
Then we use the other one to deduce the argument to function,
and another with new assumption `f a : B` to deduce the final conclusion.

```
 Γ ⊢ a : A    Δ ; b : B ⊢ c : C
--------------------------------------- ⊸L
 Γ;Δ; f : A ⊸ B ⊢ let b = f a in c : C
```

We can derive another rule:

```
 Δ ; b : B ⊢ c : C
--------------------------------------------- ⊸l'
 Δ ; a : A; f : A ⊸ B ⊢ let b = f a in c : C
```

which follows by taking `Γ = a : A`, and discharging the
`Γ ⊢ a : A` using `ID`-rule. The `⊸L'` rule cannot be taken as a definition,
as using it would often require `CUT` (which we avoid).

universal quantification, ∀
----------------------------

For some reason, universal quantification is often omitted from linear logics.
Its rules are similar to as in System F, as in this restricted (non-dependent)
settings types are not terms.

The rules involve *substitution* in terms, also `Y` is assumed to be fresh.
They resemble the rules for implication.
The right rule says: to construct universally quantified expression,
we need to be able to construct it for any variable name (i.e. `X` is not used in `Γ`).

```
 Γ ⊢ b : B [Y/X]
--------------------- ∀R
 Γ ⊢ Λ X -> b : ∀X.B
```

Left rule is almost exact copy of ⊸L, except we can make up new type expressions
as we go, they won't consume anything in the context.
`T type` is a pedantic addition to remind us about that, but we'll omit it for brevity.

```
 T type    Γ; b' : B [T/X] ⊢ c : C
-------------------------------------- ∀L
 Γ; b : ∀X. B ⊢ let b' = b @T in c : C
```

Now we have rules and syntax for times, implication and universal quantification,
so we can BB-encode times-connective next.

Encoding of times
-----------------

It might be easy to guess the BB-encoding of *times*:

```
A ⊗ B ≡ ∀R. (A ⊸ B ⊸ R) ⊸ R
```

That's a reasonable guess if we can show `A ⊗ B ⊸ R ≡ A ⊸ B ⊸ R` isomorphism.

Here, the `LHS ≡ RHS` notation means `LHS ⊢ RHS` and `RHS ⊢ LHS`.
The ⇒ direction is quite direct (read bottom-up):

```
------- ID
 R ⊢ R
--------------- ⊸L'
 B ; B ⊸ R ⊢ R
----------------------- ⊸L'
 A ; B ; A ⊸ B ⊸ R ⊢ R
----------------------- ⊗L
 A ⊗ B ; A ⊸ B ⊸ R ⊢ R
------------------------- ⊸R
 A ⊗ B ⊢ (A ⊸ B ⊸ R) ⊸ R
----------------------------- ∀R
 A ⊗ B ⊢ ∀R. (A ⊸ B ⊸ R) ⊸ R
```

Terms were omitted for clarity, but the last line would look like (after in lining `let`s).

```
lhs : A ⊗ B
    ⊢ Λ R -> λ f -> case lhs of Pair a b -> f a b
    : ∀R. (A ⊸ B ⊸ R) ⊸ R
```

The reverse, ⇐ direction is quite direct too
(~all proofs in this fragment of linear logic are quite direct, that's the nice thing about whole thing)

```
 ...
--------------- ⊗R
 A ; B ⊢ A ⊗ B
------------------- ⊸R²    --------------- ID
 ⊢ (A ⊸ B ⊸ A ⊗ B)          A ⊗ B ⊢ A ⊗ B
------------------------------------------ ⊸L
 (A ⊸ B ⊸ A ⊗ B) ⊸ A ⊗ B ⊢ A ⊗ B
--------------------------------- ∀L
 ∀R. (A ⊸ B ⊸ R) ⊸ R ⊢ A ⊗ B
```

The final line after inlining is:

```
rhs : ∀R. (A ⊸ B ⊸ R) ⊸ R
    ⊢ rhs @(A ⊗ B) (λ a b -> Pair a b)
    : A ⊗ B
```

These are about what you would expect in non-linear calculus for × and →.
The derivation trees would be similar.
However with ⊕ (and &) things get more interesting.

plus, ⊕
-------

The plus connective looks very much like + in non-linear logic.
I'd say, there shouldn't be anything surprising in the rules

```
 Γ ⊢ a : A
------------------- ⊕R₁
 Γ ⊢ InL a : A ⊕ B

 Γ ⊢ b : B
------------------- ⊕R₂
 Γ ⊢ InR b : A ⊕ B
```

```
 Γ ; a : A ⊢ c₁ : C    Γ ; b : B ⊢ c₂ : C
--------------------------------------------------------------- ⊕L
 Γ ; ab : A ⊕ B ⊢ case ab of { InL a -> c₁ ; InR b -> c₂ } : C
```

At this point, one could think that we could do

```
A ⊕ B ≢ ∀R. (A ⊸ R) ⊸ (B ⊸ R) ⊸ R
```

analogously to `either`. But that doesn't work: Try!
The right hand side type says that we have to use both: (A ⊸ R) and (B ⊸ R)
arguments, yet we'll need only either one. We'll need a &-connective.

with, &
-------

With, & is an interesting construction. Its rules are mirrored versions
of ⊕ rules, on the other hand they look like rules for ×.

Right rule is very similar to ⊗R, but note that both halves
are constructed using the same context:

```
 Γ ⊢ a : A    Γ ⊢ b : B
------------------------ &R
 Γ ⊢ With a b : A & B
```

The above works as left rules only allow to use either half
of the with-pair. We can choose, but cannot use both;
in ⊗L-rule we cannot choose, must use both.

```
 Γ ; a : A ⊢ c : C
------------------------------------------- &L₁
 Γ ; ab : A & B ⊢ let a = fst ab in c : C

 Γ ; b : B ⊢ c : C
------------------------------------------- &L₂
 Γ ; ab : A & B ⊢ let b = snd ab in c : C
```

Encoding of plus
----------------

Using *with* we can BB-encode *plus*.

```
A ⊕ B ≡ ∀R. ((A ⊸ R) & (B ⊸ R)) ⊸ R
```

That's using

```
A ⊕ B ⊸ R ≡ (A ⊸ R) & (B ⊸ R)
```

distributivity property.

Instead of derivation tree, I only write the proof terms. The ⇒ direction:

```
lhs : A ⊕ B
    ⊢ Λ R -> λ f -> case lhs of
      InL a -> fst f a
      InR b -> snd f b
    : ∀R. ((A ⊸ R) & (B ⊸ R)) ⊸ R
```

Depending on a `lhs` branch we pick proper half of with argument.

The reverse, ⇐ direction

```
rhs : ∀R. ((A ⊸ R) & (B ⊸ R)) ⊸ R
    ⊢ rhs @(A ⊕ B) (With (λ a -> InL a) (λ b -> InR b))
    : A ⊕ B
```

Encoding of with
----------------

So let's try to write a BB-encoding for A & B. Reasonable guess is

```
A & B ≢ ∀R. ((A ⊸ R) ⊕ (B ⊸ R)) ⊸ R
```

The ⇒ direction works out:

```
lhs : A & B
    ⊢  Λ R -> λ arbr -> case arbr of
       InL ar -> ar (fst lhs)
       InR br -> br (snd lhs)
    : ∀R. ((A ⊸ R) ⊕ (B ⊸ R)) ⊸ R
```

However the opposite direction doesn't:

```
rhs : ∀R. ((A ⊸ R) ⊕ (B ⊸ R)) ⊸ R
    ⊢ rhs @(A & B) ???
    : A & B
```

where ⊢ ??? : (A ⊸ A & B) ⊕ (B ⊸ A & B). If we knew the future,
we could put the correct `InL` or `InR` argument for `???`, to match
the later happening `fst` or `snd`. But we don't know how the result
will be used.

We have to give up.

If we lookup in
[Girard's Linear Logic](https://doi.org/10.1016/0304-3975(87)90045-4)
we'll learn that
there are following properties

```
A ⊗ B ⊸ R ≡ A ⊸ B ⊸ R
A ⊕ B ⊸ R ≡ (A ⊸ R) & (B ⊸ R)
```

but the third one is only half-distributive, i.e. works only in one direction.

```
A & B ⊸ R ⇒ (A ⊸ R) ⊕ (B ⊸ R)
```

Unfortunate, but known fact. So, we cannot (at least obviously) simulate
A & B using something else.
Maybe *with* is an essential connective, you simply cannot admit. In other words,
if you want something like *with*, you need to add that.

One way to understand linear logic is
[the resource-interpretation](https://en.wikipedia.org/wiki/Linear_logic#The_resource_interpretation).
If we set A = 2€, B = 3$, R = soda, then

- `A & B ⊸ R` means "we put both 2€ and 3$ into a machine, it will make a choice (not using other currency) and give use a soda back"
- `(A ⊸ R) ⊕ (B ⊸ R)` means "we make a choice, whether to put in 2€ and get a soda, or to put in 3$ and get a soda"

This example is especially relevant when paying with a credit card.
You may pay in a local currency or your currency.
If the exchange rate is fair, i.e. A ≈ B, then the two approaches are essentially
the same. But when A ≉ B, maybe it's good to have a choice.

Note, that ⇒ direction is a machine asking you to make a choice (often they do),
but it could make choice for you (charge local currency if you use contactless; or better option for merchant).
But if you use cash, making a choice which currency to use, you cannot be charged price in different one.

one, 1; zero, 0; top, ⊤
------------------------

Story with units follows the lines of related binary-connectives.

One, 1 is unit of times. One doesn't carry
any information, so we can freely introduce and remove it from the context.
t has expected rules:

```
----------- 1R
 ⊢ One : 1
```

```
 Γ         ⊢ c : C
------------------- 1L
 Γ ; x : 1 ⊢ c : C
```

Zero, 0 is unit of plus. It has only a left rule, if there's a value of type 0
in a context, "all is lost", anything is derivable.

```
------------------------- 0L
 Γ; x : 0 ⊢ absurd x : C
```

Top, ⊤ is unit of with. It has only a right rule. It's an interesting
type which let us dismiss stuff, putting "boring" stuff a side.
But we cannot ever get completely rid of of them, neither extract back.
A bit like black hole, I guess.

```
------------------------- ⊤R
 Γ ⊢ boring {x y...} : ⊤
```

Corresponding BB-encodings are

```
1 ≡ ∀R. R ⊸ R
```

Conversions: an exercise.

```
0 ≡ ∀R. R
```

The ⇒ direction is given by `absurd`. The ⇐ by instantiating with `@0`.

⊤, as &, cannot really be admitted. We can squash everything into ⊤,
but it's impossible to get rid of it.

Conclusion
----------

In higher order linear lambda calculus, type and term abstraction
and application is not enough. The plus & and top ⊤ stand out.

On the other hand, if your linear language doesn't have plus and top, there are
still a lot of you can do. Then the setup is close to normal lambda calculus;
yet subtly different, you must be aware that not all tricks will work out.

This post is a not-so-gentle introduction into linear lambda calculus.
I plan to write-mumble more, so this post will act as a reference point.

Appendix: All rules in one place
--------------------------------

```
--------------- ID
 a : A ⊢ a : A
```

```
 Γ ⊢  a : A    Δ; a' : A ⊢ b : B
--------------------------------- CUT
 Γ;Δ ⊢ let a' = a in b : B
```

```
 Γ; a : A ⊢  b : B
---------------------- ⊸R
 Γ ⊢ λ a -> b : A ⊸ B
```

```
 Γ ⊢ a : A    Δ ; b : B ⊢ c : C
--------------------------------------- ⊸L
 Γ;Δ; f : A ⊸ B ⊢ let b = f a in c : C
```

```
 Γ ⊢ a : A    Δ ⊢ b : B
------------------------ ⊗R
 Γ;Δ ⊢ Pair a b : A ⊗ B
```

```
 Γ; a : A; b : B ⊢ c : C
------------------------------------------------ ⊗L
 Γ; ab : A ⊗ B   ⊢ case ab of Pair a b -> c : C
```

```
 Γ ⊢ b : B [Y/X]
--------------------- ∀R
 Γ ⊢ Λ X -> b : ∀X.B
```

```
 T type    Γ; b' : B [T/X] ⊢ c : C
-------------------------------------- ∀L
 Γ; b : ∀X. B ⊢ let b' = b @T in c : C
```

```
 Γ ⊢ a : A
------------------- ⊕R₁
 Γ ⊢ InL a : A ⊕ B

 Γ ⊢ b : B
------------------- ⊕R₂
 Γ ⊢ InR b : A ⊕ B
```

```
 Γ ; a : A ⊢ c₁ : C    Γ ; b : B ⊢ c₂ : C
--------------------------------------------------------------- ⊕L
 Γ ; ab : A ⊕ B ⊢ case ab of { InL a -> c₁ ; InR b -> c₂ } : C
```

```
 Γ ⊢ a : A    Γ ⊢ b : B
------------------------ &R
 Γ ⊢ With a b : A & B
```

```
 Γ ; a : A ⊢ c : C
------------------------------------------- &L₁
 Γ ; ab : A & B ⊢ let a = fst ab in c : C

 Γ ; b : B ⊢ c : C
------------------------------------------- &L₂
 Γ ; ab : A & B ⊢ let b = snd ab in c : C
```

```
----------- 1R
 ⊢ One : 1
```

```
 Γ         ⊢ c : C
------------------- 1L
 Γ ; x : 1 ⊢ c : C
```

```
------------------------- 0L
 Γ; x : 0 ⊢ absurd x : C
```

```
---------------- ⊤R
 Γ ⊢ boring {x y...} : ⊤
```
