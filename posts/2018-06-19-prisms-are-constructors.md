---
title: Prisms are constructors
author: Oleg Grenrus
tags: lens, notes
---

Prisms could be introduced as *first-class pattern matching*, but that is a
one-sided view. I'd say they are **generalised constructors**, though maybe
more often used for pattern matching than for actual construction.

The important property of constructors (and *lawful prisms*), is their
injectivity.  Though the usual prism laws don't state that directly,
injectivity property can be deduced.

To quote `lens` documentation, the prisms laws are:

First, if I `review` a value with a `Prism` and then `preview`, I will get it back:

```haskell
preview l (review l b) ≡ Just b
```

Second, if you can extract a value a using a `Prism` `l` from a value `s`, then
the value `s` is completely described by `l` and `a`:

```haskell
preview l s ≡ Just a ⇒ review l a ≡ s
```

In fact, the first law alone is enough to prove the injectivity of construction
via `Prism`:

```haskell
review l x ≡ review l y ⇒ x ≡ y
```

The proof is straight-forward:

```haskell
review l x ≡ review l y
  -- x ≡ y -> f x ≡ f y
preview l (review l x) ≡ preview l (review l y)
  -- rewrite both sides with the first law
Just x ≡ Just y
  -- injectivity of Just
x ≡ y
```

We can use injectivity property as an additional tool in the equational
reasoning toolbox. Or we can use it as a easy property to check to decide
whether something is a lawful `Prism`.  The check is easy as we only the
`review` side of `Prism`. Many *smart constructors*, which for example
normalise the input data, aren't lawful prisms.

An example using [`case-insensitive`](https://hackage.haskell.org/package/case-insensitive-1.2.0.11):

```haskell
-- Bad!
_CI :: FoldCase s => Prism' (CI s) s
_CI = prism' ci (Just . foldedCase)

λ> review _CI "FOO" == review _CI "foo"
True

λ> "FOO" == "foo"
False
```

The first law is also violated:

```haskell
λ> preview _CI (review _CI "FOO")
Just "foo"
```

Pattern synonyms
----------------

We have to note, that as simply as you can construct lawless prisms,
similarly simply you can construct "surprising" pattern synonyms

```haskell
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
import Data.CaseInsensitive

pattern CI :: FoldCase a => a -> CI a
pattern CI a <- (foldedCase -> a) where CI a = mk a

λ> case CI "FOO" of CI s -> s
"foo"
```

That pattern will be surprising to downstream users, precisely because it's not
injective, therefore even it looks like a constructor, it's not.

For compiler writers it means, that expression `case P x of P y -> z`, where `P` is a
pattern synonym, cannot be beta-reduced, but compiler needs to expand patterns first.
(for a pattern synonym obeying prism laws, that would be safe to do).

Dual: Surjectivity of Lens
--------------------------

Similarly, `view` through `Lens` is surjective:

```haskell
forall a. exists s. view l s ≡ a
```

We can prove that using the first lens law, *GetPut*:

```haskell
view l (set l a s) ≡ a
```

If there exists *any* value `s'`, then the required `s` is `set l a s`, as then
`view l (set l a s) ≡ a`, exactly the *GetPut* law. Otherwise, if there aren't
any `s'` values, the reasoning isn't that interesting. This is classical
argument, but hopefully I can hand wave through it.

```haskell
-- There is a field for every type in the Void.
devoid :: Lens' Void Void a b
devoid = lens absurd (\v _ -> v)
```

Appendix: Coq
-------------

The above is simple to formalise in Coq:

```coq
Variable Prism : Type -> Type -> Type.

Variable preview : forall {S A : Type} (p : Prism S A) (s : S), option A.
Variable review : forall {S A : Type} (p : Prism S A) (a : A), S.

Definition LawP1 (S A : Type) (p : Prism S A) : Type :=
 forall (a : A), preview p (review p a) = Some a.

Definition LawP2 (S A : Type) (p : Prism S A) : Type :=
  forall (s : S) (a : A),
    preview p s = Some a -> review p a = s.

Definition PrismInj (S A : Type) (p : Prism S A) : Type :=
  forall (x y : A),
    review p x = review p y -> x = y.

Lemma lemmaPrismInj (S A : Type) (p : Prism S A) :
  LawP1 S A p -> PrismInj S A p.
Proof.
  unfold PrismInj. unfold LawP1.
  intros lawp1 x y H.
  assert (Some x = Some y).
  rewrite <- (lawp1 x).
  rewrite <- (lawp1 y).
  f_equal. exact H.
  injection H0. auto. Qed.


Variable Lens : Type -> Type -> Type.

Variable view : forall {S A : Type} (l : Lens S A) (s : S), A.
Variable set : forall {S A : Type} (l : Lens S A) (a : A) (s : S), S.

Definition LawL1 (S A : Type) (l : Lens S A) : Type :=
  forall (s : S) (a : A),   
    view l (set l a s) = a.

Definition LawL2 (S A : Type) (l : Lens S A) : Type :=
  forall (s : S), set l (view l s) s = s.

Definition LensSurj (S A : Type) (l : Lens S A) : Type :=
  forall (a : A), exists (s : S), view l s = a.

Lemma lemmaLensSurj (S A : Type) (l : Lens S A) (s : S) :
  LawL1 S A l -> LensSurj S A l.
Proof.
  unfold LawL1. unfold LensSurj. intros lawl1 a.
  exists (set l a s).
  apply lawl1.
  Qed.
```

Acknowledgements
----------------

Thanks to Alp Mestanogullari for suggesting a section about `PatternSynonyms`.
