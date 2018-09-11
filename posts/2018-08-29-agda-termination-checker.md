---
title: Notes on Agda's termination checker
author: Oleg Grenrus
tags: agda
---

I'm positively surprised about Agda's termination checker.
I'm working on a larger exercise,
and Agda believes my main lemma (which does all the work for one liner theorem) terminates.
But I don't, yet.

(Also there is a recent [thread in /r/haskell](https://www.reddit.com/r/haskell/comments/98j7re/implementing_a_total_dsl/))

I asked about Agda's termination checker on `#agda` IRC channel, and
user `gallais` kindly responded that there's a paper from 20 years ago:
*`foetus` - Termination Checker for Simple Functional Programs* by Andreas Abel
http://www2.tcs.ifi.lmu.de/~abel/foetus.pdf.

Also, in the same discussion, `gallais` mentioned a "hidden" Agda feature:
fine grained reporting.  We can specify different verbosity levels for special
prefixes (parts of compiler), e.g.

```
agda -v term:5 Example.agda
```

As advised, you can find various reporting prefixes by grepping Agda's source
code for `reportS`.

Examples
--------

Let's try it out:

<h3>Braiding</h3>

This is very simple example, which should work in any language with termination checker.

```agda
open import Data.Bool
open import Data.Nat
open import Data.List

braid₁ : {A : Set} → List A → List A → List A
braid₁ []       []       = []
braid₁ []       ys       = ys
braid₁ xs       []       = xs
braid₁ (x ∷ xs) (y ∷ ys) = x ∷ y ∷ braid₁ xs ys
```

And Agda tells us:

```
termination checking body of braid₁
  : {A : Set} → List A → List A → List A
kept call from Example.braid₁ A (_∷_ x xs) (_∷_ y ys)
  to Example.braid₁ (A) (xs) (ys)
  call matrix (with guardedness): 
     =    ?    ?    ?
     ?    =    ?    ?
     ?    ?   -1    ?
     ?    ?    ?   -1
[Example.braid₁] does termination check
```

The `A` argument stays the same `=`, but **both** list argument decrease by one.
That is stronger condition than needed for termination, but it's nice Agda finds it.
There seems to be an extra dimension in the matrix, I don't know what it is :(

Agda is in fact very smart, if we rewrite the function as:

```agda
braid₂ : {A : Set} → List A → List A → List A
braid₂ []       ys   = ys
braid₂ (x ∷ xs) ys   = x ∷ braid₂ ys xs
```

Agda still thinks it terminates with:

```agda
termination checking body of braid₂
  : {A : Set} → List A → List A → List A
kept call from Example.braid₂ A (_∷_ x xs) ys
  to Example.braid₂ (A) (ys) (xs)
  call matrix (with guardedness): 
     =    ?    ?    ?
     ?    =    ?    ?
     ?    ?    ?    =
     ?    ?   -1    ?
[Example.braid₂] does termination check
```

Somehow Agda figures out an order for arguments. With higher verbosity level,
it prints:

```text
Idempotent call matrices (no dot patterns):

  --> Example.braid₂ -->
     =    ?    ?    ?
     ?    =    ?    ?
     ?    ?   -1    ?
     ?    ?    ?   -1
```

i.e the same matrix as for `braid₁`!

I'd like if Agda printed the ordering it decides from these matrices.
In this example it's trivial, in others it may be less.
Maybe that's something to contribute.

<h3>Merge sort</h3>

Next example, also mentioned in the paper, is merge from merge-sort.
This is interesting example also.
If I remember right, that's where Coq makes programmers do tricks.

```agda
merge-ord : {A : Set} → (A → A → Bool) → List A → List A → List A
merge-ord _  []           []        = []
merge-ord _  []           ys        = ys
merge-ord _  xs           []        = xs
merge-ord le xs@(x ∷ xs′) ys@(y ∷ ys′) with le x y
... | true  = x ∷ merge-ord le xs′ ys
... | false = y ∷ merge-ord le xs ys′
```

And Agda tells us

```text
termination checking body of merge-ord
  : {A : Set} → (A → A → Bool) → List A → List A → List A
kept call from Example.merge-ord A le (_∷_ x xs′) (_∷_ y ys′) (true)
  to Example.merge-ord (A) (le) (xs′) (y ∷ ys′)
  call matrix (with guardedness): 
     =    ?    ?    ?    ?    ?
     ?    =    ?    ?    ?    ?
     ?    ?    =    ?    ?    ?
     ?    ?    ?   -1    ?    ?
     ?    ?    ?    ?    =    ?
kept call from Example.merge-ord A le (_∷_ x xs′) (_∷_ y ys′) (false)
  to Example.merge-ord (A) (le) (x ∷ xs′) (ys′)
  call matrix (with guardedness): 
     =    ?    ?    ?    ?    ?
     ?    =    ?    ?    ?    ?
     ?    ?    =    ?    ?    ?
     ?    ?    ?    =    ?    ?
     ?    ?    ?    ?   -1    ?
termination checking body of .Example.with-40
  : {A : Set} (le : A → A → Bool) (x y : A) →
    Bool → (xs′ ys′ : List A) → List A

...

======================================================================
========================= New call matrices ==========================
======================================================================

Idempotent call matrices (no dot patterns):

     =    ?    ?    ?    ?    ?
     ?    =    ?    ?    ?    ?
     ?    ?    =    ?    ?    ?
     ?    ?    ?   -1    ?    ?
     ?    ?    ?    ?    =    ?

     =    ?    ?    ?    ?    ?
     ?    =    ?    ?    ?    ?
     ?    ?    =    ?    ?    ?
     ?    ?    ?    =    ?    ?
     ?    ?    ?    ?   -1    ?

[Example.merge-ord] does termination check
```

Agda doesn't tell use the lexical ordering (as `foetus` does),
but it's some what visible from the matrices above.

<h3>Tree</h3>

The last example is which made me wonder how Agda's termination checker really work.
Note: the calls are **nested**.

```agda
data Tree (A : Set) : Set where
  leaf : Tree A
  bin  : Tree A → Tree A → Tree A
  
contrived : {A : Set} → Tree A → Tree A → Tree A
contrived x leaf      = x
contrived x (bin y z) = contrived (contrived x y) z
```

And Agda tells us

```text
termination checking body of contrived
  : {A : Set} → Tree A → Tree A → Tree A
kept call from Example.contrived A x (bin y z)
  to Example.contrived (A) (x) (y)
  call matrix (with guardedness): 
     ?    ?    ?    ?
     ?    =    ?    ?
     ?    ?    =    ?
     ?    ?    ?   -1
kept call from Example.contrived A x (bin y z)
  to Example.contrived (A) (contrived x y) (z)
  call matrix (with guardedness): 
     =    ?    ?    ?
     ?    =    ?    ?
     ?    ?    ?    ?
     ?    ?    ?   -1

...

======================================================================
========================= New call matrices ==========================
======================================================================

Idempotent call matrices (no dot patterns):

  --> Example.contrived -->
     ?    ?    ?    ?
     ?    =    ?    ?
     ?    ?    ?    ?
     ?    ?    ?   -1

[Example.contrived] does termination check
```

The inner call is `= -1`: `y` is smaller than `bin y z`.
The outer call is `? -1`: `z` is smaller than `bin y z`.
So in both cases the last argument is decreasing one!

I wrote the program, and hadn't any idea why it terminates.
Agda tells me. It feels obvious afterwards, I feel silly.

A very similar function

```agda
contrived₂ : {A : Set} → Tree A → Tree A → Tree A
contrived₂ x leaf      = x
contrived₂ x (bin y z) = contrived₂ z (contrived₂ x y)
```

fails

```text
======================================================================
========================= New call matrices ==========================
======================================================================

Idempotent call matrices (dot patterns):

  --> Example.contrived₂ --> Example.contrived₂ -->
     ?    ?    ?    ?
     ?    =    ?    ?
     ?    ?    ?    ?
     ?    ?    ?    ?
```

Agda doesn't know it would terminate.
I think it is bogus too, so it is good that Agda doesn't accept that function.

Conclusion
----------

I still don't understand all the details of Agda's termination checker,
but I see it considers all the arguments in the decision. That's very useful.

You can imagine how this matrix-business extends naturally to mutually recursive
functions (no different than last example!), or to sized types (where
programmer may remove some question marks!)
