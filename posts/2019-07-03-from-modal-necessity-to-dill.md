---
title: From modal necessity in ILL to DILL
author: Oleg Grenrus
tags: linear
---

This post is my notes showing that if you apply a modal necessity construction
as described by Pfenning and Davies[^judgmental] to linear logic,
you get dual intuitionistic linear logic[^dill] by Barber.
Maybe this is a well known fact, please tell me if it is so!
Also we'll connect that to recent developments, where variables have
*multiplicities*.

[^judgmental]: Frank Pfenning and Rowan Davies,
  [A judgmental reconstruction of modal logic](https://doi.org/10.1017/S0960129501003322) (2001)

[^dill]: Andrew Barber,
  [Dual Intuitionistic Linear Logic](http://www.lfcs.inf.ed.ac.uk/reports/96/ECS-LFCS-96-347/) (1994)

Validity
--------

Let us briefly recap necessity/validity from *a judgmental reconstruction of modal logic*[^oplss].
There are judgments which do not depend on hypotheses about the truth of propositions.
Pfenning and Davies introduce the new judgment that A is valid.
Evidence for the validity of A is simply unconditional evidence for A.

[^oplss]: A modal logic is a logic with fancy prefix operators: modalities. I learned about that paper following [Proof Theory Foundations -videos](https://www.cs.uoregon.edu/research/summerschool/summer14/curriculum.html) by Brigitte Pientka at Oregon Programming Languages Summer School (OPLSS) 2014.

1. If `∙ ⊢ A true` then `A valid`.
2. If `A valid` then `Γ ⊢ A true`.

After that Pfenning and Davies internalize the judgment as a proposition,
we'll follow their notation and write `□A` for the proposition expressing
that A is valid.

At this point we should note that these all makes sense in a linear logic setting.
Using [the resource-interpretation](https://en.wikipedia.org/wiki/Linear_logic#The_resource_interpretation) of linear logic,
we can say that if A is valid then to construct A we don't need any (linear) resources:
it's an environment-friendly thing.

Pfenning and Davies then separate context into *true* and *valid* parts.
We'll do the same, but use Γ for a collection of valid assumptions,
and Δ for the collection of true assumptions. (i.e. swapping their notation).
At this point it becomes clear that Γ is intuitionistic context, and Δ is linear context.
We'll write contexts as `Γ, a : A ; Δ , b : B` separating context parts by
semicolon, and individual assumptions (or parts) by comma. For empty
part we'll write `∙`.

Next we need to adapt the rules for the linear setting.

**Hypothesis / variable / axiom.** We change the `ID` rule (from [my last post](2019-06-26-linear-church-encodings.html)) to allow any intuitionistic
context. Here `Δ = a : A`

```
------------------ Lin-ID
 Γ; a : A ⊢ a : A
```

Using an assumption from intuitionistic context doesn't "consume" anything.
Note that linear part is required to be empty, `Δ = ∙`

```
---------------------- Int-ID
 Γ, a : A ; ∙ ⊢ a : A
```

**Right rule for □.** This rule is very simple, note how the  `Δ = ∙` on both sides of the line.

```
 Γ ; ∙ ⊢ a : A
------------------ □R
 Γ ; ∙ ⊢ box a : □A
```

**Left rule for □.** The correct rule is not immediately obvious,
but here it is anyway

```
 Γ , a : A ; Δ  ⊢  c : C
--------------------------------------------- □L
 Γ ; Δ , ba : □A ⊢ case ba of box a -> c : C
```

These rules feels weird. We can show that rules are locally *sound* and *complete*.
Local soundness describes a single step of admissibility of cut, kind of β-reduction step.
Local completeness describes a way to η-expand. These are simple checks,
so we know that right and left rules are well-balanced: not too powerful nor weak.

**Local soundness**
The way right and left rules are set up, a proof term with right rule on the
left, and left rule on the right have to be glued together with `CUT`.
If `CUT` is admissible (hopefully it is), then we can reduce the term,
removing the `CUT` rule (often pushing it deeper into the term).
In sequent calculi `CUT` rules show where we need to compute.

Assume that we have CUT rules for
intuitionistic and linear contexts, `Int-CUT` and `Lin-CUT` respectively.
If there is `Lin-CUT` with □R and □L premises:

```
     𝓓                          𝓔
------------              ----------------
 Γ ; ∙ ⊢ A                 Γ , A  ; Δ ⊢ C
------------ □R           ---------------- □L
 Γ ; ∙ ⊢ □A                Γ ; Δ , □A ⊢ C
------------------------------------------------------ Lin-CUT
 Γ ; Δ ⊢ let ba = box a in case ba of box a' -> c : C
```

We can admit Cut, reduce the proof, cutting "smaller" things.

```
    𝓓                    𝓔
-----------      ---------------
 Γ ; ∙ ⊢ A        Γ , A ; Δ ⊢ C
----------------------------------- Int-CUT
       Γ ; Δ ⊢ let a' = a in c : C
```

Or more succinctly:

```haskell
let ba = box a in case ba of box a' -> c
  ===>
let a' = a in c
```

**Local completeness** We can expand

```
--------------------- Lin-ID
 Γ ; x : □A ⊢ x : □A
```

into

```
---------------- Int-ID
 Γ , A ; ∙ ⊢  A
---------------- □R
 Γ , A ; ∙ ⊢ □A
-------------------------------------------- □L
 Γ ; x : □A ⊢ case x of box y -> box y : □A
```

Or more succinctly:

```haskell
x
  ===>
case x of box y -> box y
```

Dual intuitionistic linear logic
--------------------------------

Now if you take a look at the
[Dual Intuitionistic Linear Logic](http://www.lfcs.inf.ed.ac.uk/reports/96/ECS-LFCS-96-347/) (1994)
report by Andrew Barber, you'll notice
that the DILL system is exactly what is described above.

There are two axiom rules, two cut rules, ... The only differences is that
necessity is written as !A, not □A.

Of course it's not required to have two separate contexts, but *programming* in
DILL is a lot more convenient, than in ILL with only a linear context.

Box is comonad and comonoid
---------------------------

As a small digression, having the rules for □, we can show

```
∙ ; □A ⊢ A         -- extract
∙ ; □A ⊢ □□A       -- duplicate
∙ ; □A ⊢ □A ⊗ □A   -- split
∙ ; □A ⊢ 1         -- delete
```

where for the last two we need to modify rules for ⊗ and 1 from [my last post](2019-06-26-linear-church-encodings.html),
but these modifications, as the proofs itself are quite straight-forward.

Connecting the dots
-------------------

In this post we saw how simple construction of validity applied to
linear logic (ILL) gives DILL. Pfenning and Davies already mention
that their □ corresponds to linear ! (pronounced "of course"[^wikipedia]).

[^wikipedia]: [Wikipedia: Linear Logic: Syntax](https://en.wikipedia.org/wiki/Linear_logic#Syntax)

A (hopefully) interesting exercise is to work through possibility modality, ⋄.
Will it give linear ? (pronounced "why not"). I have no idea.

On the other hand, I think that "!A means that A doesn't require any resources" is a nice
intuition. If `A = 5€`, it would be nice to have `!A` indeed!

There are recent developments in linear logic

- Conor McBride. [I got plenty o' nutting'](https://doi.org/10.1007/978-3-319-30936-1_12) 2016
- Robert Atkey. [The syntax and semantics of quantitative type theory.](https://bentnib.org/quantitative-type-theory.html) 2018
- Robert Atkey and James Wood [Linear metatheory via linear algebra](http://www.ii.uib.no/~bezem/abstracts/TYPES_2019_paper_42.pdf) 2019
- Orchard, Liepelt, Eades [Quantitative Program Reasoning with Graded Modal Types](https://kar.kent.ac.uk/74450/) (Granule) 2019

which instead of having two contexts, use a single
context but have an annotation on each assumption.
For example, look at the *GrMini* from Granule paper, and syntax in Atkey & Wood extended abstract:
they are quite the same, but differnt from what we just seen.
The difference is well illustrated by ⊗R-rule: (our) two context approach rule is

```haskell
 Γ ; Δ₁ ⊢ a : A     Γ ; Δ₂ ⊢ b : B
----------------------------------- ⊗R
 Γ ; Δ₁ , Δ₂ ⊢ Pair a b : A ⊗ B
```

where we split the (linear) context into "structural" parts.
Instead we could use a single context, and parametrise it by *usages*,
and split the usages, not the context:

```haskell
 Γ{R} ⊢ a : A     Γ{S} ⊢ b : B
------------------------------- ⊗R
 Γ{R+S} ⊢ Pair a b : A ⊗ B
```

Generalising what you use as annotations, you can get
Pfenning and Davies’ modal type theory
or Barber's DILL, as Atkey and Wood show in their abstract.

In hindsight, using annotations/multiplicities/... is an "obvious" discovery.
If you try to formalise linear logic fragment in Coq or Agda, *removing* things
from the context needs a lot more plumbing than just *marking as used*.

This brief post connects dots from few quite old papers with recent developments.
In my opinion, the old stuff is relatively simple, and
hopefully helps to understand the new stuff.
