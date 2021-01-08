---
title: Dependent Linear types in QTT
author: Oleg Grenrus
tags: linear
---

This post is my musings about a type system proposed by
Conor McBride in [I Got Plenty o' Nuttin'](https://personal.cis.strath.ac.uk/conor.mcbride/PlentyO-CR.pdf)
and refined by Robert Atkey in [Syntax and Semantics of Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.pdf): *Quantitative Type Theory* or QTT for short.
[Idris 2](https://github.com/idris-lang/Idris2) is based on QTT,
so at the end there is some code too.

Non-linear
----------

But let me start with recalling *Simply Typed Lambda Calculus with Products and Coproducts*, $\lambda^{\to\times+}$ (that is a mouthful!).
As the name already says there are three binary connectives,
functions, products and coproducts (or sums).

$$
\to \qquad \times \qquad +
$$

But in Martin Löf Type Theory (not quantiative) we have only two:
pi and sigma types.

$$
\prod \qquad \sum
$$

We can recover ordinary functions, $\to$ and ordinary pairs in a straigh-forward  way:

$$
A \to B \coloneqq \prod_{\_:A} B
\qquad
A \times B \coloneqq \sum_{\_:A} B
$$

However, to get coproducts we need something extra.
One general way is to add *finite sets*.
We get *falsehood* (empty finite set),
*truth* (singleton finite set), booleans (binary finite sets),
and so on. But these three are enough.

With booleans we can define

$$
A + B \coloneqq \sum_{t:\mathbb{B}\mathsf{ool}}
\mathbf{if}\; t \;\mathbf{then}\; A \;\mathbf{else}\; B
$$

Which is very reasonable. That is how we represent sum types
on physical machines: a tag and a payload which type depends on a tag.

A natural question is what we get if switch $\sum$ to $\prod$. Unfortuntely, nothing new:

$$
\prod_{t:\mathbb{B}\mathsf{ool}}
\mathbf{if}\; t \;\mathbf{then}\; A \;\mathbf{else}\; B
\cong
A \times B
\coloneqq
\sum_{\_:A} B
$$

We get another way to describe pairs.

To summarise:

$$
\begin{tabular}{l|cc}
$\mathbf{op}$ & $\prod$ & $\sum$ \\
\hline
$\mathbf{op}_{\_:A} B$                           & $\to$     & $\times$ \\
$\mathbf{op}_{t:\mathbb{B}\mathsf{ool}} \ldots$  & $\times$  & $+$
\end{tabular}
$$

Let us next try to see how this works out with linear types.


Linear
------

In the intuitionistic linear calculus (ILC, also logic, ILL)
we have four binary connectives:
linear functions,
times,
plus and with.

$$
\multimap \qquad \otimes \qquad \oplus \qquad \binampersand
$$

Often an unary bang

$$
!
$$

is also added to allow writing non-linear programs as well.

Making linear *dependent* theory is hard,
and Quantitative Type Theory is a promising attempt. It seems to work.

And it is not complicated. In fact, as far as we concerned, it still has just two connectives

$$
\prod \qquad \sum
$$

but slightly modified to include *multiplicity* (denoted by $\pi, \rho, \ldots$):

$$
\prod_{x\overset{\pi}{:}A} B
\qquad
\sum_{x\overset{\pi}{:}A} B
$$

The multiplicity can be $1$, i.e. linear single use.
Or $\omega$, the unrestricted use, but also $0$, which is irrelevant usage.
I find this quite elegant.

The rules are then set up so we don't run into problems with "types using our linear variables",
as they are checked in irrelevant context.

As in the non-linear setting, we can recover two "simple" connectives immediately:

$$
A \multimap B \coloneqq \prod_{\_ \overset{1}{:} A} B
\qquad
A \otimes B \coloneqq \sum_{\_ \overset{1}{:} A} B
$$

Other multiplicities allow us to create some "new" connectives

$$
A \to B \coloneqq \mathop{!A} \multimap B = \prod_{\_ \overset{\omega}{:} A} B
\qquad
\forall (x:A). B = \prod_{\_ \overset{0}{:} A} B
$$

where $\forall$ is the irrelevant quantification.

You should guess that next we will recover $\oplus$ and $\binampersand$
using booleans.
However booleans in linear calculus are conceptually hard,
when you start to think about resource interpretation of linear calculus.
However, *booleans are not resources*, they are information.
About introduction Atkey says

<blockquote>
... no resources are required to construct the constants true and false
</blockquote>

but the elimination is a bit more involved.
The important bit is, however, that we have if-then-else which behaves
reasonably.

Then

$$
A \oplus B \coloneqq \sum_{t \mathop{\overset{1}{:}} \mathbb{B}\mathsf{ool}}
\mathbf{if}\; t \;\mathbf{then}\; A \;\mathbf{else}\; B
$$

and

$$
A \mathop\binampersand B \coloneqq \prod_{t \mathop{\overset{1}{:}} \mathbb{B}\mathsf{ool}}
\mathbf{if}\; t \;\mathbf{then}\; A \;\mathbf{else}\; B
$$

That $\oplus$ behaves as we want.
When we match on $\sum$ we learn the tag and payload.
As tag has multiplicity 1, we can once match on it to learn the type
of the payload.
(Note: I should really write the typing rules, and derivations, but I'm quite confident it works this way. Highly likely I'm wrong :)

The with-connective, $\binampersand$, is mind-juggling.
It's a product (in fact, the product in CT-sense). We can extract parts,
but if we have to decide which, cannot do both.

$$
\begin{aligned}
\mathit{fst} &: A \mathop\binampersand B \multimap A \\
\mathit{fst} &=
\lambda w : \ldots \mapsto w\,\mathsf{true}
\end{aligned}
$$

The value of type $A \mathop\binampersand B$ is a function, and we can only call it once,
so we cannot write a value of type $A \mathop\binampersand B \multimap A \otimes B$,
nor the inverse.

So the $\otimes$ and the $\binampersand$ are both product like, but different.

We redraw the table from the previous section.
there are no more unelegant duplication:

$$
\begin{tabular}{l|cc}
$\mathbf{op}$ & $\prod$ & $\sum$ \\
\hline
$\mathbf{op}_{\_ \mathop{\overset{1}:} A} B$                            & $\multimap$      & $\otimes$ \\
$\mathbf{op}_{t  \mathop{\overset{1}:} \mathbb{B}\mathsf{ool}} \ldots$  & $\binampersand$  & $\oplus$
\end{tabular}
$$

Diag
----

It's often said that you cannot write 

```
diag :: a -> (a, a)
```

in linear calculus.

This is true if we assume that tuples are $\otimes$ tensors.
That is natural assumption as $\otimes$ is what makes curry with $\multimap$ arrows.

However, the product is $\binampersand$.
I argue that "the correct" type of diag ($\Delta$) is

```
diag : a -> a & a
diag x = x :&: x
```

And in fact, the adjointness is the same as in CT-of-STLC,
(Idris2 agrees with me):

$$
\oplus \dashv \Delta \dashv \binampersand
$$

If we could just normalise the notation, then we'd use

$$
\begin{tabular}{c|c}
linear logic & category theory \\
\hline
$\otimes$ & $\otimes$ \\
$\oplus$ & $+$ \\
$\mathop{\binampersand}$ & $\times$
\end{tabular}
$$

But that would be sooo... confusing.

There is plenty of room for more confusion, it gets better.

Units
-----

Products and coproducts usually have units,
so is the case in the linear calculus too.

$$
\begin{tabular}{c|c|c|c}
linear logic connective & unit & category theory & unit \\
\hline
$\otimes$ & $1$ & $\otimes$ & $I$ \\
$\oplus$ & $0$ & $+$ & $0$ \\
$\mathop{\binampersand}$ & $\top$ & $\times$ & $1$
\end{tabular}
$$

Spot a source of possible confusion.

We know, that because $\binampersand$ is the product,
its unit is the terminal object, $1$.
And now we have to be careful.

**Definition:** T is terminal object if for every object X in category C
there exist unique morphism $X \to T$.

Indeed the $\top$ in linear logic is such object.
It acts like a dumpster.
If we don't like some (resource) thing, we can map it to $\top$.
If we already have $\top$ (there is no way to get rid of it),
we can tensor it with something else we don't like and map the resulting "pair" to another $\top$.
Bottomless trash can!

In category theory we avoid speaking about objects directly (it is point-free extreme).
If we need to, we speak about object $A$, we rather talk about $1 \to A$ morphism (constant function).
This works because, e.g. in Sets category:

$$
X \cong \mathrm{Hom}(1, X)
$$

There the use of $1$ comes from it being the unit of $\times$
used to internalize arrows, i.e. define $A \to B$ objects (and binary functions, currying, etc).

In linear logic, the "friend" of $\multimap$ is, however, $\otimes$,
and its unit is not terminal object.

$$
(A \otimes B) \multimap C \cong A \multimap (B \multimap C)
$$

So we rather have

$$
X \cong \mathrm{Hom}(I_\otimes, X)
$$

which is again confusing, as you can confuse $I$ for initial object, $0$, which it isn't.
To help avoid that I used the subscript.

The takeaway is that $\top$ and $1$ in linear logic are different
objects, and you have to be very careful so ordinary lambda calculus (or e.g. Haskell)
intuition doesn't confuse you.

I wish there were a category where linear stuff is separate. In Sets $\times = \otimes = \mathop{\binampersand}$.
Vector spaces are close, but they have own source of confusion
(there $\times = + = \oplus$, direct sum, which is a product, and coproduct).

Idris 2
-------

All above is nicely encodable in [Idris 2](https://github.com/idris-lang/Idris2).
If you want to play with linear logic concepts,
I'd say that Idris2 is the best playground at the moment.

```haskell
module QTT

-----------------------------------------------------------------------
-- Pi and Sigma
-----------------------------------------------------------------------

Pi1 : (a : Type) -> (a -> Type) -> Type
Pi1 a b = (1 x : a) -> b x

data Sigma1 : (a : Type) -> (a -> Type) -> Type where
    Sig1 : (1 x : a) -> (1 y : b x) -> Sigma1 a b

-----------------------------------------------------------------------
-- Lollipop
-----------------------------------------------------------------------

Lollipop : Type -> Type -> Type
Lollipop a b = Pi1 a \_ => b

-- handy alias
(-@) : Type -> Type -> Type
(-@) = Lollipop
infixr 0 -@

-- for constructor, just write \x => expr

-- Lollipop elimination, $
lollipopElim : Lollipop a b -@ a -@ b
lollipopElim f x = f x

-----------------------------------------------------------------------
-- Times
-----------------------------------------------------------------------

Times : Type -> Type -> Type
Times a b = Sigma1 a \_ => b

-- Times introduction
times : a -@ b -@ Times a b
times x y = Sig1 x y

-- Times elimination
timesElim : Times a b -@ (a -@ b -@ c) -@ c
timesElim (Sig1 x y) k = k x y

-----------------------------------------------------------------------
-- With
-----------------------------------------------------------------------

With : Type -> Type -> Type
With a b = Pi1 Bool \t => if t then a else b

-- With elimination 1
fst : With a b -@ a
fst w = w True

-- With elimination 2
snd : With a b -@ b
snd w = w False

-- There isn't really a way to write a function for with introduction,
-- let me rather write diag.

diag : a -@ With a a
diag x True  = x
diag x False = x

-- Also note, that even if With would be a built-in, it should
-- be non-strict (and a function is).
-- We may use the same resource in both halfs differently,
-- and the resource cannot be used until user have selected the half.

-----------------------------------------------------------------------
-- Plus
-----------------------------------------------------------------------

Plus : Type -> Type -> Type
Plus a b = Sigma1 Bool \t => if t then a else b

-- Plus introduction 1
inl : a -@ Plus a b
inl x = Sig1 True x

-- Plus introduction 2
inr : b -@ Plus a b
inr y = Sig1 False y

-- Plus elimination, either... with a with twist
-- Give me two functions, I'll use one of them, not both.
plusElim : With (a -@ c) (b -@ c) -@ Plus a b -@ c
plusElim f (Sig1 True  x) = f True  x
plusElim f (Sig1 False y) = f False y

-----------------------------------------------------------------------
-- Extras
-----------------------------------------------------------------------

-- plusElim is reversible.
-- Plus -| Diag

plusElimRev : (Plus a b -@ c) -@ With (a -@ c) (b -@ c)
plusElimRev f True  = \x => f (inl x)
plusElimRev f False = \y => f (inr y)

-- Diag -| With
adjunctFwd : (c -@ With a b) -@ With (c -@ a) (c -@ b)
adjunctFwd f True  = \z => f z True
adjunctFwd f False = \z => f z False

adjunctBwd : With (c -@ a) (c -@ b) -@ (c -@ With a b)
adjunctBwd f c True  = f True c
adjunctBwd f c False = f False c

-----------------------------------------------------------------------
-- Hard exercise
-----------------------------------------------------------------------

-- What would be a good way to imlement Top, i.e unit of With.
--
-- fwd : a -@ With Top a
-- bwd : With Top a -@ a
--
-- I have ideas, I'm not sure I like them.
```
