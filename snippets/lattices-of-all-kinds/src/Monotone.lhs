\input{../../preamble.tex}
%include polycode.fmt
%
% %s/{lattice-\(.*\)}/{..\/images\/lattice-\1.png}/
% %s/{cat-\(.*\)}/{..\/images\/cat-\1.png}/
%
\renewcommand{\Conid}[1]{\mathtt{#1}}
\renewcommand{\Varid}[1]{\mathtt{#1}}
\newcommand{\Haskell}{\textsc{Haskell}}
%subst keyword a = "\text{\texttt{\textbf{" a "}}}"
%format . = "\mathbin\mathtt{.}"
%format && = "\mathbin\mathtt{\&\&}"
%format $ = "\mathbin\mathtt{\$}"
%format /\ = "\land"
%format \/ = "\lor"
%format `x` = "\times"

\title{All kinds of lattices}
\author{Oleg Grenrus}
%\date{2019-07-22}
\begin{document}
\maketitle
\iffalse
\begin{code}
{- cabal:
build-depends: base, containers, lattices, topograph, process
-}
\end{code}
\fi

%if 0
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monotone (
    Monotone (..),
    isMonotone,
    Domain (..),
    Display (..),
    main
    ) where

import           Algebra.Lattice             (BoundedMeetSemiLattice (..), BoundedJoinSemiLattice (..), Lattice (..))
import           Algebra.Lattice.M2          (M2 (..))
import           Algebra.Lattice.ZeroHalfOne (ZeroHalfOne (..))
import           Algebra.PartialOrd          (PartialOrd (..))
import           Control.Category            (Category)
import           Control.Monad               (void)
import           Data.Map                    (Map)
import           Data.Maybe                  (mapMaybe)
import           Data.Proxy                  (Proxy (..))
import           Data.Set                    (Set)
import           System.Process              (readProcess)
import           Topograph                   (adjacencyList, reduction, runG)

import qualified Control.Category            as C
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
\end{code}
%endif

\emph{Partial ordered set}, or poset for short is well used example in
category theory books.
Yet, posets are not too familiar for an average
CT-curious Haskeller, yet they are easy to visualise!
Let's do exactly that, and mention elementery bits of category theory.
In particular, I'll scan over the six first chapters of
\emph{Category Theory} by Steve Awodey~\cite{Awodey:2010},
repeating related definitions and poset examples.\footnote{%
If you want to learn category theory, getting a book is a small investment.
Your public or university library probably have a copy.
}

\section{Categories}
\label{sec:categories}

\begin{definition}[Awodey 1.1]
A \emph{category} consist of the following data
\begin{itemize}
\item Objects: $A, B, C, \ldots$
\item Arrows: $f, g, h, \ldots$

\item For each arrow $f$, there are given objects
\begin{equation*}
\mathrm{dom}(f), \qquad \mathrm{cod}(f)
\end{equation*}
called the \emph{domain} and \emph{codomain} of $f$. We write
\begin{equation*}
f : A \to B
\end{equation*}
to indicate that $A = \mathrm{dom}(f)$ and $B = \mathrm{cod}(f)$.

\item Given arrows $f : A \to B$ and $g : B \to C$, that is, with
\begin{equation*}
\mathrm{cod}(f) = \mathrm{dom}(g)
\end{equation*}
there is given an arrow
\begin{equation*}
g \circ f : A \to C
\end{equation*}
called the \emph{composite} of $f$ and $g$.

\item For each object $A$, there is given an arrow
\begin{equation*}
1_A : A \to A
\end{equation*}
called the \emph{identity arrow} of $A$.

\item Associativity:
\begin{equation*}
h \circ (g \circ f) = (h \circ g) \circ f
\end{equation*}
for all $f : A \to B$, $g : B \to C$, $h : C \to D$.

\item Unit:
\begin{equation*}
f \circ 1_A = f = 1_B \circ f
\end{equation*}
for all $f : A \to B$.
\end{itemize}
\end{definition}

We'll see how |Category| type-class is related later, in \cref{sec:product}.


A partially ordered set or \emph{poset} is a set $A$ equipped with a binary
relation $a \le_A b$ such that the following conditions hold for all $a,b,c \in A$:
\begin{itemize}
\item reflexivity: $a \le_A a$,
\item transitivity: if $a \le_A b$ and $b \le_A c$, then $a \le_A c$,
\item antisymmetry: if $a \le_A b$ and $b \le_A a$, then $a = b$.
\end{itemize}

In \Haskell{}, we can define a type-class for \emph{decidable}
partial orders. Decidable means, that we can \emph{decide} whether
the relation holds:
\begin{spec}
class Eq a => PartialOrd a where
    leq :: a -> a -> Bool
\end{spec}
We'll see examples of posets and instances of |PartialOrd| in a moment.

Any partial order $P$ can be regarded as a category by taking the objects
to be the elements of $P$ and taking an unique arrow,
\begin{equation*}
a \to b \quad\text{if and only if}\quad a \le b.
\end{equation*}
The reflexive and transitive conditions on $\le$ ensure that this is indeed
a category.

\emph{Finite} poset-categories are easy to visualise.
We'll draw a \emph{graph} where vertices are objects,
and edges are arrows between objects. To make pictures
clearer, we can omit the implied composite arrows.
We'll consider a subcategory of partial orders, namely \emph{lattices},
i.e. partial orders with all meets and joins.
We'll explain what that means later.

\section{Domain-class}

Before continuing to examples of poset-categories, we'll need
to code up how to display them.
We'll need a helper typeclass, |Domain|.
The name will become clear later.
All partial orders we'll work with, will be instances of the |Domain| typeclass.

We also require that types are |Ord|. The |Ord| instance doesn't need to be
consistent with |PartialOrd|\footnote{%
We use \href{https://hackage.haskell.org/package/lattices}{\texttt{lattices}} package for lattice related functionality,
and \href{https://hackage.haskell.org/package/topograph}{\texttt{topograph}} to operate on directed acyclic graphs.
} (and often cannot be). We'll use it to put
elements into a |Map| and |Set|, i.e. it's an exposed "implementation detail".
All finite sets can be totally ordered, so it's not a problem to require.

As we work with finite sets, we can have a list of all elements, |elements|.
We also require that the list is in a \href{https://en.wikipedia.org/wiki/Topological_sorting}{topological ordering},
which is (hopefully) a small optimisation.

|ltPairs| method is used for visualisation. It's a list of
pairs $(x, y)$ such that $x \le y$, but $x \neq y$,
yet not all such pairs: transitive pairs can be removed.

\begin{code}
data V2 a = V2 !a !a

class (PartialOrd a, Ord a) => Domain a where
    elements  :: [a]
    ltPairs   :: [V2 a]
\end{code}

|ltPairs| has a default implementation.
First we construct an adjacency matrix |am|, using |elements|. Here we
use the fact that |elements| is topologically ordered, so we do a bit less
work. Then we construct a DAG, compute its reduction, and return
an adjacency list. Finally that list is flattened into a list of |V2| pairs.
|V2 a| is a homogenous pair of |a|.

\begin{code}
    ltPairs
        =  either (const []) (concatMap mk)
        $  runG am $ \g -> adjacencyList (reduction g)
      where
        am :: Map a (Set a)
        am = Map.fromList $ go elements where
            go []      = []
            go (x:xs)  = (x, Set.fromList [ x' | x' <- xs, leq x x' ]) : go xs

        mk :: (a, [a]) -> [V2 a]
        mk (x, xs) = map (V2 x) xs
\end{code}

The actual display code is in \cref{sec:display}. There we generate
a \emph{Graphviz} graph, and render it to \emph{PNG}-image.
The |Display| type class specifies how to render elements,
we won't abuse |Show|.

\begin{code}
class Display a where
    display :: a -> String
\end{code}

\section{Simple lattices}

In this section, we'll visualise few simple lattices:
two-, three- and four-element lattices.

\subsection{Bool}

One of the simplest lattices, is the\footnote{%
It's actually \emph{the} two element lattice.
For example a set $\{ a, b \}$ with a discrete
partial order $a \le a, b \le b$ is not a lattice,
as there are no $a \land b$ element, such that
$a \land b \le a$  and $a \land b \le a$.
} two element lattice. It's also known as the category $\mathbf{2}$.
It's handy to use |Bool| as it is two-element set. |Domain Bool| instance
is simple to define. We sort $[ minBound .. maxBound ]$ list,
in case if |Enum| and |Bounded| instances don't agree with |PartialOrd|.

\begin{code}
instance Domain Bool where
    elements = insertionSort leq [ minBound .. maxBound ]
\end{code}

The |Display| instance is brevety, |"T"| and |"F"| for |True| and |False|.

\begin{code}
instance Display Bool where
    display False  = "F"
    display True   = "T"
\end{code}

\begin{figure}[H]
\begin{center}
\includegraphics[scale=0.4]{lattice-bool}
\end{center}
\caption{|Bool| lattice}
\label{fig:bool}
\end{figure}

With these two instances in place, we can visualise |Bool| lattice,
on \cref{fig:bool}.

\begin{code}
outputBool  ::  IO ()
outputBool  =   outputGraph (Proxy :: Proxy Bool) "lattice-bool.png"
\end{code}

\subsection{Zero-Half-One}
\label{sec:zho}

Next lattice is so called \emph{zero-half-one} lattice.
It's the three element ($0, \frac{1}{2}, 1$) totally ordered lattice.

\begin{equation*}
0 \le \frac{1}{2} \le 1
\end{equation*}

It's the category $\mathbf{3}$.
The |Domain| and |Display| instances are defined similarly as in the |Bool| case.

\begin{code}
type ZHO = ZeroHalfOne

instance Domain ZeroHalfOne where
    elements = insertionSort leq [ minBound .. maxBound ]

instance Display ZeroHalfOne where
    display Zero  = "0"
    display Half  = "H"
    display One   = "1"
\end{code}

\begin{figure}[H]
\begin{center}
\includegraphics[scale=0.4]{lattice-zho}
\qquad\qquad
\includegraphics[scale=0.4]{lattice-zho2}
\end{center}
\caption{|ZHO| lattice, reduced and with all arrows explicitly drawn}
\label{fig:zho}
\end{figure}

The |ZHO| lattice is on \cref{fig:zho}. Reduced version is a prettier,
as redundant information is dropped.

\begin{code}
outputZHO  ::  IO ()
outputZHO  =   outputGraph (Proxy :: Proxy ZHO) "lattice-zho.png"
\end{code}

\subsection{M2}

$M_2$ is one more "primitive" lattice. It is a four element lattice, $0, a, b, 1$,
and the first one which partial order isn't total.
Another four-element lattice is the totally ordered one.

\begin{equation*}
0 \le a \le 1 \qquad 0 \le b \le 1 \qquad a, b \ \text{are not related}
\end{equation*}

It's visualised on \cref{fig:m2}, the graph has a nice diamond shape.

\begin{figure}[H]
\begin{center}
\includegraphics[scale=0.4]{lattice-m2}
\end{center}
\caption{$M_2$ lattice}
\label{fig:m2}
\end{figure}

\begin{code}
instance Domain M2 where
    elements = insertionSort leq [ minBound .. maxBound ]

instance Display M2 where
    display M2i  = "1"
    display M2a  = "a"
    display M2b  = "b"
    display M2o  = "0"

outputM2  ::  IO ()
outputM2  =   outputGraph (Proxy :: Proxy M2) "lattice-m2.png"
\end{code}

\section{Functor}

Before proceeding, we'll answer a question: is there a category
with posets as objects? Yes, it's called $\mathbf{Pos}$!
What are the arrows in this category?
An arrow from a poset $A$ to a poset $B$ is a function
\begin{equation*}
m : A \to B
\end{equation*}
that is \emph{monotone}, in the sense that, for all $a, a' \in A$,
\begin{equation*}
a \le_A a' \qquad\text{implies}\qquad m(a) \le_B m(a').
\end{equation*}
We need to know that $1_A : A \to A$ is monotone, and also
that if $f : A \to B$ and $g : B \to C$ are monotone, then $g \circ f : A \to C$
is monotone. That holds, check!

Recall, posets are categories, so monotone functions are "mappings" between
categories. A "homomorphism\footnote{morphism preserving the structure} of categories"
is called a functor.

\begin{definition}[Awodey 1.2]
A \emph{functor}
\begin{equation*}
F : \mathbf{C} \to \mathbf{D}
\end{equation*}
between categories $\mathbf{C}$ and $\mathbf{D}$ is a mapping of objects
to objects and arrows to arrows, in such a way that
\begin{itemize}
\item $F (f : A \to B) = F(f) : F(A) \to F(B)$,
\item $F(1_A) = 1_{F(A)}$,
\item $F(g \circ f) = F(g) \circ F(f)$.
\end{itemize}
That is, $F$ preserves domains and codomains, identity arrows,
and composition. A functor $F : \mathbf{C} \to \mathbf{D}$ thus gives
a sort of "picture" -- perhaps distorted -- of $\mathbf{C}$ in $\mathbf{D}$.
\end{definition}

The \Haskell\ version looks quite different:
\begin{spec}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
\end{spec}
There is a mismatch of a notation of category theory, and what
can be written as code. In CT notation $F$ acts both on objects and arrows.
In \Haskell\ |f| acts on objects, and |fmap f| acts on arrows.
Substituting above, |id| and |.| into definition of functor, will
also give familiar laws
\begin{spec}
fmap id       = id
fmap (g . f)  = fmap g . fmap f
\end{spec}

However, \Haskell\ |Functor|-class is only for functors from a pseudo-category $\mathbf{Hask}$
to itself, where |f|, a mapping from types to types is a type-constructor,
not arbitrary type family.
|Functor| is a very special case of category theoretical variant.

With small posets, like |Bool| and |M2| we can visualise a
monotone function, a functor. Let's consider a function
\begin{spec}
f :: Bool -> M2
f True   = M2i
f False  = M2o
\end{spec}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.5]{cat-bool-m2}
\end{center}
\caption{Graph of |f :: Bool -> M2|}
\label{fig:f-bool-to-m2}
\end{figure}

The graph of |f| is on \cref{fig:f-bool-to-m2}. Dotted and dashed lines are arrows
in |Bool| and |M2| respectively.
We can see on figure, that |f| indeed gives a picture of |Bool| in |M2|.

In \Haskell\ we have only written
a mapping of objects, |True| and |False|. The mapping of arrows is something
we need to check, to be able to claim that |f| is a functor, and therefore
a monotone function. The other way around, there are mappings from 
|Bool| to |M2| which aren't monotone, and aren't functors.

In this section we went backwards. More principal approach would been to first
consider functors between poset categories. The monotonicity requirement is
implied by first functor requirement. This is a power of category
theory. When you look for something, category theory tells you which
properties it should have. Once you find something which satisfies
the requirements, you know that it's the right one (up to an isomorphism).

\section{Product}
\label{sec:product}

Next, we are going to see the categorical definition of a product of two
objects in a category.

\begin{definition}[Awodey 2.15]
In any category $\mathbf{C}$, a \emph{product diagram} for the objects
$A$ and $B$ consists of an object $P$ and arrows (\emph{projections})
\begin{center}\includegraphics[scale=0.5]{cat-prod-a}\end{center}
satisfying the following universal mapping property:

Given any diagram of the form
\begin{center}\includegraphics[scale=0.5]{cat-prod-b}\end{center}
there exists a unique $u : X \to P$, making the diagram
\begin{center}\includegraphics[scale=0.5]{cat-prod-c}\end{center}
commute, that is, such that $x_1 = p_1 u$ and $x_2 = p_2 u$.
\end{definition}

\begin{example}[Awodey 2.5, 4th example]
Let $P$ be a poset and consider a product of elements $p, q \in P$. We must
have projections
\begin{equation*}
\begin{aligned}
p \times q &\le p \\
p \times q &\le q
\end{aligned}
\end{equation*}
and if for any element $x$,
\begin{equation*}
x\le p, \qquad\text{and}\qquad x \le q
\end{equation*}
then we need
\begin{equation*}
x \le p \times q.
\end{equation*}
This operation $p \times q$ is usually called the \emph{greatest lower bound} or \emph{meet}:
$p \times q = p \land q$.
A poset with all finite meets is a \emph{meet-semilattice}.
\end{example}

\begin{example}[Awodey 2.4, 2nd example]
Products of "structured sets" like monoids or groups or lattices can be often constructed
as products of the underlying sets with \emph{componentwise} operations:
If $P$ and $Q$ are meet-semilattices, for instance, $P \times Q$ can constructed
by taking the underlying set of $P \times Q$ to be the set
$\{ \langle p, q \rangle \mid p \in P, q \in Q \}$.
It can be partially ordered by
\begin{equation*}
\langle p, q \rangle \le \langle p', q' \rangle
\qquad\text{if and only if}\qquad
p \le p' \quad\text{and}\quad q \le q'
\end{equation*}
And we can define meet as
\begin{equation*}
\langle p, q \rangle \land \langle p', q' \rangle =
\langle p \land p', q \land q' \rangle
\end{equation*}
We must check that
\begin{equation*}
\begin{aligned}
\langle p, q \rangle \land \langle p', q' \rangle = \langle p \land p', q \land q' \rangle &\le \langle p, q \rangle \\
\langle p, q \rangle \land \langle p', q' \rangle = \langle p \land p', q \land q' \rangle &\le \langle p', q' \rangle
\end{aligned}
\end{equation*}
and that projection functions
$p_1 : P \times Q \to P$
and
$p_2 : P \times Q \to Q$ are monotone, as 
the pairing $\langle f, g \rangle : X \to P \times Q$,
if $f : X \to P$ and $g : X \to Q$.
A lot to check, but it all holds.

The category $\mathbf{Pos}$ of posets has products,
as well as category $\mathbf{Latt}$ of lattices\footnote{%
In this article we examine the subcategories with \emph{finite} underlying sets.
More correctly, we should speak about $\mathbf{FinPos}$ and $\mathbf{FinLatt}$.
}.
\end{example}

At this point you should try to make it clear to yourself:
$\mathbf{Latt}$ is a category of categories, and it has products.
In other words products of categories with products.
Abstract construction of abstract constructions.

The product in $\mathbf{Pos}$ is easy to encode in \Haskell. Pairs are products,
so it's enough to write instances for |(a,b)|.

\begin{spec}
instance (PartialOrd a, PartialOrd b) => PartialOrd (a, b) where
    leq (a, a') (b, b') = leq a a' && leq b b'
\end{spec}
And that's all. The underlying theory says that this is a correct instance.

Let's define |Display| and |Domain| instances, to visualise few lattice products.
\begin{code}
instance (Display a, Display b) => Display (a, b) where
    display (a, b) = "(" ++ display a ++ "," ++ display b ++ ")"

instance (Domain a, Domain b) => Domain (a, b) where
    elements
        =  insertionSort leq
        $  [ (a, b) | a <- elements, b <- elements ]
\end{code}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-boolbool}
\end{center}
\caption{|Bool `x` Bool| lattice}
\label{fig:boolbool}
\end{figure}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-boolzho}
\end{center}
\caption{|Bool `x` ZHO| lattice}
\label{fig:boolzho}
\end{figure}

If we then visualise the |Bool `x` Bool| lattice, \cref{fig:boolbool},
we'll see that it's $M_2$. So $M_2$ isn't "primitive",
it's the product of |Bool| and |Bool| lattices! The |Bool `x` ZHO|
lattices on \cref{fig:boolzho} is pretty and new, we haven't seen such lattice
yet.
\begin{code}
outputBoolBool  ::  IO ()
outputBoolBool  =   outputGraph (Proxy :: Proxy (Bool, Bool)) "lattice-boolbool.png"

outputBoolZHO  ::  IO ()
outputBoolZHO  =   outputGraph (Proxy :: Proxy (Bool, ZHO)) "lattice-boolzho.png"
\end{code}

\section{Duals: Coproduct}

We can form a ``dual statement'' in the elementary language of category theory
by making the following replacements:
\begin{center}
\begin{tabular}{rcl}
$f \circ g$ & for & $g \circ f$ \\
$\mathrm{cod}$ & for & $\mathrm{dom}$ \\
$\mathrm{dom}$ & for & $\mathrm{cod}$.
\end{tabular}
\end{center}
i.e. ``reversing the arrows''.

Let us consider the example of products and see what the dual notion must be.
\begin{definition}[Awodey 3.3]
A diagram $A \xrightarrow{\quad q_1 \quad} Q \xleftarrow{\quad q_2 \quad} B$
is a ``dual-product'' of $A$ and $B$ if for any $Z$ and
$A \xrightarrow{\quad z_1 \quad} Z \xleftarrow{\quad z_2 \quad} B$
there is an unique $u : Q \to Z$ with $u \circ q_1 = z_1$ and $u \circ q_2 = z_2$
all as indicated in
\begin{center}\includegraphics[scale=0.5]{cat-coprod-c}\end{center}
Actually, these are called \emph{coproducts}; the convention is to use the prefix ``co-'' to
indicate the dual notion.
\end{definition}

\begin{example}[Awodey 3.7]
In a fixed poset $P$, what is a coproduct of two elemnents $p, q \in P$?
We have
\begin{equation*}
p \le p + q\qquad\text{and}\qquad q \le p + q
\end{equation*}
and if
\begin{equation*}
p \le z\qquad\text{and}\qquad q \le z
\end{equation*}
then
\begin{equation*}
p + q \le z.
\end{equation*}
So $p + q = p \lor q$ is the \emph{join}, or \emph{least upper bound}, of $p$ and $q$.
A poset with all finite joins is a \emph{join-semilattice}.
A poset with all finite meets and joins is a \emph{lattice}.
\end{example}

Similarly as product, we can try to define a coproduct in $\mathbf{Pos}$.
Given posets $P$ and $Q$, the poset $P + Q$ has tagged elements
$\mathrm{L}\;p$ and $\mathrm{R}\;q$, and is partially order by
\begin{equation*}
\begin{aligned}
\mathrm{L}\;p \le \mathrm{L}\;p' &\qquad\text{if and only if}\qquad p \le p' \\
\mathrm{R}\;q \le \mathrm{R}\;q' &\qquad\text{if and only if}\qquad q \le q'
\end{aligned}
\end{equation*}
The injections are \emph{monotone}, as the pairing $[f,g] : P + Q \to Z$,
if $f : P \to Z$ and $Q : P \to Z$ are monotone.

Let's encode coproducts in \Haskell. Coproduct or \emph{sum} of sets is
|Either|. Instances for |Either|:
\begin{code}
instance (PartialOrd a, PartialOrd b) => PartialOrd (Either a b) where
    leq (Left a)   (Left a')   = leq a a'
    leq (Right b)  (Right b')  = leq b b' 
    leq _          _           = False

instance (Display a, Display b) => Display (Either a b) where
    display (Left a)   =  "L " ++ display a
    display (Right b)  =  "R " ++ display b

instance (Domain a, Domain b) => Domain (Either a b) where
    elements
        =  insertionSort leq
        $  map Left elements ++ map Right elements
\end{code}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-boolObool}
\end{center}
\caption{|Bool + Bool| poset}
\label{fig:boolObool}
\end{figure}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-boolboolObool}
\end{center}
\caption{|Bool `x` (Bool + Bool)| poset}
\label{fig:boolboolObool}
\end{figure}

|Bool + Bool| partial order (\cref{fig:boolObool}) is not a lattice,
there are two disjoint parts. We can conclude that even $\mathbf{Pos}$
has coproducts, $\mathbf{Latt}$ doesn't.
Sad truth, many categories don't have coproducts.

The graph of |Bool `x` (Bool + Bool)|
on \cref{fig:boolboolObool}
nicely show the distributivity of coproducts and products (the execise 8.9.3)
there are two disjoint |Bool `x` Bool| graphs.

\begin{code}
outputBoolOrBool  ::  IO ()
outputBoolOrBool  =
    outputGraph (Proxy :: Proxy (Either Bool Bool)) "lattice-boolObool.png"

outputBoolBoolOrBool  ::  IO ()
outputBoolBoolOrBool  =
    outputGraph (Proxy :: Proxy (Bool, Either Bool Bool)) "lattice-boolboolObool.png"
\end{code}


\section{Expontentials: Monotone functions}

Next we are going to look at one more elementaty universal structure.
This important structure is called an \emph{exponential}, and it can
be thought of as a categorical notion of a ``function space''.

\begin{definition}[Awodey 6.1]
Let the category $\mathbf{C}$ have binary products.
An \emph{exponential} of objects $B$ and $C$ consists of an object
\begin{equation*}
C^B
\end{equation*}
and an arrow (\emph{evaluation})
\begin{equation*}
\epsilon : C^B \times B \to C
\end{equation*}
such that, for any object $A$ and arrow
\begin{equation*}
f : A \times B \to C
\end{equation*}
there is an a unique arrow (\emph{transpose} of $f$)
\begin{equation*}
\tilde{f} : A \to C^B
\end{equation*}
such that
\begin{equation*}
\epsilon \circ (\tilde{f} \times 1_B) = f
\end{equation*}
all as in the diagram
\begin{center}\includegraphics[scale=0.5]{cat-expo}\end{center}
\end{definition}

\begin{example}[Exponential in $\mathbf{Sets}$]
In category $\mathbf{Sets}$ the exponential $C^B$ is a set of function space
between $C$ and $B$.
It's often important
diffentiate between $B \to C$, which is an \emph{arrow} in a category,
and a $C^B$ which is an \emph{object} in that category.
\Haskell\ "abuses" the notation by identifying $C^B = B \to C$.
\end{example}

\begin{example}[Awodey 6.4]
The category $\mathbf{Pos}$ has exponential objects.
For the exponential $Q^P$, we take the set of monotone functions,
\begin{equation*}
Q^P = \{ f : P \to Q \mid f\ \text{monotone} \}
\end{equation*}
ordered \emph{pointwise}, that is,
\begin{equation*}
f \le q \qquad\text{if and only if}\qquad f\,p \le q\,p\ \text{for all}\ p \in P.
\end{equation*}
The evaluation $\epsilon : Q^P \times P \to Q$
and transposition $\tilde{f} : X \to Q^P$ of a given arrow $f : X \times P \to Q$
are the usual ones of the underlying functions.
Awodey shows that these are monotone.
\end{example}

Coding this up in \Haskell\ is a bit trickier than previously.
We'll have a |newtype Monotone|,
to be able to write our instances.

\begin{code}
newtype Monotone a b = M { evalMonotone :: a -> b }
\end{code}

As a short digression, we can define |Category Monotone| instance.
In \Haskell\ |Category| type-class is defined for morphisms
in that category, where usually in category theory text we
speak about the objects: $\mathbf{Set}$, $\mathbf{Pos}$, $\mathbf{Latt}$\ldots
This is one more case where category theory and \Haskell\ ``differ in notation''.
\begin{code}
-- Category of partial orders
instance Category Monotone where
    id         = M id
    M g . M f  = M (g . f)
\end{code}

|Eq|, |Ord|, |PartialOrd| instances use |elements| provided by |Domain| type-class.
The choice of the name should become obvious now.

\begin{code}
instance (Domain a, Eq b) => Eq (Monotone a b) where
    M f == M g           = all (\x -> f x == g x) elements

instance (Domain a, PartialOrd b) => PartialOrd (Monotone a b) where
    leq (M f) (M g)      = all (\x -> leq (f x) (g x)) elements

instance (Domain a, Ord b) => Ord (Monotone a b) where
    compare (M f) (M g)  = foldMap (\x -> compare (f x) (g x)) elements
\end{code}

|Lattice| instances are defined pointwise.
Note, that while |Monotone| preserve joins and meets, also 
joins and meets of a arbitrary subset, but it doesn't
preserve $\top$ and $\bot$ elements.
Yet, all \emph{finite} lattices do have $\top$ and $\bot$ elements.

\begin{code}
instance Lattice b => Lattice (Monotone a b) where
    M f /\ M g  = M (\x -> f x /\ g x)
    M f \/ M g  = M (\x -> f x \/ g x)

instance BoundedJoinSemiLattice b => BoundedJoinSemiLattice (Monotone a b) where
    bottom      = M (const bottom)

instance BoundedMeetSemiLattice b => BoundedMeetSemiLattice (Monotone a b) where
    top         = M (const top)
\end{code}

|Display| instance simply lists the codomain elements:

\begin{code}
instance (Domain a, Display b) => Display (Monotone a b) where
    display (M f) = concatMap (display . f) elements
\end{code}

The tricky part is the |Domain| instance.
First we define a |isMonotone| helper, which checks
whether |f :: a -> b| is monotone, if it is, it returns |Just (M f)|,
otherwise |Nothing|.
As we assume that |a| and |b| have lawful |PartialOrd| instances,
it's enough to test for |ltPairs| only.

The |elements| are then generated by enumerating all possible functions (between
``monotone'' |elements| of |a| and |b|) and filtering out not monotone ones.
|elements| of |Monotone| doesn't contain all possible functions |a -> b|.

\begin{code}
isMonotone :: (Domain a, PartialOrd b) => (a -> b) -> Maybe (Monotone a b)
isMonotone f
    | all (\(V2 x y) -> leq (f x) (f y)) ltPairs  = Just (M f)
    | otherwise                                   = Nothing

instance (Domain a, Domain b) => Domain (Monotone a b) where
    elements = insertionSort leq $ case elements :: [b] of
        [] -> []
        bs@(b : _) -> mapMaybe isMonotone
            [  \a -> Map.findWithDefault b a pref
            |  pref <- expo elements bs
            ]

expo :: Ord a => [a] -> [b] -> [Map a b]
expo []      _   = [Map.empty]
expo (a:as)  bs  = bs >>= \b -> map (Map.insert a b) (expo as bs)
\end{code}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-bool2bool}
\qquad
\includegraphics[scale=0.4]{lattice-zho2bool}
\end{center}
\caption{|Bool -> Bool| and |ZHO -> Bool| lattices}
\label{fig:bool2bool}
\end{figure}

Now we can see few examples. |Bool -> Bool| instance is shown on \cref{fig:bool2bool}. That's |ZHO|!
The |ZHO -> Bool| instance on the same figure is the totally ordered set of size four.

\begin{code}
outputBoolToBool  ::  IO ()
outputBoolToBool  =
    outputGraph (Proxy :: Proxy (Monotone Bool Bool)) "lattice-bool2bool.png"

outputZHOToBool  ::  IO ()
outputZHOToBool  =
    outputGraph (Proxy :: Proxy (Monotone ZHO Bool)) "lattice-zho2bool.png"
\end{code}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-zho2zho}
\qquad\qquad
\includegraphics[scale=0.4]{lattice-bool2zho}
\end{center}
\caption{|ZHO -> ZHO| and |Bool -> ZHO| lattices}
\label{fig:zho2zho}
\end{figure}

The |ZHO -> ZHO| and |Bool -> ZHO|  lattices are getting a little
of complexity, see \cref{fig:zho2zho}. These are simple
examples highlighting that exponential objects from total ordered sets
to another may be partial (as well as products, |Bool `x` Bool| is not a toset).

\begin{code}
outputZHOToZHO  ::  IO ()
outputZHOToZHO  =
    outputGraph (Proxy :: Proxy (Monotone ZHO ZHO)) "lattice-zho2zho.png"

outputBoolToZHO  ::  IO ()
outputBoolToZHO  =
    outputGraph (Proxy :: Proxy (Monotone Bool ZHO)) "lattice-bool2zho.png"
\end{code}

Exponential lattices with |M2| are pretty. |ZHO -> M2| (\cref{fig:zho2m2})
has nice planar graph. The |M2 -> ZHO| (\cref{fig:m22zho}) has few
overlapping edges. |M2 -> M2| (\cref{fig:m22m2}) starts to 
exercise \emph{Graphviz} layout algorithm. Yet the final stress test is
|(ZHO -> ZHO) -> ZHO|, or $\mathtt{ZHO}^{\mathtt{ZHO}^\mathtt{ZHO}}$ is on \cref{fig:big}.
A beautiful monster.

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-zho2m2}
\end{center}
\caption{|ZHO -> M2| lattice}
\label{fig:zho2m2}
\end{figure}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-m22zho}
\end{center}
\caption{|M2 -> ZHO| lattice}
\label{fig:m22zho}
\end{figure}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.4]{lattice-m22m2}
\end{center}
\caption{|M2 -> M2| lattice}
\label{fig:m22m2}
\end{figure}

\begin{figure}[ht]
\begin{center}
\includegraphics[scale=0.2]{lattice-big}
\end{center}
\caption{|(ZHO -> ZHO) -> ZHO| lattice}
\label{fig:big}
\end{figure}

\begin{code}
outputM2ToM2   ::   IO ()
outputM2ToM2   =
    outputGraph (Proxy :: Proxy (Monotone M2 M2)) "lattice-m22m2.png"

outputZHOToM2   ::   IO ()
outputZHOToM2   =
    outputGraph (Proxy :: Proxy (Monotone ZHO M2)) "lattice-zho2m2.png"

outputM2ToZHO   ::   IO ()
outputM2ToZHO   =
    outputGraph (Proxy :: Proxy (Monotone M2 ZHO)) "lattice-m22zho.png"

outputBig   ::   IO ()
outputBig   =
    outputGraph (Proxy :: Proxy (Monotone (Monotone ZHO ZHO) ZHO))
        "lattice-big.png"
\end{code}

\section{Conclusion}

That was fun. Pretty pictures.
It was nice to learn that $\mathbf{Pos}$ and $\mathbf{Latt}$ are a \emph{cartesian closed}.

\begin{definition}[Awodey 6.2]
A category is called \emph{cartesian closed}, if it has all finite products and exponentials.
\end{definition}
In 6.6 section Awodey explains the correspondence between CCCs and $\lambda$-calculus:
\begin{equation*}
\text{CCC} \sim \lambda\text{-calculus}
\end{equation*}

That  means that we can write simply typed $\lambda$-calculus (STLC) programs,
and interpret them in CCC of our liking.
Conal Elliott describes how we can compile STLC
to CCCs and gives more examples of CCCs \cite{Elliott:2017}, there
are practical applications!
It's an interesting question, what would a programming with lattices
be useful for?

\newpage

\bibliography{Monotone}

\appendix

\section{Insertion sort}

Insertion sort topographically orders the list, given a partial order
decision procedure.

\begin{code}

insertionSort :: (a -> a -> Bool) -> [a] -> [a]
insertionSort le = go where
    go []      = []
    go (x:xs)  = insert x (go xs)

    insert x []                     = [x]
    insert x (y : ys)  | le x y     = x : y : ys
                       | otherwise  = y : insert x ys
\end{code}

\section{Display}
\label{sec:display}

Functions to render pretty pictures.

\begin{code}
displayDomain :: forall a. (Domain a, Display a) => Proxy a -> String
displayDomain _ = unlines $
    [  "digraph G {"
    ,  "rankdir=BT;"
    ,  "node [shape=box];"
    ]  ++
    [  show (display x) ++ " -> " ++ show (display y) ++ ";"
    |  V2 x y <- ltPairs :: [V2 a]
    ]  ++
    [  "}"
    ]

outputGraph :: (Domain a, Display a) => Proxy a -> FilePath -> IO ()
outputGraph p fp = void $ readProcess "dot"
    [ "-Tpng", "-o" ++ fp ]
    (displayDomain p)
\end{code}

\section{Another ZHO}

This is a wrapper over |ZHO| with complete |ltPairs|.

\begin{code}
newtype ZHO2 = ZHO2 ZHO deriving (Eq, Ord, PartialOrd, Display)

instance Domain ZHO2 where
    elements  = map ZHO2 elements
    ltPairs   = [ V2 x y | x <- elements, y <- elements, leq x y ]

outputZHO2 :: IO ()
outputZHO2 = outputGraph (Proxy :: Proxy ZHO2) "lattice-zho2.png"
\end{code}

\section{Main}
\label{sec:main}

The |main| function outputs all the images we defined above.

\begin{code}
main :: IO ()
main = do
    outputBool
    outputZHO
    outputZHO2
    outputM2
    outputBoolBool
    outputBoolZHO
    outputBoolOrBool
    outputBoolBoolOrBool
    outputBoolToBool
    outputZHOToBool
    outputBoolToZHO
    outputZHOToZHO
    outputM2ToM2
    outputZHOToM2
    outputM2ToZHO
    outputBig
\end{code}
\end{document}
