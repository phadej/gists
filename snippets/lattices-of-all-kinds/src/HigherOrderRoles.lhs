\input{../../preamble.tex}
\title{Higher-order roles}
\author{Oleg Grenrus}
%\date{2019-07-23}

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
%format /\ = "\mathbin\texttt{\textbackslash/}"
%format \/ = "\mathbin\texttt{\textbackslash/}"
%format `x` = "\times"
%format :~: = "\mathbin{:\sim:}"

%format role = "\text{\texttt{\textbf{role}}}"

\newcommand{\Nom}{\ensuremath{\mathsf{Nom}}}
\newcommand{\Rep}{\ensuremath{\mathsf{Rep}}}
\newcommand{\Phm}{\ensuremath{\mathsf{Phm}}}
\newcommand{\Iso}{\ensuremath{\mathsf{Iso}}}
\newcommand{\Role}{\ensuremath{\mathsf{Role}}}
\newcommand{\truncate}[1]{\ensuremath{\lVert #1 \rVert}}
\newcommand{\roleOf}[1]{\mathcal{R}\{#1\}}

\newcommand{\todo}[2]{{\color{darkred}{#1}}\marginpar{\footnotesize#1 #2}}

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
{-# LANGUAGE GADTs #-}
module HigherOrderRoles where

import Monotone
import Data.Coerce (Coercible, coerce)
import Data.Set (Set)
import Algebra.PartialOrd (PartialOrd (..))
import Algebra.Lattice (Lattice (..))
\end{code}
%endif

\section{Introduction}

These notes describe an idea for \emph{higher order roles} for zero-cost coercions
in \Haskell. I write these after reading
\emph{Higher-Order roles GHC proposal}\footnote{\url{https://github.com/ghc-proposals/ghc-proposals/pull/233}} by Richard Eisenberg
and its spin-off proposal \emph{Coercions without type constructor roles}\footnote{\url{https://github.com/ghc-proposals/ghc-proposals/pull/248}} by John Ericson.

\emph{Safe Zero-cost Coercions for Haskell} are introduced by Joachim Breitner et al.
\cite{Breitner:2014}.
There is also recent work for \emph{A Role for Dependent Types in Haskell} by Stephanie Weirich et al. 
\cite{1905.13706}.

Our idea is simple: embrace the higher order. If there are functions of types,
let there be function of roles.

A \emph{newtype} in \Haskell\
is a user-defined algebraic datatype with exactly one constructor; that constructor takes exactly one argument. Here is an example:
\begin{code}
newtype HTML = MkHTML String
\end{code}
Thanks to the work of Breitner et al. we can define a no-cost operation that
converts the list |[HTML]| to the list |[String]| by coercing between
these representationally equal types
\begin{code}
unpackList :: [HTML] -> [String]
unpackList = coerce
\end{code}

\section{Three different equivalences: roles lattice}

Breitner et al \cite[section 4.1: Roles and casts]{Breitner:2014}:

\begin{description}
\item[Nominal equality] written $\sim_\Nom$ is the equality that the
type checker reasons about. When \Haskell\ a programmer says that two
\Haskell\ types are the ``same'', we mean that the types are nominally
equal. Thus, we can say that $|Int| \sim_\Nom |Int|$.

\item[Representational equality] written $\sim_\Rep$
holds between two types that share the same run-time representation. Because
all types that are nominally equal also share the same representation, nominal
equality is a subset of representational equality. Continuing the example from
the introduction, $|HTML| \sim_\Rep |String|$.

\item[Phantom equality] written $\sim_\Phm$, holds between any two types,
whatsoever. It may seem odd that we produce and consume proofs of this “equality”, but doing so keeps the
system uniform and easier to reason about. It allows for
zero-cost conversions among types with phantom parameters.
We can say $|Int| \sim_\Phm |Char|$.
\end{description}

These three equalities form a lattice:
\begin{equation*}
\Nom \le \Rep \le \Phm
\end{equation*}
Nominal equality is the smallest equivalence ($x$ is related only to itself),
phantom equality is the largest one (everything is related),
representational equality is an equivalence in between.

We can codify this in \Haskell, so we can play with this lattice.
\begin{code}
data Role = Nom | Rep | Phm deriving (Eq, Ord, Show, Enum, Bounded)
instance PartialOrd Role where
    leq = (<=)
instance Lattice Role where
    (/\) = min
    (\/) = max
\end{code}

There could be more equivalences in between, for example user-defined
\emph{isomorphism}. That equality would be larger than representational
equality. However, such equality cannot be used by solvers
automatically: for example there are two $|Bool| \sim_\Iso |Bool|$,
induced by |id| and |not|. On the other hand the three
equivalences we consider are \emph{propositional}, in other words
$|A| \sim_R |B|$ witnesses are unique if they exist.
It's clear for nominal and phantom equalities, representational
equalities ``normal forms`` could be thought to be in the form of
$|A| \sim_\Rep |B| = |A| \sim_\Rep |R| \circ |R| \sim_\Rep |B|$,
where |R| is the type of underlying representation.

By solvers we mean an elaborators from surface syntax to (explicitly typed)
internal representation. Hardly anyone would like to write
equivalence (coercion) plumbing by hand. Elaborator inside a type-checker
should do that.

The role lattice of \Nom, \Rep, and \Phm\, let's call it \Role, has some nice properties.
To begin with, it lives in a category of \emph{finite} (and therefore \emph{complete}) lattices.
That category is \emph{cartesian closed}, which means that
there are all ``function space'' like things.
It's meaninful to talk about $\Role \to \Role$ object, which is a finite lattice
itself.
Because these lattices are complete, we can use Kleene fixed-point theorem (cite?).

Syntactically, we make only small change, but I think it will work out.
We change from ``a first order''
\begin{spec}
type role Map nominal representational
\end{spec}
to a higher order
\begin{spec}
type role Map k v = truncate k \/ v
\end{spec}
We'll explain |truncate| function in the next section.

\section{Type $\to$ Type roles}

|Type -> Type| kinds are prelevant in \Haskell\, so let us see how they
would work through couple of examples.

\begin{example}[Box]
Let's start with a simple type
\begin{spec}
data Box a = Box a
\end{spec}
For two unknown types |b| and |c| we can reason, that if
\begin{align*}
|b| \sim_\Nom |c| &\Longrightarrow |Box b| \sim_\Nom |Box c| & \text{e.g. } |b| = |c| = |Int| \\
|b| \sim_\Rep |c| &\Longrightarrow |Box b| \sim_\Rep |Box c| & \text{e.g. } |b = Html|, |c = String| \\
|b| \sim_\Phm |c| &\Longrightarrow |Box b| \sim_\Phm |Box c| & \text{e.g. } |b = Int|, |c = String| \\
\end{align*}
The role of |Box| is pleasingly simple, it's the identity function:
\begin{equation*}
\roleOf{|Box|} = \lambda\,|a| \mapsto |a|
\end{equation*}
Currently in \Haskell\, we'd write that as
\begin{spec}
type role Box representational
\end{spec}
\end{example}

\begin{example}[Proxy]
Even simpler type than |Box|, is |Proxy|.
\begin{spec}
data Proxy a = Proxy
\end{spec}
For two unknown types |b| and |c| we can reason, that if
\begin{align*}
|b| \sim_\Nom |c| &\Longrightarrow |Proxy b| \sim_\Nom |Proxy c| \\
|b| \sim_\Rep |c| &\Longrightarrow |Proxy b| \sim_\Rep |Proxy c| \\
|b| \sim_\Phm |c| &\Longrightarrow |Proxy b| \sim_\Rep |Proxy c|
\end{align*}
Note, that \Phm\ role is transfomed to \Rep: |Proxy Int| and |Proxy String|
are representationally equivalent.
\begin{equation*}
\roleOf{|Proxy|} = \lambda\,|a| \mapsto |a| \land \Rep
\end{equation*}
Currently in \Haskell\, we'd write that as
\begin{spec}
type role Proxy phantom
\end{spec}
\end{example}

\begin{example}[Set]
The third example is |Set a|. |Set a| is an opaque data type,
which has internal invariants, which rely on |Ord a| instance behaviour.
For example |Set (Down Int)| coerced from |Set Int| will be badly broken.
\begin{spec}
data Set a = ...
\end{spec}
For two unknown types |b| and |c| we can reason, that if
\begin{align*}
|b| \sim_\Nom |c| &\Longrightarrow |Set b| \sim_\Nom |Set c| \\
|b| \sim_\Rep |c| &\Longrightarrow |Set b| \sim_\Phm |Set c| \\
|b| \sim_\Phm |c| &\Longrightarrow |Set b| \sim_\Phm |Set c|
\end{align*}
This kind of function cannot be expressed with lattice operations $\land$ or $\lor$
so we introduce a new primitive, $\lVert - \rVert$ or |truncate|.
\begin{align*}
\truncate{\Nom} &= \Nom \\
\truncate{\Rep} &= \Phm \\
\truncate{\Phm} &= \Phm
\end{align*}
More generally, \truncate{-} can be defined for any bounded lattice:
\begin{align*}
\truncate{\bot} &= \bot \\
\truncate{-}    &= \top
\end{align*}
Truncation here is in a sense of ``if it's not really equal, than it's not equal at all``.

Using this new operator we can write
\begin{equation*}
\roleOf{|Set|} = \lambda\,|a| \mapsto \truncate{|a|}
\end{equation*}
Currently in \Haskell\, we'd write that as
\begin{spec}
type role Set nominal
\end{spec}
\end{example}

In previous three examples we have seen how current |nominal|, |representational| and |phantom| roles
are represented. This approach simple generalises to higher orders though.
Before going there, let's investigate these examples a bit more.

\begin{figure}[ht]
\centering
\includegraphics[scale=0.5]{lat-zho2zho}
\caption{$\Role\to\Role$ lattice}
\label{fig:role2role}
\end{figure}

\begin{figure}[ht]
\centering
\includegraphics[scale=0.5]{lat-zho2zho-a}
\caption{$\Role\to\Role$ lattice, where $\Nom \mapsto \Nom$}
\label{fig:role2role-a}
\end{figure}

In all three previous examples we have
\begin{equation*}
\roleOf{|Box|}\,\Nom = \roleOf{|Proxy|}\,\Nom = \roleOf{|Set|}\,\Nom = \Nom
\end{equation*}
This is a property of type constructors (and type families).
However, in theory we should (\todo{do we?}{or can solver just fail}) be able to represent ``non-anchored''
functions as well.
On \cref{fig:role2role} we can see that $\Role\to\Role$ lattice has ten
elements; and even if we ``anchor'' \Nom, \cref{fig:role2role-a}, there are
still 6 elements. What are the other ones (\todo{good for}{what})?
Maybe it's enough to consider functions which only grow, i.e. $\forall r. r \le f r$,
that's wrong (|phantom| is $\Nom\cdot\Rep\cdot\Rep$, last $\Phm\not\le\Rep$),
on the other hand
$|a| \sim_\Rep |b| \mapsto |f a| \sim_\Nom |f b|$,
i.e. |Coercible a b => f a :~: f b| is impossible, \todo{is it}{or is there some weird scenario, with GADTs e.g.}?.
Is this something solver can use as \todo{a heuristic}{I have no ideas about solver}?


\bibliography{Monotone}

\appendix
\section{TODO}
\begin{itemize}
\item not star kinds
\end{itemize}

\begin{code}
newtype Age = MkAge Int
data Phantom b = Phantom
data NestedPhantom b = MkNP [Phantom b] | SomethingElse
\end{code}
 
\begin{code}
data Equality a b where
    Refl :: Equality a a

data Coercion a b where
    Coerce :: Coercible a b => Coercion a b

data Universal a b where
    Something :: Universal a b
\end{code}

\end{document}
