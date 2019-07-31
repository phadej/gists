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
%format ~ = "\sim"
%format k1 = "\mathtt{k}_1"
%format k2 = "\mathtt{k}_2"
%format r1 = "\mathtt{r}_1"
%format r2 = "\mathtt{r}_2"
%format <$> = "\mathbin{\texttt{<\$>}}"
%format <*> = "\mathbin{\texttt{<*>}}"
%format ==  = "\mathbin{\texttt{==}}"
%format />  = "\rightharpoonup"
%format @@  = "\cdot"
%format `nah` = "\ne"

% REPL prompt
%format *>>> = "\lambda\!\!\vartriangleright"

% Additional keyword
%format role    = "\text{\texttt{\textbf{role}}}"
%format family  = "\text{\texttt{\textbf{family}}}"

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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module HigherOrderRoles where

import Monotone
import Algebra.Lattice (Lattice (..), meets, BoundedMeetSemiLattice (..), BoundedJoinSemiLattice  (..), joins, BoundedLattice, lfp, gfp, gfpFrom, lfpFrom)
import Algebra.PartialOrd (PartialOrd (..))
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Control.Applicative (ZipList (..))
import Data.Maybe (fromJust)
import Data.Set (Set)

import qualified Control.Category as C

type Kind = Type
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
instance BoundedMeetSemiLattice Role where
    top = Phm
instance BoundedJoinSemiLattice Role where
    bottom = Nom
instance Domain Role where
    elements =  insertionSort leq [ minBound .. maxBound ]
instance Display Role where
    display = show
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
type role Map k v = nominal k /\ representational v
\end{spec}
We'll explain |truncate| function in the next section.

\section{Type $\to$ Type roles}

|Type -> Type| kinds are prelevant in \Haskell\, so let us see how they
would work through couple of examples. But befor that,
let's define few functions.

\begin{definition}[$\Role\to\Role$ functions] To manipulate roles
we need few primitives. Naming is suggestive.
\begin{code}
nominal :: Role -> Role
nominal Nom  = Nom
nominal _    = Phm

representational :: Role -> Role
representational r  = r

phantom :: Role -> Role
phantom Nom = Nom
phantom _   = Rep
\end{code}
\end{definition}

\begin{example}[representational |Box|]
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

\begin{example}[phantom |Proxy|]
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

\begin{example}[nominal |Set|]
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
so we introduce a new primitive, $\lVert - \rVert$ or |nominal|.
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

\begin{figure}[hp]
\centering
\includegraphics[scale=0.5]{lat-zho2zho}
\caption{$\Role\to\Role$ lattice}
\label{fig:role2role}
\end{figure}

\begin{figure}[hp]
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


\section{Equivalences of higher order}

It's relatively simple to understand what does the $\sim_\Nom$ or $\sim_\Rep$
means. In \Haskell\ and type theories we may write |f :~: g| or $f \equiv g$
for things of the |Type -> Type| kind. Yet for representational equality,
we at best could write something like
\begin{spec}
type Coercible1 f g = forall x y. Coercible x y => Coercible (f x) (g y)
\end{spec}
What does $f \sim_{\lambda x \mapsto y} g$ mean?

We propose that the answer is \emph{functional extensionality}

\begin{definition}[functional extensionality]
\emph{functional extensionality} says that two functions are equal,
if their .... It's an \emph{external} properety, therefore a name,
It's an axiom, which doesn't (alone?) break
your type theory.  A non-dependent version is, for objects $f, g : A \to B$
\begin{equation*}
f \equiv_{A\to B} g \cong \prod_{x : A}  f\,x \equiv_B f\,x
\end{equation*}
\end{definition}

We can extend functional extensionality right hand side to be
\begin{equation*}
\prod_{x y : A} x \equiv_A y \to f\,x \equiv_B f\,y
\end{equation*}

Getting ahead of ourselves, we can write something similar
in \Haskell:
\begin{code}
type Equality1 f g = forall x y. Equality x y -> Equality (f x) (g y)
\end{code}
Let's define |Equality| and other equivalences next.

\begin{definition}[Equivalence witnessess] Using GADTs
we can capture evidence of equivalences:
\begin{code}
data Equality a b where
    Refl       :: a ~ b          =>  Equality a b

data Coercion a b where
    Coerce     :: Coercible a b  =>  Coercion a b

data Universal a b where
    Something  ::                    Universal a b
\end{code}
\todo{a name for |Universal| and |Something|}{could be something else, but what?}
We can also write a type family, mapping a promoted |Role| to a \emph{witness}
of respectful equivalence. \todo{|RoleForType|}{is a bad name}
\begin{code}
type family RoleForType (r :: Role) :: Type -> Type -> Type where
    RoleForType Nom = Equality
    RoleForType Rep = Coercion
    RoleForType Phm = Universal
\end{code}
\end{definition}

\begin{remark}[Everything respects $\sim_\Nom$]
\label{remark:congurence}
As the $\sim_\Nom$ is propositional equality, everything should respect it,
we have a principle of congruence. For all types, |x, y| and type functions
|M :: A -> B|
\begin{equation*}
|x| \sim_\Nom |y| \to |M x| \sim_\Nom |M y|
\end{equation*}
\begin{code}
cong :: Equality a b -> Equality (f a) (f b)
cong Refl = Refl
\end{code}
The above holds for type constructors and type families.
However, for other equivalences, e.g. $\sim_\Rep$ similar statement doesn't hold,
in general
\begin{equation*}
x \sim_\Rep y \not\to M\,x \sim_\Rep M\,y
\end{equation*}
For a counter example, take $M = |Set|$.
\end{remark}

\begin{definition}[Equivalence witnesses indexed by roles]
We can generalise |RoleForType| to higher kinds. The following will be in pseudo-\Haskell.

Assume there is a data type for \emph{closed role expressions},
indexed by their kind:
\begin{code}
data RoleExpr (k :: Kind) where
    RoleConst  :: Role -> RoleExpr Type
    RoleId     :: RoleExpr (k -> k)
    -- \ldots STLC syntax, in some way, plus primitives
\end{code}

\todo{Add an execution of RoleExpr}

Then there could a type or data family, or GADT for witnesses,
generalising |RoleForType|
\begin{code}
data Witness (k :: Type) (r :: RoleExpr k) (x :: k) (y :: k) where
  W_Const  ::  SRole r
           ->  RoleForType r x y
           ->  Witness Type (RoleConst r) x y
  W_Id     ::  (forall r u v. Witness k r u v -> Witness k r (f u) (g v))
           ->  Witness (k -> k) RoleId f g
  -- \ldots and for other constructors
\end{code}
The |SRole| is defined in the next example.

A \emph{tricky part} of this whole business is to completely define |RoleExpr|
and |Witness|. Especially in a way which is amenable for an implementation.
Note, that as currently, |Witness| would be 0-bit data type; this
definition is to simulate what a compiler could do internally.
\end{definition}

\begin{example}[singletons overdrive]
This example hopefully highlights, that we don't do anything beyond
any compiler level of understand. GHC could handle this already. It's just not convininent for real use.
Let's define singletons for |Role| so we can define some |Witness|.
\begin{code}
data SRole (r :: Role) where
    SNom :: SRole Nom
    SRep :: SRole Rep
    SPhm :: SRole Phm

class     SRoleI (r :: Role)  where srole  ::  SRole r
instance  SRoleI Nom          where srole  =   SNom
instance  SRoleI Rep          where srole  =   SRep
instance  SRoleI Phm          where srole  =   SPhm
\end{code}
And we can play with |Witness|
\begin{code}
newtype Identity a = Identity { runIdentity :: a }

identityW :: Witness (Type -> Type) RoleId Identity Identity
identityW = W_Id witness where
    witness   ::  forall r x y. Witness Type r x y
              ->  Witness Type r (Identity x) (Identity y)
    witness (W_Const r equiv) = witness' r equiv

    witness'  ::  SRole r -> RoleForType r x y
              ->  Witness Type (RoleConst r) (Identity x) (Identity y)
    witness' SNom  Refl    = W_Const SNom Refl
    witness' SRep  Coerce  = W_Const SRep Coerce
    witness' SPhm  _       = W_Const SPhm Something
\end{code}
\end{example}

\begin{example}[Injectivity]
only for type constructors. \todo{TBW}{}
\end{example}

Now we can answer a question, what does $|f| \sim_\Nom |g|$ means.
We need a $r : \Role\to\Role$, such that $r\,\Nom = \Nom$.
That will give us the congruence we mentioned previously in \cref{remark:congurence}.
We could define something like
\begin{equation*}
\Nom_{\Role\to\Role} = \sum_{r : Role\to\Role} r \le \Nom\cdot\Phm\cdot\Phm
\end{equation*}
but I think it's simply too complicated, ``assumping the worst'' should
work as well. We can simply overestimate:
\begin{equation*}
\Nom_{\Role\to\Role} = \Nom\cdot\Phm\cdot\Phm
\end{equation*}
We can identity such ``good enough to respect nominal equality'' element,
for each kind and define
\begin{equation*}
\Nom_{r_1 \to r_2} = \bigvee_{\mathclap{f(\Nom_{r_1}) \le Nom_{r_2}}} f
\end{equation*}
\begin{code}
class (Domain r, BoundedLattice r) => Nominal r where
    nominal' :: r
instance Nominal Role where
    nominal' = Nom
instance (Nominal r1, Nominal r2) => Nominal (Monotone r1 r2) where
    nominal' = joins [ f | f <- elements, evalMonotone f nominal' `leq` nominal' ]

test01 :: Bool
test01 = isMonotone nominal == Just nominal'
\end{code}

For phantom roles we'll need a definition, which preserves nominal
equality, but otherwise marks everything as phantom.
\begin{code}
phantom' :: Nominal r => r -> Role
phantom' r  | leq r nominal'  = Nom
            | otherwise       = Rep
\end{code}
|phantom'| is monotone. Check!


\begin{example}[$\Rep\cdot\Rep\cdot\Rep$ equivalance]
\todo{TBW}{}
\end{example}


\section{Current approach overapproximates}

Consider a simple data type:
\begin{code}
data Ap f a = Ap (f a)
\end{code}

GHC tells that its role is
\begin{spec}
type role Ap representational nominal
\end{spec}

We can do
\begin{code}
coerceAp1 :: Ap [] a -> Ap ZipList a
coerceAp1 = coerce
\end{code}
or even
\begin{code}
coerceAp2 :: Coercible f g => Ap f a -> Ap g a
coerceAp2 = coerce
\end{code}
where |Coercible f g| is weird beast. But not
\begin{spec}
coerceAp3 :: Coercible a b => Ap [] a -> Ap ZipList b
coerceAp3 `nah` coerce
\end{spec}

So it looks like that current role for |Ap| can be intepretted as a role expression
\begin{code}
apRoleNow :: (Monotone Role Role, Role) -> Role
apRoleNow (f, a)  =   f' @@ a
                  \/  nominal' @@ a
  where
    f' = f -- f is representational/parametric, so identity mapping
\end{code}
but we can make more precise role
\begin{code}
apRole :: (Monotone Role Role, Role) -> Role
apRole (f, a) = f @@ a
\end{code}

which is smaller, i.e. better: $|apRole| \le |apRoleNow|$

\begin{spec}
*>>> leq <$> isMonotone apRoleNow <*> isMonotone apRole
Just False
*>>> leq <$> isMonotone apRole <*> isMonotone apRoleNow
Just True
\end{spec}

\subsection{Reasoning}

Lattices are used in constraint solving a lot (\todo{maybe I should find at least a citation}{}),
in a language with $\lor$ and $\land$ and equality $=$.  One could
ask questions like
\begin{equation*}
x \lor c_1 \land c_2 = c_3  \quad\Rightarrow\quad x = \text{?}
\end{equation*}

Here we try to solve more complex problem, where we have monotone functions
as well
\begin{equation*}
f(c_1) \lor c_2 \land c_3 = c_4 \quad\Rightarrow\quad f = \text{?}
\end{equation*}

Where former problem is ``simple'', latter is hard, isn't it?
In both cases we are interested in ``the best'' (smallest) solution, i.e. for all other solutions $y$ or $g$
$x \le y$ or $f \le g$.

For finite ``generating'' lattices, like $\Role$, we can solve
the problems by simply enumerating; but that's extremely slow.
We need to be more clever.
In first order problem enumerating is ok approach: there are only three options.

Currently in GHC roles are using first-order language (there are functions, but they all are known)
\begin{equation*}
|type role Ap representational nominal| \Rightarrow \roleOf{|Ap|} = \lambda\,(f : \Role)\,(x : \Role) \mapsto f \lor \mathsf{nominal}(x)
\end{equation*}
but we want higher-order:
\begin{equation*}
|type role Ap = \f a -> f a| \Rightarrow \roleOf{|Ap|} = \lambda\,(f : \Role\to\Role)\,(x : \Role) \mapsto f(x)
\end{equation*}

\section{Fixed points}

\todo{TBW}{}

\section{Backreasoning}
\label{sec:backreasoning}

\todo{TBW}{}

\section{Rules sketch}

\todo{TBW}{}

\section{Role inference}

The procedure (for non recursive ADTs) is quite direct, and functional!
\begin{enumerate}
\item mark all arguments as |phantom'| to respect nominal equality
\item translate the ADT syntax as is, replacing all sums and products operations
by $\lor$, and concrete types by their role.
\todo{Role for concrete types, like |Int| is \Nom}{explain why}.
\item simplify by evaluation
\end{enumerate}

\begin{example}[role for |Foo|]
Let's consider simple ADT
\begin{code}
data Foo a = FooInt Int | FooX a
\end{code}
The raw translation is
\begin{code}
fooRole :: Role -> Role
fooRole a  =   phantom' a  -- marking all arguments phantom by default
           \/  Nom         -- |FooInt| constructor
           \/  a           -- |FooX| constructor
\end{code}
And |fooRole| is |representational| as expected.
\begin{spec}
*>>> display <$> isMonotone fooRole
Just "NomRepPhm"
*>>> (==) <$> isMonotone fooRole  <*> isMonotone representational
Just True
\end{spec}
\end{example}

\begin{example}[some more exciting types]
\todo{TBW}{}
\end{example}

\begin{example}[some recursive types]
\todo{TBW}{}
\begin{code}
data List a = Nil   | Cons a  (List a)
data Nat1 a = Zero  | Succ    (Nat1 a)
\end{code}
\begin{code}
listRole :: Role -> Role
listRole = evalMonotone $ lfp $ \rec -> unsafeMonotone $ \a ->
        phantom' a
    \/  a \/ evalMonotone rec a

nat1Role :: Role -> Role
nat1Role = evalMonotone $ lfp $ \rec -> unsafeMonotone $ \a ->
        phantom' a
    \/  evalMonotone rec a
\end{code}
Should be |listRole = representational|, |nat1Role = phantom|. seems to work
\todo{tests}{, we need to write}
\end{example}

\begin{example}[mutually recursive types]
\todo{TBW}{}
\end{example}

\begin{example}[universal]
\todo{TBW}{}
\end{example}

\begin{example}[existentials] Existentials contain *some* variables,
we must assume the worst, i.e. |nominal'|.

Here |/>| is a monotone function, and |f @@ x| is application,
i.e. |Monotone| and |evalMonotone|.
\begin{code}
data Exists c a where
    MkExists :: c a b => b -> Exists c a
\end{code}

\begin{code}
existsRole :: (Role /> Role /> Role) -> Role -> Role
existsRole c a  =   phantom' c \/ phantom' a
                \/  c @@ a @@ b \/ b
  where
    b = nominal'

\end{code}

If |c = ~|, which has nominal roles
\begin{code}
equalityRole :: Role /> Role /> Role
equalityRole = nominal'

-- $\Nom\cdot\Phm\cdot\Phm$
existsEq :: Role -> Role
existsEq = existsRole equalityRole
\end{code}

Or if |c = Coercible|
\begin{code}
coercibleRole :: Role /> Role /> Role
coercibleRole = joinM

-- $\Nom\cdot\Rep\cdot\Phm$
existsCoe :: Role -> Role
existsCoe = existsRole coercibleRole
\end{code}

Recall, normal type-classes have nominal role, |Coercible| is special.
I cannot come with a reason for $|Dict (SomeClass a)| \sim_\Rep |Dict (SomeClass b)|$.

With \texttt{FunctionalDependencies}:
\begin{code}
class Monad m => MonadReader r m | m -> r where
  ask :: m r
\end{code}
What the role should be? (Note our |MonadReader| doesn't have non-algebraic |local|).
From $|Dict (MonadReader r1 m)| \sim_\Nom |Dict (MonadReader r2 m)|$
we should be able to deduce $|r1| \sim_\Nom |r2|$, shouldn't we?

And this does work in GHC today!
\begin{code}
data Dict c where
    MkDict :: c => Dict c

test1  ::  Equality (Dict (MonadReader r1 m)) (Dict (MonadReader r2 m))
       ->  Equality r1 r2
test1   =   \Refl -> Refl

test2  ::  Coercion (Dict (MonadReader r1 m1)) (Dict (MonadReader r2 m2))
       ->  (Equality r1 r2, Equality m1 m2)
test2  =   \Coerce -> (Refl, Refl)
\end{code}

Explain this in \cref{sec:backreasoning}
\end{example}

\begin{example}[Void1] |Void1| is no different from |Proxy| role-wise.
\begin{code}
data Void1 (a :: Type)

-- $\Nom\cdot\Rep\cdot\Rep$
void1Role :: Role -> Role
void1Role a = phantom' a
\end{code}
\end{example}

\begin{example}[Fix] Fixed points need fixed points.
\begin{code}
newtype Fix f = Fix (f (Fix f))
\end{code}

\begin{spec}
type role Fix nominal  -- GHC now
\end{spec}

\begin{code}
fixRole :: (Role /> Role) -> Role
fixRole = evalMonotone $ lfp $ \rec -> unsafeMonotone $ \f ->
   phantom' f \/
   f @@ evalMonotone rec f

fixRole2 :: (Role /> Role) -> Role
fixRole2 f = lfp (\x -> phantom' f \/ f @@ x)

-- current GHC
approx :: (Role /> Role) -> Role
approx r = nominal' @@ r

-- $|ZipList| \sim_{|axiomZipListList|} |List|$
axiomZipListList :: Role /> Role
axiomZipListList = unsafeMonotone $ \case
    Nom -> Rep
    Rep -> Rep
    Phm -> Phm
\end{code}
\end{example}


\section{Dependent Haskell}

\section{Mumblings}


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
\item Can \emph{Compiling to categories} make code for writing role expressions
nicer? i.e. no |->| in types, only |/>|, but ordinary lambda expressions.
\end{itemize}

\begin{code}
newtype Age = MkAge Int
data Phantom b = Phantom
data NestedPhantom b = MkNP [Phantom b] | SomethingElse
\end{code}

\end{document}
