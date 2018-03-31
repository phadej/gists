---
title: Free Monad and Free Applicative using single Free type
subtitle: monads and applicatives are just a monoids in the category of endofunctors
author: Oleg Grenrus
---

I define free `Monad` and free `Applicative` using single type, parametrised
over a tensor.  It's possible, because both `Monad` and `Applicative` are
monoids in the category of endofunctors.  There is little, if any, practical
applications I can foresee for these definitions, but it's fun to see how
things are connected.  I won't prove laws hold, or constructions are free (=
are left adjoint of a forgetful functor).

The post was inspired by Bartosz Milewski blog post about [Free Monoidal Functors](https://bartoszmilewski.com/2018/02/17/free-monoidal-functors/),
and only while writing this I discovered
[Notions of Computations as Monoids](http://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids_ext.pdf),
read that for rigid explanations.

*Warning*: plenty of infix operators ahead. Also formulas typeset with LaTeX.

<div id="toc"></div>

Introduction
------------

Free applicative functors are free monoids where Day convolution
is a tensor:

$$ \mathit{FreeA}\; F = \mu G. 1 + F \star G $$

On the other hand, free monads are also free monoids, but with function
composition as monoidal product.

$$ \mathit{FreeM}\; F = \mu G. 1 + F \circ G $$

Note, lists are also free monoids where product is a Cartesian product
(different category)!

$$ \mathit{List}\; A = \mu B. 1 + A \times B $$

So we can extract the common *free monoid* notion:

$$ \mathit{Free}_\otimes\; F = \mu G. 1 + F \otimes G $$

or

$$
\sum_{n\ge0} F^{\otimes n} = 1 + F + F \otimes F + F \otimes F \otimes F + \cdots
$$

as in [free monoid - ncatlab](https://ncatlab.org/nlab/show/free+monoid) article.

Using $\mathit{Free}_\otimes$ we can abstract over applicatives and monads (and lists):

$$
\begin{aligned}
\mathit{FreeA} &= \mathit{Free}_\star \\
\mathit{FreeM} &= \mathit{Free}_\circ  \\
\mathit{List}  &= \mathit{Free}_\times
\end{aligned}
$$

Let's try to encode all of that in Haskell.

This blog post is a literate Haskell file.
For this work we don't need a lot of extensions, they'd *almost* fit on one line.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module SingleFree where

-- all these imports are from the base library
import Control.Applicative ((<**>))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import GHC.Exts (Constraint)

-- for examples
import Control.Monad.Trans.Writer (Writer, tell, runWriter)
import Data.Functor.Const (Const (..))
import Text.Read (readMaybe)

-- we'll define own isomorphic variants.
import qualified Data.Functor.Compose as B -- from base
import qualified Data.Functor.Day as K     -- from kan-extensions
```

Free monoid in the category of endofunctors
-------------------------------------------

Let's start by defining a free monoid.
The definition resembles in shape the definition of lists,
but is higher-order and parametrised over a tensor `h` and its unit `i`:

```haskell
data Free (h :: (* -> *) -> (* -> *) -> (* -> *)) i f a
    = Done (i a)
    | More (h f (Free h i f) a)
```

We could also used a `FixH` (as Bartosz does in his post) and separate `FreeF`
algebra, but for our purposes splitting out recursion is not necessary, and
would only clutter the presentation.

Next we need tensors. These definitions are unorthodox. Enlightened
reader would guess why we use such variants. We will explain that
in the respective sections.

```haskell
data Day f g a where
    (:<**>:) :: f x -> g (x -> a) -> Day f g a

data Comp f g a where
    (:>>=:)  :: f x -> (x -> g a) -> Comp f g a

infix 1 :>>=:
```

Using `Free` we can right away define free `Applicative` and free `Monad`:
Both tensors have `Identity` as a unit functor.

```haskell
type FreeA = Free Day Identity
type FreeM = Free Comp Identity
```

Let's see how to define the operations on `Free`.
We can immediately define `pure` (or `return`), independently of choice of `h` or `f`:

```haskell
point :: a -> Free h Identity f a
point = Done . Identity
```

As both free applicative and free monad are free monoids in category of
endofunctors, it shouldn't be surprising that `pure` and `return` look like
empty list. (Even there's `a`, there aren't any `f`).

Both `Applicative` and `Monad` instances can be defined in a very similar
way using auxiliary classes `HAppend` and `FromDay`:

```haskell
instance (FromDay h, i ~ Identity) => Applicative (Free h i f) where
    pure    = point
    f <*> x = happend $ fromDay $ f :<**>: fmap (flip id) x

instance (h ~ Comp, i ~ Identity) => Monad (Free h i f) where
    return  = point
    m >>= k = happend $ m :>>=: k
```

`FromDay` witnesses a tensor homomorphism from `Day` to `h`.

```haskell
class (HAppend h, I h ~ Identity) => FromDay h where
    fromDay :: Day ~~> h
```

The long squiggly arrow is a transformation between `Tensor`s:

```haskell
type h ~~> h' = forall (f :: * -> *) g a. Functor g => h f g a -> h' f g a
```

`Day` can be converted to `Day` trivially:

```haskell
instance FromDay Day where
    fromDay = id
```

`HAppend` is a generalization of `++` for our `Free` lists, `happend` appends two
`Free h` "lists". We need such class as plumbing is different for different choices of
`h`. (Here I really miss that we cannot use infix operator as a type variable!)

$$
\mathit{happend} : \mathit{Free}_\otimes\; F \otimes \mathit{Free}_\otimes\; F \to Free_\otimes F
$$

```haskell
class Tensor h => HAppend h where
    happend :: Free h (I h) f `h` Free h (I h) f ~> Free h (I h) f
```

The short squiggly arrows are natural transformations:

```haskell
infixr 0 ~>
type f ~> g = forall a. f a -> g a
```

<h3>Lifting and retracting</h3>

One interesting operation is lifting `f a` into a free structure.
For that we need more structure from `h`, in particular
left unitor `intro1`. Then we map $1$ into `Done`, and wrap that into `More`.
In other words that's a `singleton` for a list.

$$
\begin{aligned}
\mathit{intro}_1 &: F \to F \otimes 1 \\
\mathit{Done} &: 1 \to \mathit{Free}_\otimes\; F \\
\mathit{More} &: F \otimes \mathit{Free}_\otimes\; F \to \mathit{Free}_\otimes\; F
\end{aligned}
$$

For this operation we show types of intermediate results:

$$
\begin{aligned}
\mathit{fa} &: F \\
\mathit{intro}_1\; \mathit{fa} &: F \otimes 1 \\
\mathit{hbimap}\; \mathit{id}\; \mathit{Done}\; (\mathit{intro}_1\; \mathit{fa}) &: F \otimes \mathit{Free}_\otimes\; F \\
\mathit{More}\; (\mathit{hbimap}\; \mathit{id}\; \mathit{Done}\; (\mathit{intro}_1\; \mathit{fa})) &: \mathit{Free}_\otimes\; F
\end{aligned}
$$

Or in Haskell:

```haskell
liftFree :: Tensor h => f a -> Free h (I h) f a
liftFree fa = More $ hbimap id Done $ intro1 fa
```


The last operation is retracting of a free structure, one can spot the
resemblance with `foldMap`.

```haskell
retractFree :: forall h f g a. (HApply h, C h g)
            => (f ~> g) -> Free h (I h) f a -> g a
retractFree _nt (Done x) = hpure (Proxy :: Proxy h) x
retractFree  nt (More x) = happly (hbimap nt (retractFree nt) x)
```

`HApply` "applies" or retracts the tensor. That doesn't work for all `f`, so we
need a constraint. It carries natural transformations

$$
\begin{aligned}
\mathit{hpure} &: 1 \to F  \\
\mathit{happly} &: F \otimes F \to F
\end{aligned}
$$

Or in Haskell terms:

```haskell
class Tensor h => HApply h where
    type C h :: (* -> *) -> Constraint

    hpure  :: C h f => Proxy h -> I h a -> f a
    happly :: C h f => h f f a -> f a
```

Examples
--------

<h3>Free Monad</h3>

The basic example for free monads is the restricted IO.
First we define the operation type (note, not a `Functor`):

```haskell
data Console a where
    Output :: String -> Console ()
    Input  :: Console String
```

Test console writes output into `Writer` log, and always returns `"yes"` as an input.

```haskell
testConsole :: Console ~> Writer [String]
testConsole (Output s) = tell [s]
testConsole Input      = return "yes"
```

The runnable example of reading an input once, writing it and `"done"`,
and returning the read value as result of the computation:

```haskell
-- ("yes",["yes", "done"])
exampleM :: (String, [String])
exampleM = runWriter $ retractFree testConsole $ do
    x <- liftFree Input
    liftFree $ Output x
    liftFree $ Output "done"
    return x
```

<h3>Free Applicative</h3>

Free applicatives are used a lot in various parsers. For example
a naive command line argument parser: First a type encapsulating
an option (example as in [Free Applicative Functors](https://arxiv.org/pdf/1403.0749.pdf) by Paolo Capriotti and Ambrus Kaposi)

```haskell
data Option a = Option
    { optName   :: String
    , optReader :: String -> Maybe a
    }
```

Then we need a type for a value we need to parse, let's use simple `User`:

```haskell
data User = User String Int
```

The example parser could look like:

```haskell
userParser :: Free Day Identity Option User
userParser = User
    <$> option "name"
    <*> option "id"
  where
    option :: Read a => String -> Free Day Identity Option a
    option n = liftFree $ Option n readMaybe
```

And because the structure is static, we can not only *run* the parser,
but also generate "help" message:

```haskell
-- ["name","id"]
exampleA :: [String]
exampleA = getConst $ retractFree nt userParser where
    nt :: Option ~> Const [String]
    nt = Const . return . optName
```

Higher-order functors and tensors
---------------------------------

In this section we define `HBifunctor`, `Tensor` and other related type classes.

As we deal with tensors which are (higher-order) bifunctors, we will need a
variant of `Bifunctor` for higher order functors. In this post will have a
special version, one where `bfmap` (a variant of `fmap`) requires only second
argument to be `Functor`.  The kind annotation for `h` isn't strictly required,
but it's there to show that `h` is binary operations on types of kind  `* -> *`.
`hbimap` is a higher-order variant of `bimap`.

```haskell
class HBifunctor (h :: (* -> *) -> (* -> *) -> (* -> *)) where
    bfmap  :: Functor g => (a -> b) -> h f g a -> h f g b
    hbimap :: (f ~> f') -> (g ~> g') -> (h f g ~> h f' g')
```

Using `HBifunctor` we can defined `Functor` instance for `Free`.

```haskell
instance (HBifunctor h, Functor i) => Functor (Free h i f) where
    fmap f (Done x) = Done (fmap f x)
    fmap f (More x) = More (bfmap f x)
```

Some `HBifunctor`s are `Tensor`s.
Tensor laws resembles monoidal ones: `I h` is a unit functor
and `h` is associative up to isomorphism.

$$
\begin{aligned}
F \otimes 1 &\cong F \\
1 \otimes F &\cong F \\
(F \otimes G) \otimes H &\cong F \otimes (F \otimes H)
\end{aligned}
$$

See
[Wikipedia](https://en.wikipedia.org/wiki/Monoidal_category) or
[ncatlab](https://ncatlab.org/nlab/show/monoidal+category) for more discussion.

```haskell
class (HBifunctor h, Functor (I h)) => Tensor h where
    type I h :: * -> *

    intro1   :: f ~> h f (I h)
    intro2   :: Functor g => g ~> h (I h) g

    elim1    :: Functor f => h f (I h) ~> f
    elim2    :: Functor g => h (I h) g ~> g

    assoc    :: (Functor f, Functor g, Functor k)
             => h f (h g k) ~> h (h f g) k
    disassoc :: (Functor k)
             => h (h f g) k ~> h f (h g k)
```

Next we'll define two `Tensor`s: function composition `Comp` and Day convolution
`Day`.


Day convolution
---------------

Day convolution (named after Brian Day) is a product in the category of
endofunctors. We won't use [`kan-extensions`
definition](http://hackage.haskell.org/package/kan-extensions-5.1/docs/Data-Functor-Day.html)
but have slightly different one:

``` {.haskell .ignore}
-- used in this post
data Day f g a where
    (:<**>:) :: f x -> g (x -> a) -> Day f g a
```

It and `kan-extensions` definition are isomorphic, we can convert back and forth:

``` {.haskell .ignore}
-- from kan-extensions
data K.Day f g a where
   K.Day :: f b -> g c -> (b -> c -> a) -> Day f g a
```

```haskell
toKanExts :: Day f g a -> K.Day f g a
toKanExts (fx :<**>: gxa) = K.Day fx gxa (flip id)

fromKanExts :: Functor g => K.Day f g a -> Day f g a
fromKanExts (K.Day fx gy xya) = fx :<**>: fmap (flip xya) gy
```

Similarly to `Comp` (defined later), `Functor (Day f g)` requires `Functor g`,
but not `Functor f`.

```haskell
-- used in this post
instance Functor g => Functor (Day f g) where
    fmap h (fx :<**>: gxa) = fx :<**>: fmap (h .) gxa
```

`Day` is `HBifunctor`:

```haskell
instance HBifunctor Day where
    bfmap = fmap
    hbimap f g (fx :<**>: gxa) = f fx :<**>: g gxa
```

Next we define rest of the instances for `Day`: it's a `Tensor`,
`HApply` and `Happend`.

`Day` is a tensor, with `Identity` as a unit:

```haskell
instance Tensor Day where
    type I Day = Identity

    intro1 fx = fx :<**>: Identity id
    intro2 gx = Identity () :<**>: fmap const gx

    elim1 (fx :<**>: Identity xa) = fmap xa fx
    elim2 (Identity x :<**>: gxa) = fmap ($ x) gxa

    assoc (fx :<**>: (gy :<**>: kyxa)) =
        (fx :<**>: fmap (,) gy) :<**>: fmap uncurry kyxa

    disassoc ((fx :<**>: gxy) :<**>: kya) =
        fx :<**>: (gxy :<**>: fmap (.) kya)
```

`HApply` and `Happend` instances are interesting, they show why we chose
`:<**>:` name for the constructor. `HApply` needs `Applicative` constraint,
and we use `<**>` in `happly` implementation:

```haskell
instance HApply Day where
    type C Day = Applicative

    hpure _ = pure . runIdentity
    happly (fx :<**>: gxa) = fx <**> gxa
```

`HAppend` instance is straight forward. Note how `More` case
resembles the `disassoc` defined above.

```haskell
instance HAppend Day where
    happend (fx :<**>: gxa) = fx -<**>- gxa

(-<**>-) :: FreeA f x -> FreeA f (x -> a) -> FreeA f a
Done (Identity x)    -<**>- xa = fmap ($ x) xa
More (fz :<**>: gzx) -<**>- xa = More $
    fz :<**>: (gzx -<**>- fmap (.) xa)
```

One more point: `kan-extensions` `Day` has `liftA2` as the *"native"* operation
(you should think how `Free Day` "lists" would look like, if used this definition):

``` {.haskell .ignore}
instance HAppend K.Day where
    happend (K.Day fx gy xya) = liftA2' xya fx gy

liftA2' :: (a -> b -> c)
        -> Free K.Day Identity f a
        -> Free K.Day Identity f b
        -> Free K.Day Identity f c
liftA2' abc (Done (Identity a))      fb = fmap (abc a) fb
liftA2' abc (More (K.Day gx fy xya)) fb = More $ K.Day
    gx
    (liftA2' (,) fy fb)
    (\x (y, b) -> abc (xya x y) b)
```


Functor composition
-------------------

In previous section we defined own version of functor composition.

``` {.haskell .ignore}
-- used in this post
data Comp f g a where
    (:>>=:) :: f x -> (x -> g a) -> Comp f g a
```

That's a surprising choice, as Functor composition is usually defined as

``` {.haskell .ignore}
-- from base
newtype Compose f g a = Compose (f (g a))
```

We can convert between these definition quite freely:

```haskell
toBase :: Functor f => Comp f g a -> B.Compose f g a
toBase (fx :>>=: xga) = B.Compose (fmap xga fx)

fromBase :: B.Compose f g a -> Comp f g a
fromBase (B.Compose fga) = fga :>>=: id
```

The reason is that to make comparison with Day convolution more fair,
we'll drop the requirement for `Functor f`. We can do this by wrapping `f`
in (sliced in) `Coyoneda`.

So instead of

``` {.haskell .ignore}
-- base
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)
```

we have more relaxed context for `Functor (Comp f g)` instance:

```haskell
-- used in this post
instance Functor g => Functor (Comp f g) where
    fmap ab (fx :>>=: xga) = fx :>>=: fmap ab . xga
```

And because of relaxed context, we can defined `HBifunctor` instance:

```haskell
instance HBifunctor Comp where
    bfmap = fmap
    hbimap f g (fx :>>=: xga) = f fx :>>=: g . xga
```

Instead of usual `Free` monad (which requires `Functor f`), with our `Comp`
we will get a *freer* monad (which doesn't require `Functor f`). `Free` monad
adds `return` and `join` operations (you still need `fmap`), where naive
encoding of freer monad adds `return` and `bind`, thus doesn't requiring
anything from the `f`.

``` {.haskell .ignore}
-- Haskell98, written using GADTSyntax
data Free f a where
    Pure :: a -> Free f a
    Join :: f (Free f a) -> Free f a

-- Needs GADTs
data Freer f a where
    Pure :: a -> Freer f a
    Bind :: a -> (a -> Freer f b) -> Freer f b
```

The rest of this section is instance definitions.

`Comp` is a tensor with `Identity` as a unit:

```haskell
instance Tensor Comp where
    type I Comp = Identity

    intro1 fx = fx :>>=: Identity
    intro2 gx = Identity () :>>=: const gx

    elim1 (fx :>>=: xga) = fmap (runIdentity . xga) fx
    elim2 (fx :>>=: xga) = xga (runIdentity fx)

    disassoc ((fx :>>=: xgy) :>>=: yka) =
        fx :>>=: \x -> xgy x :>>=: yka

    assoc = error "hard"
```

The `HApply` requires a `Monad` as a constraint:

```haskell
instance HApply Comp where
    type C Comp = Monad

    hpure _ = return . runIdentity
    happly (fx :>>=: xga) = fx >>= xga
```

As in `Day` case, `Happend` instance for `Comp` shows why the constructor
is named `:>>=:` (Also if we would use `Compose` from `base`, the operation
would look like `join`).

```haskell
instance HAppend Comp where
    happend (x :>>=: f) = x ->>=- f

(->>=-) :: FreeM f a -> (a -> FreeM f b) -> FreeM f b
Done (Identity x)  ->>=- f = f x
More (x :>>=: g)   ->>=- f =
    More $ x :>>=: \a -> g a ->>=- f
```

The last thing to do is `FromDay` instance for `Comp`.
Because *Hask* has rich structure, we can convert `Day` into `Comp`.

```haskell
instance FromDay Comp where
    fromDay (fx :<**>: gxa) = fx :>>=: \x -> fmap ($ x) gxa
```

Compare that to

```haskell
flipAp :: Monad m => m a -> m (a -> b) -> m b
flipAp ma mab = ma >>= \a -> fmap ($ a) mab
```

it's virtually the same.

Bonus: Convert Applicative to Monad
-----------------------------

As we represent free applicatives and free monads using same
`Free`, it's quiet simple to map free `Applicative` to free `Monad`:

```haskell
convert :: FreeA f ~> FreeM f
convert = hhoistFree fromDay
```

where `hhoistFree` changes the tensor:

```haskell
hhoistFree :: forall h g i f. (HBifunctor h, HBifunctor g, Functor i)
           => (h ~~> g)
           -> Free h i f ~> Free g i f
hhoistFree nt = go where
    go :: forall x. Free h i f x -> Free g i f x
    go (Done x) = Done x
    go (More x) = More $ nt $ hbimap id go x
```



Bonus: Arrows
-------------

Let's look on tensors we have, They have similar structure:

``` {.haskell .ignore}
data Comp f g a where (:>>=:)  :: f x -> (x -> g a) -> Comp f g a
data Day  f g a where (:<**>:) :: f x -> g (x -> a) -> Day  f g a
```

The object $$\ \mathit{fga} : (F \otimes G) A $$ can be split into
$$fx : F X$$ and $$xga : X \overset{\otimes G}{\Longrightarrow} A$$ for some $$X$$.

This can be encoded in Haskell as another type-class:

```haskell
class Tensor h => Uncons h where
    type Arrow h :: (* -> *) -> * -> * -> *

    split   :: h f g a -> Combined (Arrow h) f g a
    combine :: f x -> Arrow h g x a -> h f g a

    (>>>) :: Arrow h (Free h (I h) f) a b
          -> Arrow h (Free h (I h) f) b c
          -> Arrow h (Free h (I h) f) a c

infixr 1 >>>
```

where `Combined` is a specialized dependent sum:

```haskell
data Combined (h' ::(* -> *) -> * -> * -> *) f g a where
    (:=>) :: f x -> h' g x a -> Combined h' f g a
```

We already spoiled a little: the reminder of "unconsing" `f x`, is an arrow!
And they can be composed with `>>>`. Using this machinery
we can define `happend'` differently, pushing the combination plumbing
into implementation of `>>>`:

```haskell
happend' :: Uncons h
         => Free h (I h) f `h` Free h (I h) f ~> Free h (I h) f
happend' t = case split t of
    Done i   :=> f -> elim2' (combine i f)
    More lhs :=> f -> case split lhs of
        x :=> g -> More $ combine x (g >>> f)
```

where `elim2'` is a specialised version of `elim2` to help out the type-checker:

```haskell
elim2' :: Tensor h => I h `h` Free h (I h) f  ~> Free h (I h) f
elim2' = elim2
```

Now we only need to show that `Comp` and `Day` are instances of `Uncons`:
The arrow for `Comp` is not so surprising, it's `Kleisli`:

```haskell
newtype Kleisli g x a = Kleisli (x -> g a)

instance Uncons Comp where
    type Arrow Comp = Kleisli

    split (fx :>>=: xga) = fx :=> Kleisli xga
    combine fx (Kleisli xga) = fx :>>=: xga

    Kleisli ab' >>> Kleisli bc' = Kleisli $ go ab' bc' where
        go :: (a -> FreeM f b) -> (b -> FreeM f c) -> (a -> FreeM f c)
        go ab bc a = case ab a of
            Done (Identity b)  -> bc b
            More (fx :>>=: xb) -> More $ fx :>>=: go xb bc
```

The arrow for `Day` is less known, so called `StaticArrow` (it's an `Arrow` if
`g` is an `Applicative`, `Kleisli` is an arrow if `g` is a `Monad`.
It has different names in different packages `arrows`: [`StaticArrow`](http://hackage.haskell.org/package/arrows-0.4.4.1/docs/Control-Arrow-Transformer-Static.html);
`semigroupoids`: [`Static`](http://hackage.haskell.org/package/semigroupoids-5.2.2/docs/Data-Semigroupoid-Static.html);
`profunctors`: [`Cayley`](http://hackage.haskell.org/package/profunctors-5.2.2/docs/Data-Profunctor-Cayley.html))

```haskell
newtype StaticArrow g x a = StaticArrow (g (x -> a))

instance Uncons Day where
    type Arrow Day = StaticArrow

    split (fx :<**>: gxa) = fx :=> StaticArrow gxa
    combine fx (StaticArrow gxa) = fx :<**>: gxa

    StaticArrow ab' >>> StaticArrow bc' = StaticArrow $ go ab' bc' where
        go :: FreeA f (a -> b) -> FreeA f (b -> c) -> FreeA f (a -> c)
        go (Done (Identity ab))   bc = fmap (. ab) bc
        go (More (fx :<**>: fxb)) bc =
            More $ fx :<**>: go fxb (fmap (.) bc)
```

I think we can define `Free` also using a
[`Path`](https://twitter.com/phadej/status/885236594910932992) of `Arrow`s and
a first element. I leave that as an extended exercise for a reader. :)

Bonus: Product
-------------------

We defined `Tensor` so we can have other unit than `Identity`. We can use this
now.  The `Product` (pair) as a tensor with `Proxy` as a unit.
The free monoid is a "list of `f a`".

Again, we don't use `base` `Product`, but a variant with embedded `Coyoneda`
for first element. The mapping function is put into a new data type.
This makes plumbing less tedious as `Arrow Prod = ProdK`:

```haskell
data Prod f g a where
    (:*:) :: f x -> ProdK g x a -> Prod f g a

data ProdK g a b = (a -> b) :> g b

infix 6 :*:
infix 7 :>
```

Fixity definitions are important, as we don't need to write as many parentheses.
We skip ahead to define `HApply`.  What's the right constraint?  `Alternative`
feels like a good fit, but it has restrictive context (`Applicative`), so let's
define a new class:

```haskell
class Functor f => Monoid1 f where
    unit1 :: f a
    (<!>) :: f a -> f a -> f a

infixl 3 <!>

instance HApply Prod where
    type C Prod = Monoid1

    hpure _ _ = unit1
    happly (fx :*: xa :> ga) = xa <$> fx <!> ga

instance (h ~ Prod, i ~ Proxy) => Monoid1 (Free h i f) where
    unit1 = Done Proxy
    f <!> x = happend (f :*: id :> x)
```

*Note:* `Monoid1` class is deliberately agnostic to "left catch" vs. "left distribution".
It's just a monoid.
(See  [`MonadPlus` on wiki.haskell.org](https://wiki.haskell.org/MonadPlus) 
or documentation for [`Alt`](http://hackage.haskell.org/package/semigroupoids-5.2.2/docs/Data-Functor-Alt.html)
class).

```haskell
instance HAppend Prod where
    happend (fx :*: xa :> gxa) = appendProd fx xa gxa

type FreeP = Free Prod Proxy

appendProd :: FreeP f x -> (x -> a) -> FreeP f a -> FreeP f a
appendProd (Done Proxy) _ fa = fa
appendProd (More (fy :*: yx :> fx)) xa fa = More $
    fy :*: xa . yx :> appendProd fx xa fa
```

The following example will use `retractFree` to extract the list of `f a`.

```haskell
newtype List1 f a = List1 { getList1 :: [f a] }

instance Functor f => Functor (List1 f) where
    fmap f (List1 xs) = List1 (fmap (fmap f) xs)

instance Functor f => Monoid1 (List1 f) where
    unit1 = List1 []
    List1 xs <!> List1 ys = List1 (xs ++ ys)

freePToList :: Functor f => FreeP f a -> [f a]
freePToList = getList1 . retractFree (\x -> List1 [x])
```

In theory, we could split `Free` into `FixH` and `FreeF`, and then make a
fixed point of a composition of `FreeF Day` and `FreeF Prod` to build a free
`Alternative` out of abstract non-senses. In practice you'd use [something
simpler](http://hackage.haskell.org/package/free-5/docs/Control-Alternative-Free.html)
or even completely custom thing (especially if you cannot have default `many`
or `some`).

The rest of the section are instances, for completeness. Note that
`I Prod = Proxy`. It's unusual use for `Proxy`!

```haskell
instance Functor g => Functor (ProdK g a) where
    fmap f (ab :> gb) = f . ab :> fmap f gb

instance Functor g => Functor (Prod f g) where
    fmap ab (fx :*: xaga) = fx :*: fmap ab xaga

instance HBifunctor Prod where
    bfmap = fmap
    hbimap f g (fx :*: xaga) = f fx :*: hoistProdK g xaga

hoistProdK :: (f ~> g) -> ProdK f a b -> ProdK g a b
hoistProdK nt (ab :> gb) = ab :> nt gb

instance Tensor Prod where
    type I Prod = Proxy

    intro1 fa = fa :*: id :> Proxy
    intro2 ga = Proxy :*: id :> ga

    elim1 (fx :*: xa :> _)  = fmap xa fx
    elim2 (_  :*: _  :> ga) = ga

    disassoc ((fx :*: xy :> gy) :*: ya :> ka) =
        fx :*: ya . xy :> (gy :*: ya :> ka)
    assoc (fx :*: xa :> (gy :*: ya :> ka)) =
        (fx :*: xa :> fmap ya gy) :*: id :> ka

instance Uncons Prod where
    type Arrow Prod = ProdK

    split (fx :*: xaga) = fx :=> xaga
    combine = (:*:)

    ab :> fb >>> bc' :> fc = bc' . ab :> go fb bc' fc where
        go :: FreeP f x -> (x -> y) -> FreeP f y -> FreeP f y
        go (Done Proxy)             _  gc = gc
        go (More (gx :*: xb :> gb)) bc gc =
            More $ gx :*: bc . xb :> go gb bc gc
```

Conslusion & remarks
---------------------

There is a lot of abstract nonsense in this post, hopefully you enjoyed
and gained some new insights! Drop me a line on
[Twitter](https://twitter.com/phadej/status/966273651787157505)
or
[Reddit](https://www.reddit.com/r/haskell/comments/7z4vwb/free_monad_and_free_applicative_using_single_free/).

[Bartows Milweski comments](https://twitter.com/BartoszMilewski/status/966295462033620992):
Strictly speaking Day convolution works on functors from a monoidal category C to Set. Don't have to be endofunctors. There is also a V-enriched version where Set is replaced by another monoidal category V.

---

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)
