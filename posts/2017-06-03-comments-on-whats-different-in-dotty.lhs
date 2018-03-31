---
title: Comments on "What's Different In Dotty" by Martin Odersky
author: Oleg Grenrus
---

Today I watched a [Scala Days 2017 Keynote](https://www.youtube.com/watch?time_continue=3433&v=9lWrt6H6UdE):
"What's Different In Dotty" by Martin Odersky.
Even I write Haskell at work (lucky me! not only on free time), it's still
interesting to see how other languages evolve.

I think, all the
[DOT](https://infoscience.epfl.ch/record/215280) business is very nice, and
the [dotty](http://dotty.epfl.ch/) is a very good thing. Having sound
foundations for the language is a big deal.
*Note:* Do not mix up Scala's <emph>System D<sub>&lt;:</sup></emph> with Haskell's
[<emph>System DC</emph>](https://www.seas.upenn.edu/~sweirich/papers/systemd-submission.pdf). Dependent stuff everywhere! :)

Also the fact, that `dotc` (The dotty compiler) is implementing [MS Language Server Protocol](https://github.com/Microsoft/language-server-protocol) is a huge deal.
I hope that Alan and others will succeed in their [Haskell LSP](https://github.com/alanz/haskell-lsp) / [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) endeavor!

All so good, but the talk ends with an anticlimax mutable example (around [59:15](https://youtu.be/9lWrt6H6UdE?t=59m17s)):

```scala
def table(init: implicit Table => Unit) = {
  implicit val t = new Table
  init
  t
}

def row(init: implicit Row => Unit)(implicit t: Table) = {
  implicit val r = new Row
  init
  t.add(r) // :(
}

def cell(str: String)(implicit r: Row) =
  r.add(new Cell(str)) // :(
```

Why? Scala is *Object-Oriented Meets Functional*.

---

That DSL can be rewritten in Haskell, in purely functional way. First an
import and simple data definitions:

```haskell
module DifferentDotty where
import Control.Monad.Writer.CPS

newtype Cell  = Cell String deriving Show
newtype Row   = Row [Cell]  deriving Show
newtype Table = Table [Row] deriving Show
```

Using which we can write the DSL part: the magical `implicit` is replaced by
a spooky(?) `Writer` monad.

```haskell
table :: Writer [Row] () -> Table
table = Table . execWriter

row :: Writer [Cell] () -> Writer [Row] ()
row = tell . singleton . Row . execWriter

cell :: String -> Writer [Cell] ()
cell = tell . singleton . Cell

singleton :: a -> [a]
singleton x = [x]
```

And the usage looks like:

```haskell
t = table $ do
  row $ do
    cell "top left"
    cell "top right"
  row $ do
    cell "bottom left"
    cell "bottom right"
```

Compare that to the Scala variant. The two versions are essentially the same (but no mutation!).

```scala
table {
  row {
    cell("top left")
    cell("top right")
  }
  row {
    cell("bottom left")
    cell("bottom right")
  }
}
```

An exercise: translate the Haskell example to Scala and compare the three
versions.

*Note:* we use `WriterT` from [writer-cps-mtl package](https://hackage.haskell.org/package/writer-cps-mtl).
See [Gabriel Gonzalez e-mail from 2013](https://mail.haskell.org/pipermail/libraries/2013-March/019528.html) for details about why.
Probably should use [`DList`](http://hackage.haskell.org/package/dlist) too.

---

IMHO, it's sad that Martin doesn't seem to like monads. The example above is a
simple use of a simple monad, for greater good: referential transparency. [In
the presentation, right before the DSL
example](https://youtu.be/9lWrt6H6UdE?t=55m12s), he showed that implicits are
faster than a `Reader` monad.  But that's not true universally.  GHC optimizes
`Reader` to be as fast as the explicit argument passing. It would be great if
few Ph.D. dissertations will be written (and implemented) about how to make
`Reader` as fast as implicits on JVM platform. Working with monads can be made
syntactically pleasant too, for example see [Automatically Escaping
Monads](http://icfp16.sigplan.org/event/hiw-2016-papers-automatically-escaping-monads)
by Ben Lippmeier,
[video](https://www.youtube.com/watch?v=wG8AErq6Bbo&list=PLnqUlCo055hX1F0PCi9FjdllYQMwCQvps&index=6).
I'm sure that work would benefit all functional-style Scala developers,
probably also Kotlin and even Java.

---

You can run this file with
```
stack --resolver=nightly-2017-03-01 ghci --ghci-options='-pgmL markdown-unlit'
λ> :l DifferentDotty.lhs
```
fetch the source from
[https://gist.github.com/phadej/804e3e3ed33fabfb3038ed1177eafe96](https://gist.github.com/phadej/804e3e3ed33fabfb3038ed1177eafe96)
