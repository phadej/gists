---
title: "overloaded-0.2.1: Overloaded:Unit"
author: Oleg Grenrus
tags: overloaded
---

The `Overloaded:Unit` is the third and the last of the new features of 
recent [overloaded 0.2.1](https://hackage.haskell.org/package/overloaded-0.2.1)
release. I wrote about 
[`Overloaded:Categories`](https://oleg.fi/gists/posts/2020-05-04-overloaded-categories.html),
[`Overloaded:Do`](https://oleg.fi/gists/posts/2020-04-27-overloaded-local-do.html)
previously.
`overloaded` package uses source plugins to reinterpret syntax in different ways.

`Overloaded:Unit` is (hopefully) clearly a tongue-in-cheek feature.
What we can do? We can overload what `()` means.

Without any overloading we have `() :: ()`.
With `{-# OPTIONS_GHC -fplugin=Overloaded #-}` we can make that symbol anything.

Boring
------

The most sensible overloading is into [boring](https://hackage.haskell.org/package/boring-0.1.3/docs/Data-Boring.html#v:boring).
You can specify that by 

```haskell
{-# OPTIONS_GHC -fplugin=Overloaded
                -fplugin-opt=Overloaded:Unit=Data.Boring.boring #-}
```

What is `boring`? Haddock tell us. There is also a law.

```haskell

-- | 'Boring' types which contains one thing, also
-- 'boring'. There is nothing interesting to be gained by
-- comparing one element of the boring type with another,
-- because there is nothing to learn about an element of the
-- boring type by giving it any of your attention.
--
-- /Boring Law:/
--
-- @
-- 'boring' == x
-- @
--
class Boring a where
    boring :: a
```

The above is a "proper implementation" of Conor McBride StackOverflow answer
to [What does () mean in Haskell -question](https://stackoverflow.com/questions/33112439/what-does-mean-in-haskell/33115522#33115522)

The answer starts with
<blockquote>
() means "Boring".
</blockquote>

So don't blame me for `Overloaded:Unit`, it wasn't my idea :)

Other possibilities
-------------------

The `overloaded` library by default makes `()` mean [`nil`](https://hackage.haskell.org/package/overloaded-0.2.1/docs/Overloaded-Lists.html#v:nil).
Lispers may agree with that choice.

Another choice is to make `()` mean [`def`](https://hackage.haskell.org/package/data-default-class-0.1.2.0/docs/Data-Default-Class.html#v:def)
from lawless [`Defaul`t](https://hackage.haskell.org/package/data-default-class-0.1.2.0/docs/Data-Default-Class.html#t:Default) type class.

As `def` is used as ad-hoc value for default options, we could write

```haskell
() & axis_line_style . line_width .~ 2
```

or

```haskell
() { _line_width = 2
   , _line_color = red
   }
```

Is that a good idea? Who am I to judge!

About syntax
------------

When you work with compiler internals, or more generally
into implementation of some specification, you often enough learn
stuff you didn't think about. Formalization makes corner cases more visible.

One example is:

```haskell
Prelude> (   )
()
```

Try in your GHCi prompt if you don't believe me.

This leads to a question: Why unit have been given special syntax in the first place?
Why could we had

```haskell
data Unit = Unit
```

Then there would been a StackOverflow question for Conor to answer!
And we could use `boring`, `def` or `Unit`. Those would be "overloadable"
with custom preludes, i.e. basic language feature: swapping imports.

We can think about `()` as 0-tuple. But the syntax is not consistent:

```haskell
()       -- 0-tuple i.e. unit
(x)      -- just x i.e. NOT 1-tuple
(x,y)    -- 2-tuple i.e. pair
(x,y,z)  -- 3-tuple i.e. triple
```

Yet, there is 1-tuple in GHC. Kind-of.

```haskell
Prelude Language.Haskell.TH> let x = 'x'
x :: Char

Prelude Language.Haskell.TH> print $( conE (tupleDataName 1) `appE` [| x |] )

<interactive>:12:7: error:
    • Exception when trying to run compile-time code:
        tupleDataName 1
CallStack (from HasCallStack):
  error, called at libraries/template-haskell/Language/Haskell/TH/Syntax.hs:1236:19 in template-haskell:Language.Haskell.TH.Syntax
      Code: conE (tupleDataName 1) `appE` [| x |]
    • In the untyped splice: $(conE (tupleDataName 1) `appE` [| x |])
```

I haven't tried to that using GHC internals, whether one will generate panics by creating 1-tuples (i.e. boxes), or whether it will "just work".

```haskell
data Box a = Box a
```

is valid data-type - and sometimes even useful.
We cannot use 1-tuple for it - because there isn't syntax for one,
yet we have `()` for 0-tuple.
Such inconsistencies.

We need *some syntax* for tuples however. The parenthesis delimit
where tuple starts and ends. The `(1, 'x', "foo")`
is clearly a triple. Without parenthesis `1, 'x', "foo"` would be ambiguous,
that could be  either triple or nested pairs - three options!
One could use $\langle$ and $\rangle$ to delimit tuples
leaving `(` and `)` only for grouping. But using Unicode for something
as central as tuples is not a good idea. So overloading parenthesis
is probably the least bad option. But not great.

The very evil interview-question would be to ask to list all
the ways the dot is overloaded (or will be overloaded) in GHC Haskell.
The will be are Local Do and RecordDotSyntax proposals, but we have plenty
already. Poor dot, stretched for about everything.
**Hot take**: *Disallow `.` as an operator, making it a punctuation, like comma or semicolon.*
It feels it would be easier to come up with new function composition operator,
than try to disambiguate ever-increasing dot usage.

What is next?
-------------

`TupleSections`. I don't know how divided community is about these.
I think that `(left,)` and `(,right)` sections are somewhat ok, even I avoid them.
The `(,)` is kind-of an infix operator (which it isn't).

Triples and bigger tuples on the other hand...

```haskell
("foo",,,True) :: b -> c -> ([Char], b, c, Bool)
```

for me this looks like syntax error.

But on the other hand, if you are ok with that syntax,
it definitely should be overloadable for arbitrary data-types,
why stop with tuples.

```haskell
data Foo = Foo String Int Char Bool deriving (Generic, ...)

("foo",,,True) :: Char -> Bool -> Foo
```

Wouldn't that be awesome? Stay tuned for `Overloaded:TupleSections`.
