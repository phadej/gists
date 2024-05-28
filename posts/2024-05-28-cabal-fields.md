-------------------------------
title: cabal fields
author: Oleg Grenrus
-------------------------------

[`cabal-fields`](https://github.com/phadej/cabal-fields) is partly motivated by the [Migrate from the .cabal format to a widely supported format](https://github.com/haskell/cabal/issues/7548) issue.
Whether it's a solution or not, it's up to you to decide.

Envelope grammar vs. specific format grammar
--------------------------------------------

It is important to separate the envelope format (whether it's JSON, YAML, TOML, or cabal-like) from the actual file format (`package.json`, `stack.yaml`, `Cargo.toml`, or `pkg.cabal` for various package description formats).

An envelope format provides the common syntax.
Often it has special support for enumerations i.e. lists.
cabal-like format doesn't. All fields are just opaque text.
Depending on how you look at it, that's the good or bad thing.

Surely, specifying build dependencies like:

```yaml
dependencies:
  - base >= 4.13 && < 5
  - bytestring
```

makes the list structure clear for consumers.
However, e.g. [`hpack`](https://github.com/sol/hpack) doesn't use
list syntax uniformly: `ghc-options`, which is a list-typed field,
is still an opaque text field in `hpack` package description.

And individual package dependencies are also just opaque text fields,
there isn't even a split between a package name and the version range.

On the other hand, in cabal-like envelope there simply aren't 
any built-in "types": no lists, no numbers, no booleans.
As a actual file format designer you need to choose how to represent them,
allowing you to pick the format best suited for the domain.
For example, in `.cabal` files we don't need to write versions in quotes,
even if we have single digit versions!

For the purpose of automatic "exact-print" editing, it would be best
if envelope format supported as much of needed structure as possible
(e.g. there would be package name and version range split).
[For example in Cargo](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html)

```toml
[dependencies]
time = "0.1.12"
```

the separation is there.

OTOH, there is a gotcha:

<blockquote>
Leaving off the caret is a simplified equivalent syntax to using caret requirements. While caret requirements are the default, it is recommended to use the simplified syntax when possible.
</blockquote>

I'm quite sure that a lot of ad-hoc tools work only with simplified syntax.

Having simple envelope format is then probably the second best.
If some file-format specific parsing has to be written anyway
(e.g. to parse version ranges), dealing with a bit more complex stuff
(like lists in `.cabal` `build-depends`) shouldn't be considerably more effort.

Greibach lexing-parsing
---------------------------

In formal language theory, a context-free grammar is in [Greibach normal form (GNF)](https://en.wikipedia.org/wiki/Greibach_normal_form) if the right-hand sides of all production rules start with a terminal symbol, optionally followed by some variables:

```text
A → xBC
A → yDE
```

This suggests a representation for parsing procedure output, which looks like
token stream (can be lazily constructed and consumed), but does represent
a complete parse result, not just the result of lexing.

The idea is to have a continuation parameter for each production,
`A` may start with `X` (`A1` constructor) and then continue with `B`, which continues with `C` and then eventually with `k`.

```haskell
data A e k
  = A1 X (B e (C e k))
  | A2 Y (D e (E e k))
  | A_Err e
```

Additionally have an error constructor so the possible parse errors
are embedded somewhere later in t he stream. So there is `A = ... | A_Err`, `B = ... | B_Err` etc.

This may sound complicated, but it isn't.
For simple grammars, the tokens stream type isn't that complicated.
See for example `aeson`'s [`Tokens`](https://hackage.haskell.org/package/aeson-2.2.2.0/docs/Data-Aeson-Decoding-Tokens.html).
For JSON value, the `Tokens` looks almost like the [`Value`](https://hackage.haskell.org/package/aeson-2.2.2.0/docs/Data-Aeson-Types.html#t:Value) type,
but it does preserve more of the grammar. For example, the key order in maps is "as written", etc.
This is sometimes important distinction: do you want a syntax representation or a value representation.

A cabal-like envelope format is also a simple grammar,
which can be parsed into similar token stream.
In `cabal-fields` it looks like

```haskell
data AnnByteString ann = ABS ann {-# UNPACK #-} !ByteString
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Tokens ann k e
    = TkSection !(AnnByteString ann) !(AnnByteString ann) (Tokens ann (Tokens ann k e) e)
    | TkField !(AnnByteString ann) !ann (TkFieldLines ann (Tokens ann k e) e)
    | TkComment !(AnnByteString ann) (Tokens ann k e)
    | TkEnd k
    | TkErr e
  deriving (Show, Eq)

data TkFieldLines ann k e
    = TkFieldLine !(AnnByteString ann) (TkFieldLines ann k e)
    | TkFieldComment !(AnnByteString ann) (TkFieldLines ann k e)
    | TkFieldEnd k
    | TkFieldErr e
  deriving (Show, Eq, Functor, Foldable, Traversable)
```

compare this to [the `Field` type](https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Fields-Field.html#t:Field) in `Cabal-syntax`;
not considerably more complicated.

A benefit of Greibach-parsing is that it's relatively easy to write FFI-able parsers in C.
We don't need to create AST, we can have lexer-like interface, leaving the handling of AST creation
to the host language.
The parser implementation can be embedded as easily embedded into e.g. Haskell or Python.

Cabal and braces
----------------

I must admit I like cabal-like format a lot. It's simplicity and free-formness make it good fit for almost any kind of configuration.

But there is a feature that I very much dislike.

While `.cabal` files are perceived to have white-space layout,
there's actually a curly-braces option.
With curly-braces you can write the whole `.cabal` file on a single line!

If you look (but I don't recommend) [into grammar for cabal envelope, the handling of curly-braces](https://github.com/haskell/cabal/blob/88c81c92751c0beb36fb7c508ccf06c682a4f9a2/Cabal-syntax/src/Distribution/Fields/Parser.hs#L178),
and especially how it interacts with whitespace layout, you'll see some horrific stuff.

You can write

```text
test-suite hkd-example
  default-language: Haskell2010
  type: { exitcode-stdio-1.0 } main-is:HKD.hs 
  hs-source-dirs:   test
  build-depends:
      base
    , some
```

but I kindly ask you, please don't. :P

In short, for a feature used (or known) as little (but surprisingly a lot, 2.5% of Hackage; more than packages using tabs!), it adds quite a lot of complexity!
And I'd argue that the syntax is not natural. And if it wasn't there, it wouldn't be added today.

So for now, I simply don't support it. `Cabal-syntax` must support all the stuff and warts, `cabal-fields` doesn't.

Cabal and section arguments
---------------------------

Another gotcha in cabal envelope format is that while the field contents are opaque,
the section arguments (e.g. the `test-suite` name, or expression in `if`)
is actually lexed. It's non an opaque string, i.e. cannot be arbitrary.

The only case where it makes a difference i can think of top my head, is to allow end-of-line comments.
E.g. you can today write (and some did / do on Hackage):

```cabal
flag tagged -- toggle me
  default: True
  manual: True
```

but I wouldn't recommend.

This is *the only* case where you can have a comment on otherwise non-whitespace line.
E.g. if you write

```cabal
  build-depends: base <5 -- i feel lucky
```

that won't work, the `-- i feel lucky` will be considered as part of the field content.

Doing it so makes the envelope format simpler: there are no escaping on the envelope level. If you escape something in e.g. `description:` field, it's handled by only by haddock.
That avoids double escaping head-aches.

So `cabal-fields` treats section arguments as an opaque text.
If you have a end-of-line comment on that line, it will be included.

C interface
-----------

The `cabal-fields` library was first prototyped in Haskell and has safe interface.
However, C doesn't have sum types, nor polymorphic recursion nor many safety features at all.
So the C version looks an ordinary lexer interface would.
But there is a guarantee that only valid cabal-like files will be recognised,
so the token stream will be well-formed; or an error token will be returned.

Python interface
----------------

I tested the pure Haskell version against Haskell-using-C-FFI version.
They behave the same (against the most of Hackage).

The goal however was to parse `.cabal` files with Python.
People do complain that they cannot modify the `.cabal` files with Python.
Why I don't understand why you'd use Python, if you can use Haskell,
but not you "can" use Python too.

The [`cabalfields-demo.py`](https://github.com/phadej/cabal-fields/blob/4bcc7e02a116ef4fca912bb998f0d9fa41439c3a/python/cabalfields-demo.py) 
by default parsers and exact-prints the input files:

```text
% python3 cabalfields-demo.py ../cabal-fields/cabal-fields.cabal 
../cabal-fields/cabal-fields.cabal
??? same: True
cabal-version: 2.4
name:          cabal-fields
version:       0.1
synopsis:      An alternative parser for .cabal like files
description:   An alternative parser for @.cabal@ like files.
...
```

with intermediate types which look like:

```python
class Field:
    def __init__(self, name, name_pos, colon_pos, contents):
        self.name = name
        self.name_pos = name_pos
        self.colon_pos = colon_pos
        self.contents = contents

class Section:
    def __init__(self, name, name_pos, args, contents):
        self.name = name
        self.name_pos = name_pos
        self.args = args
        self.contents = contents

class FieldLine:
    def __init__(self, contents, pos):
        self.contents = contents
        self.pos = pos

class Comment:
    def __init__(self, contents, pos):
        self.contents = contents
        self.pos = pos
```

I haven't yet added any modification or consistency functionality.

It would be simpler to edit the structure if instead of absolute positions, there would be differences;
and the pretty-printer would check that differences are consistent (i.e. there are needed newlines, enough indentation etc).
That shouldn't be too difficult of an exercise to do.
It might be easier to do on Haskell version first (types do help).

Perfectly, the C library would also contain a builder.
But it needs a prototype first.

Also it's easier to write parsers in C, we don't need to think of memory allocation: the tokens returned are splices of the input byte array.
Inn printing we need to have some kind of a builder abstraction: we would like to have
an interface which can be used to produce both continuous strict byte-array for Python (using custom allocators),
but also lazy `ByteString` in Haskell.

Conclusion
----------

`cabal-fields` is a library for parsing `.cabal` like files.
It is using Greibach lexing-parsing approach.
It doesn't support curly braces, and slightly differs in how it handles section arguments.
There is also a C implementation.
With a Python module using it.
And small demo of exact-printing `.cabal` like files from Python.
