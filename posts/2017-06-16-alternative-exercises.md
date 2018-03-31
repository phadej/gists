---
title: Alternative exercise
author: Oleg Grenrus
---

[*Haskell Programming from first principles* - aka `haskellbook`](http://haskellbook.com/)
"misses" a simple, yet hopefully insightful exercise about `Alternative` (introduced in Parser Combinators chapter).
I have discovered this one, while working on a real world project:

**Exercise** Write a function with following signature:

```haskell
count' :: Alternative f
       => Int  -- ^ minimum count
       -> Int  -- ^ maximum count
       -> f a
       -> f [a]
```

`f` could be a regular expression `RE` from [`regex-applicative` package](http://hackage.haskell.org/package/regex-applicative),
in that case `count' n m re` should work as `(re){n,m}` in a traditional regexp syntax.
In other words accept `n` to `m` occurrences of `re`.

```haskell
lambda> "a" =~ count' 2 3 (sym 'a')
Nothing

lambda> "aa" =~ count' 2 3 (sym 'a')
Just "aa"

lambda> "aaa" =~ count' 2 3 (sym 'a')
Just "aaa"

lambda> "aaaa" =~ count' 2 3 (sym 'a')
Nothing

lambda> "ab" =~ count' 2 3 (sym 'a')
Nothing
```


## Notes, hints and follow-ups

IF the exercise feels to hard to crack directly, there are few notes, hints and also
few follow-ups.

### Negative numbers

If `n < m`, e.g. `count' 2 1`, you may return `empty`, or accept exactly `2` values.
FWIW: JavaScript throws an exception: `Invalid regular expression: /^a{3,2}$/: numbers out of order in {}`, let's not do that.

Also in the following sub-exercises, treat negative numbers properly: write

```haskell
foo n | n < 0 = ... -- not n == 0, or foo 0
      | otherwise = ... foo (n - 1)
```

This will save you from the infinite recursion when negative numbers are passed
in.

### some and many

The definition of `many` and `some` in `base` library are slightly complicated:

```haskell
-- | One or more.
some :: f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (fmap (:) v) <*> many_v

-- | Zero or more.
many :: f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (fmap (:) v) <*> many_v
```

Write `many` as a one-liner, using `<$>`, `<*>` and `<|>`.

*Note:* `<$>` and `<*>` bind tighter than `<|>`, so you can write expressions
like `fun <$> with <*> parsers <|> foo <$> bar <*> quux`, which is grouped as
`(fun <$> with <*> parsers) <|> foo <$> bar <*> quux)`.

### Exact count

A small step towards the goal is to write simpler function first:

```haskell
count :: Alternative f
      => Int  -- ^ exact count
      -> f a
      -> f [a]
```

That should accept the exact amount of parses.
`count n re = count' n n re` ≈ `(re){n}` in traditional regex notation.

```haskell
lambda> "aaa" =~ count 3 (sym 'a')
Just "aaa"
```

### upto

Another variant with the same type-signature (!):

```haskell
upto :: Alternative f
     => Int  -- ^ up to this amount
     -> f a
     -> f [a]
```

This should accept *upto* some amount of parses.
`upto n re = count' 0 n re` ≈ `(re){n}` in traditional regex notation.

### Divide and conquer

Now, when you have defined `count` and `upto`,
you can define `count'` as simply as:

```haskell
count' n m p = (++) <$> count n p <*> upto (m - n) p
```

GHC is smart, but not super-smart. Let's help it.
Inline definition of `count`, removing the use of (++) from above definition.

### Order of alternatives

Virtually every parser combinator library is left-biased,
in other word the left hand side of `<|>` is preferred.
Explain what's the behavioral difference between

```haskell
pure [] <|> some p
some p <|> pure []
```

Analyze your `count'`, `count` and `upto` definitions, and try examples
above with your backtracking parser-combinator library of choice (e.g. `trifecta`).
Do they work as expected?

### Counting other Alternatives

There are also other `Alternative`s than parsers. For example `[]` and `STM`.
Explain to your colleague what `count'` does for `[]` and `STM`.
If there are other interesting `Alternative`s, where `count'` does something
neat, please tell me! (e.g. [via Twitter](https://twitter.com/phadej)).

## Solution

Fortunately,
[`parser-combinators`](http://hackage.haskell.org/package/parser-combinators)
package by Mark Karpov provides `count` and `count'`, so you could use them in
your code, and check *the source* for one way of solving this exercise.
