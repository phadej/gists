---
title: Formatting tabular data
author: Oleg Grenrus
tags: engineering
---

This weekend, I saw a tweet about formatting tabular data.
A complaint that it takes two hundred or so lines of C-code for something which is a oneliner.

Well, a solution (in Haskell) was a one-liner, *it fits in a tweet*
But in my opinion, it wasn't beautiful nor cool.

The solution below also fits in [a tweet](https://twitter.com/phadej/status/1122568763461767168):

```{.haskell .ignore}
tabular zs = unlines rs where
  (cs, ws, rs) = go zs
  go [] = (0, repeat 0, [])
  go (x : xs) =
    let (c, w, ys) = go xs
    in (max c (length x), zipWith max w (map length x ++ repeat 0),
        unwords (take cs (zipWith fl x ws)) : ys)
  fl s n = s ++ replicate (n - length s) ' '
```

**EDIT 2018-05-12:** Joseph C. Sible kindly pointed out that we can make it
even more concise using `foldr` (258 characters):

```{.haskell .ignore}
tabular zs = unlines rs where
  (cs, ws, rs) = foldr go (0, repeat 0, []) zs
  go x (c, w, ys) =
    (max c (length x), zipWith max w (map length x ++ repeat 0),
     unwords (take cs (zipWith fl x ws)) : ys)
  fl s n = s ++ replicate (n - length s) ' '
```

It's also beyond mortal human comprehension.
And not only because all symbol names are short.
It's special for another reason too.
It cannot be re-written as is, in say JavaScript or Scala, as it relies on **laziness**.

It's **cool**.

Not because it uses laziness.

Because, thanks to laziness, we make **only a single pass** through the table rows.

During that pass we calculate the number of columns, maximum column widths,
and *simultaneously* (!!!) take the final amount of columns padded to their final widths!

When you know what happens, the code is hopefully more understandable; maybe even obvious.
You still need to convince yourself that constructed computational _graph_ (not *tree*)
doesn't contain cycles and can be reduced into final result (i.e. won't loop).
(I had to write it down, and run it, to convince myself).

One could go further and make only single pass per table row,
but I'll leave that as an exercise for a reader. :)

It's crazy what Haskell does to you.
When you learn to use its superpowers,
there are no way back.

Finally, the less golfed version:

```haskell
module Table where

table :: [[String]] -> String
table cells = unlines rows
  where
    cols      :: Int
    rowWidths :: [Int]
    rows      :: [String]

    (cols, rowWidths, rows) = foldr go (0, repeat 0, []) cells

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go xs (c, w, yss) =
        ( max c (length xs)
        , zipWith max w (map length xs ++ repeat 0)
        , unwords (take cols (zipWith fill xs rowWidths))
          : yss
        )

    fill :: String -> Int -> String
    fill s n = s ++ replicate (n - length s) ' '
```

It works. Take an "unformatted" table of strings:

```haskell
ex1 :: String
ex1 = table
    [ [ "foo", "bar" ]
    , [ "x", "xyzzy", "foo" ]
    , [ "sik", "kik", "tik" ]
    ]
```

and format it into a nice table:

```
>>> putStr ex1
foo bar  
x   xyzzy foo
sik kik   tik
```
