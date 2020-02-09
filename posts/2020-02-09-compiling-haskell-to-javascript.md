---
title: Compiling Haskell to JavaScript, not in the way you'd expect
author: Oleg Grenrus
---

Let's start with a complete example right in the beginning.
We could write a Haskell code like:

```haskell
$(generateJS "code" [d|
   square :: Int -> Int
   square n = n * n

   poly :: Int -> Int -> Int
   poly x y = square x + 10 * y + 100

   res = poly 42 7
   |])
```

If we load the module into GHCi, we can try that we have the
functions defined:

```haskell
λ> square 5
25
λ> res
1934
```

But additionally there's a `code` value with JavaScript source code:

```haskell
λ> :t code
code :: [Char]  -- i.e. String
λ> putStr code
// square :: Int -> Int
function square (n) {
  return n * n;
}
// poly :: Int -> Int -> Int
function poly (x, y) {
  return square(x) + 10 * y + 100;
}
var res = poly(42, 7)
```

How does this work? Let's walk through in three steps.

<div id="toc"></div>

Step 1: Template Haskell splices
--------------------------------

The `[d| ... |]` is a syntax of Template Haskell *declaration quote*.
For example, `[d| answer = 42 |]` would produce a value of type
[`Q Dec`](https://hackage.haskell.org/package/template-haskell-2.15.0.0/docs/Language-Haskell-TH-Syntax.html#t:Dec),
which is an abstract syntax representation of it.

Relatedly, the `$(...)` is splice syntax. As it appears on the top level,
it splices declarations (of the type `Q [Dec]`, or expressions of the type `Q Exp` depending on the context) into the module.
This functionality is often used to define some boilerplate,
e.g. [`deriveGeneric`](https://hackage.haskell.org/package/generics-sop-0.5.0.0/docs/Generics-SOP-TH.html#v:deriveGeneric)
from `generics-sop` package to define instances,
or [`makeLenses`](https://hackage.haskell.org/package/lens-4.19/docs/Control-Lens-TH.html#v:makeLenses)
from `lens` to define `Lens` values. (Note that the `$(...)` wrapper
is not strictly required, but being explicit helps understand what happens).

We can write a first version of `generateJS`, which would
pass through the declarations given to it:

```haskell
generateJS1 :: String -> Q [Dec] -> Q [Dec]
generateJS1 _ decs = decs
```

We simply return the declarations given as argument. The result
of splicing `$(generateJS1 "somename" [d| some code ... |])` would
be the same as not wrapping that *some codeE* inside splice & quote.
Not very interesting yet, but it is a start.

Step 2: Inspecting quoted code
------------------------------

The [`Dec`](https://hackage.haskell.org/package/template-haskell-2.15.0.0/docs/Language-Haskell-TH-Syntax.html#t:Dec)
type is not opaque. We can look into the values, alter them,
or do something in addition.

`template-haskell` library also offers a pretty-printing
functionality in [`Language.Haskell.TH.Ppr`](https://hackage.haskell.org/package/template-haskell-2.15.0.0/docs/Language-Haskell-TH-Ppr.html) module,
most importantly [`pprint`](https://hackage.haskell.org/package/template-haskell-2.15.0.0/docs/Language-Haskell-TH-Ppr.html#v:pprint)
function which pretty prints various `template-haskell` types as Haskell code.
Let's play with it:

```haskell
generateJS2a :: String -> Q [Dec] -> Q [Dec]
generateJS2a _ decsQ = do
    decs <- decsQ
    liftIO $ putStr $ unlines $ map pprint decs
    return decs
```

In this snippet we `pprint` the declarations and `putStr` them.
When we (re)load the module in GHCi, something like

```haskell
square_0 :: GHC.Types.Int -> GHC.Types.Int
square_0 n_1 = n_1 GHC.Num.* n_1
```

will be printed in between other messages. There are few things
we want to improve:

- global names are fully qualified (`GHC.Num.*`)
- local names have *uniques* (i.e. `_1` suffix in `n_1`)
- printing to console is not nice for tooling

We can solve two first ones using
[`syb`](https://hackage.haskell.org/package/syb) - scrap your boilerplate.
All types in `template-haskell` have instances of [`Data`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Data.html#t:Data) type class, so using
`syb` is very easy way to do modifications:

```haskell
simplifyNames :: Data a => a ->  a
simplifyNames = SYB.everywhere (SYB.mkT simplifyName)

simplifyName :: Name -> Name
simplifyName (Name occ _) = Name occ NameS
```

We modify *all* `Name` values inside any `Data`, using `simplifyName`.
`simplifyName` then makes all names
[`NameS`](https://hackage.haskell.org/package/template-haskell-2.15.0.0/docs/Language-Haskell-TH-Syntax.html#t:NameFlavour) - *unqualified; dynamically bound*;
these names are pretty-printed without prefixes or suffixes. Exactly what we want.

To solve the third issue, let's generate the additional definition
containing the literal code. For that we'll need
to look into `template-haskell` library documentation to find
the names for combinators to write the code:

```haskell
stringDec
    :: String  -- ^ value name
    -> String  -- ^ value's value
    -> Q Dec
stringDec name' contents = do
    name <- newName name'
    valD (varP name) (normalB (stringE contents)) []
```

When looking at the `template-haskell` data structures, one can
learn few things about the Haskell surface syntax. One clear thing: it's huge.
Also that

```haskell
foo | bar       = 1
    | otherwise = 2
```

is a valid syntax, though not often used (at least on the top-level).
This is we use `normalB`, and not the expression directly:
the body of our value is not guarded, but it could be.

Using `simplifyNmaes` and `stringDec` we can define an improved version of
of `generateJSa`:

```haskell
generateJS2b :: String -> Q [Dec] -> Q [Dec]
generateJS2b name decsQ = do
    decs <- decsQ
    let decs' = map simplifyNames decs
    dec <- stringDec name $ unlines $ map pprint decs'
    return (dec : decs)
```

Now, when we reload in GHCi, the compiler is silent; yet there's new
value defined, `code`:

```
λ> putStr code
square :: Int -> Int
square n = n * n
poly :: Int -> Int -> Int
poly x y = (square x + (10 * y)) + 100
res = poly 42 7
```

Some extra parentheses are fine.

Step 3: Translating to JavaScript
---------------------------------

You probably guessed what we will do next. Instead of pretty-printing
Haskell as Haskell, we will pretty-print Haskell as JavaScript.
The `generateJS3` is essentially the same
as previously:

```haskell
generateJS3 :: String -> Q [Dec] -> Q [Dec]
generateJS3 name decsQ = do
    decs <- decsQ
    let decs1 = map simplifyNames decs
    let decs2 = map generateDec decs1
    dec <- stringDec name $ unlines $ map show decs2
    return (dec : decs)
```

but instead of `pprint` we use `generateDec`.
`generateDec` doesn't handle *all* of Haskell syntax,
only the bits we need for our example:

```haskell
generateDec :: Dec -> Doc
generateDec dec@SigD {}                = PP.text $ "// " ++ pprint dec
generateDec (FunD name clauses)        = generateFun name clauses
generateDec (ValD (VarP name) body []) = generateVal name body
generateDec dec                        =
    PP.text $ "// unhandled Dec: " ++ show dec
```

Type signatures (`SigD`) are put into comments,
function definitions (`FunD`: `fun a b = ...`) and value definitions (`ValD`: `val = ...`)
are handled by corresponding `generatefun` and `generateVal` functions.
Their definitions are in the appendix at the end of the blog post.
The last, catch all case, `show`s the Haskell value, so we can see
which cases are unhandled (if any).

As the result, if we once again reload the module in GHCi,
the `code` will have the value as in the introduction:

```haskell
λ> putStr code
// square :: Int -> Int
function square (n) {
  return n * n;
}
// poly :: Int -> Int -> Int
function poly (x, y) {
  return square(x) + 10 * y + 100;
}
var res = poly(42, 7)
```

Conclusion
----------

Is this silly? Well... kind of. Compiling Haskell to JavaScript in this way
won't work for all of Haskell, for example type-classes are not resolved, not
to mention lazy vs. strict semantics.

Yet this is an example demonstrating actual techniques used in the real world:

- For example IOHK Plutus uses the very same approach, see
  example in their tutorial: https://prod.playground.plutus.iohkdev.io/tutorial/#_writing_basic_plutustx_programs.
  They use Typed Template Haskell, but I think the idea is similar:
  Examine `template-haskell` values, generate corresponding for "your language".
  There might be some extra steps, as there often are in the compilers.

- I use the "capturing the Haskell code" part when working
  with my template library [`zinza`](https://hackage.haskell.org/package/zinza).
  `zinza` can compile its templates to a Haskell module,
  which will render the template. Using the `pprint` trick,
  I can write the input data definitions once,
  so they can be inserted into generated module, making it self-contained.
  (I have yet to write about this workflow in `zinza` documentation).

- In a similar spirit, one can capture the `Q [Dec]` output of e.g. `makeLenses`,
  pretty-print it into a file, and by using a CPP switch rather load
  a once generated Template Haskell splice from a file. For example
  on a systems where Template Haskell is rather non-available or
  just better avoidable in general.

   Obsidian Systems' `reflex-platform` used
  `-ddump-slices` and regular expression magic to achieve similar effect.
  That feels a lot more fragile. AFAIK currently they use a
  custom GHC which can dump and load Template Haskell splices.
  That way the loading is cleaner, then using CPP, but at the cost of custom patch.
  Yet, with a Source Plugin (yet if you don't have Template Haskell, you most likely don't have source plugins either) one could
  - augment all splices to dump their inputs on the "writer systems"
  - load the source of the splices on the "reader systems".

Appendix: generateFun and generateVal
-------------------------------------

```haskell
generateFun :: Name -> [Clause] -> Doc
generateFun name [Clause pats (NormalB body) decs]
    | vars <- mapMaybe isVar pats =
        PP.text "function"
            <+> ppName name
            <+> ppParensCommaSep (map (PP.text . pprint) vars)
            <+> PP.char '{'
        $$
        PP.nest 2 (PP.text "return" <+> generateExp body <> PP.char ';')
        $$
        PP.char '}'
  where
    isVar (VarP var) = Just var
    isVar _          = Nothing
generateFun name _ = error $ "difficult function: " ++ pprint name

generateVal :: Name -> Body -> Doc
generateVal name (NormalB body) =
    PP.text "var" <+> ppName name <+> PP.char '=' <+> generateExp body

ppParensCommaSep :: [Doc] -> Doc
ppParensCommaSep ds = PP.parens $ PP.hsep $ PP.punctuate PP.comma ds

nameStr :: Name -> String
nameStr (Name (OccName n) _) = n

ppName :: Name -> Doc
ppName = PP.text . nameStr

generateExp :: Exp -> Doc
generateExp (InfixE (Just l) (VarE op) (Just r))
    | nameStr op `elem` ["*", "+"]
    = generateExp l <+> ppName op <+> generateExp r
generateExp (VarE n) = ppName n
generateExp (AppE f x) =
    let (g, ys) = extractArguments f [x]
    in generateExp g <> ppParensCommaSep (map generateExp ys)
generateExp (LitE (IntegerL i)) = PP.integer i
generateExp exp
    = error $ "generateExp: difficult expression -- " ++ show exp

extractArguments :: Exp -> [Exp] -> (Exp, [Exp])
extractArguments (AppE f x) ys = extractArguments f (x : ys)
extractArguments f          ys = (f, ys)
```
