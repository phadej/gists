-------------------------------
title: Modules, namespaces, compilation units, source files etc
author: Oleg Grenrus
-------------------------------

In this post I discuss a somewhat boring part of programming language design:
*Modules, namespaces, compilation units* and *source files*.
As these concepts are surprisingly easy to mix up, I'll try to define
what *I* mean by them.
I'll (not surprisingly) use Haskell as an example language,
highlightning what GHC Haskell does well and what less so.
In short, Haskell *modules* serve also as namespaces, compilation units, and have one-to-one mapping with files.
It's simple solution, but not without drawbacks. It also further mixes up the somewhat separate concepts.

Definitions
-----------

Let's begin with definitions.

**Namespaces** let us organise the names (of functions, types, and other entities of the language).
The simplest approach from language design perspective is to have only single flat namespace.
In that case people often use *ad hoc* namespacing e.g. affixing the names: `entityCreate`, `entityDestroy` etc.

This notion of namespace is not to be confused with Haskell having separate
(also) namespaces for variables and constructors (essentially) names starting
with lower or upper case letters.

We do use Haskell *modules* as namespaces. When we import modules `qualified`,
the names are prefixed for us:

```haskell
import qualified Data.Map as Map

foo = Map.insert key value xs
```

but Haskell module system is not very great namespacing system.

**Compilation unit** is a smallest piece of code compiler processes at once.

In Haskell, again, the smallest bit compiler processes at once is a Haskell module.
But it's also the largest bit!
More about this in latter sections where I discuss issues.

**Source file** is an actual "physical" file in the filesystem.
Many languages completely abstract source files from the language.
But e.g. in C we do `#include` actual header files.

In Haskell there is (almost) one-to-one mapping between module names and source file paths.

I consider **module** to be a unit in the dependency graph. This is the "module" as in "modular programming"[^modular-programming]

<blockquote>
Modular programming is a software design technique that emphasizes separating the functionality of a program into independent, interchangeable modules, such that each contains everything necessary to execute only one aspect or "concern" of the desired functionality.
</blockquote>

For example in C++[^cpp-modules]

<blockquote>
A module is a set of source code files that are compiled independently of the translation units.
</blockquote>

It's important to note that modules don't need to introduce own namespaces,
don't need to be single file, nor need to be single compilation unit!
They are a mean to organise the code.

Observations
------------

Type-classes are an anti-modular language construct.


Issues with Haskell
-------------------

**Packages**


**Namespacing for module names**

**Module system of Haskell is underdeveloped when considered as a namespace system.**.

**hs-boot files**

[^modular-programming]: https://en.wikipedia.org/wiki/Modular_programming

[^cpp-modules]: https://learn.microsoft.com/en-us/cpp/cpp/modules-cpp?view=msvc-170
