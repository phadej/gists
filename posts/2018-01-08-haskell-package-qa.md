---
title: New things in Haskell package QA
author: Oleg Grenrus
---

During the last month I have been working (not alone) on few Haskell package
QA related tools:

- [`cabal-plan`](http://hackage.haskell.org/package/cabal-plan)
- [`multi-ghc-travis`](https://github.com/haskell-hvr/multi-ghc-travis)
- [`trustee`](https://github.com/phadej/trustee)

In this posts I'll show what's new!

<div id="toc"></div>

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)

## cabal-plan

`cabal-plan` is a library and utility for processing cabal's new-build `plan.json` file.
Yesterday the 0.3.0.0 version was released, with cool (because I did them) additions to the executable part.

*Note:* these features are something which would be nice to have in `cabal-install` proper
eventually. For now it's easier to hack on them externally, though.


<h3>list-bin completer</h3>

The `list-bin` / `list-bins` completer is smart:

```
cabal-plan list-bin ca<TAB>
```

completes to

```
cabal-plan list-bin cabal-plan
```

The command is useful to find the location of locally built executables.
I did the completer work to to experiment with `optparse-applicative`'s
[`Completer`](https://hackage.haskell.org/package/optparse-applicative-0.14.0.0/docs/Options-Applicative-Builder-Completer.html) interface.

*Note:* to make it even better we need to modify `optparse-applicative` to give
already recognized options as the input to the completer.
With custom `--builddir` option, completer doesn't work, as it looks for
`plan.json` in the wrong place.

To enable completion in Bash add

```sh
eval "$(cabal-plan --bash-completion-script $(which cabal-plan))"
```

somewhere into your `.bashrc`.

If you use Zsh, it's a little more complicated in general (you have to decide a path where to put completion functions);
but if you use *oh-my-zsh* it's "decided" for you:

```zsh
cabal-plan --zsh-completion-script $(which cabal-plan) > $HOME/.oh-my-zsh/completions/_cabal-plan
```

*Note* this works for all executables using `optparse-applicative`, e.g. `trustee` too.

<h3>new command: topo</h3>

This command prints the topographic sort of the dependency graph.

<div style="padding-left: 1em">
<img src="../images/qa-10-topo-example.png" width="600" />
</div>

In the example above, I build `multi-ghc-travis` with tests disabled.
Otherwise the script (green components) depends only on boot libraries (blue),
but when built as a package it uses [ShellCheck](https://www.shellcheck.net/)
to make self-tests when generating the `.travis.yml` commands.

*Note:* ShellCheck has custom setup, and without `custom-setup` section
the implicit `setup-depends` bounds are used (in short: `Cabal < 2`).

<h3>changes to dot command</h3>

In 0.3.0.0 the `dot` command was reworked to output a **component** graph.
This is very useful, as you can differentiate between library and test dependencies.

To help see the difference, one small, but very visible change was made:
The graph is colorful, different types of local components (library,
executable, test-suite, etc...) and outbound edges have different colors.

Also Herbert nerd-snipped me to make a transitive-reduction "information preservation".
Without the reduction, the graph is often very messy, so you pipe the dot info
through `dot`. That however loses a bit of information. Therefore I taught
cabal-plan `--tred` and `--tred-weights` options. Latter ones *merges* reduced
paths into ones left, making them thicker. For example a part of
[`servant`](http://haskell-servant.readthedocs.io/en/stable/)
dependency graph:

<div style="padding-left: 1em">
<a href="http://oleg.fi/servant-deps-3.pdf"><img src="../images/qa-01-cabal-plan.png" /></a>
</div>

Shows that many things depending on `wai-extra` depend on its dependencies
too. The bold arrow going down on the bottom right of the picture is heading to `text`.

In the same picture you see the `--path-from wai-extra --path-to network` highlight.
That's useful to find a right part of the graph.


## multi-ghc-travis

The [`multi-ghc-travis`](https://github.com/haskell-hvr/multi-ghc-travis)
is a repository with `make-travis-yml` script
to generate Travis-CI's `.travis.yml` with multiple GHC configurations.

It uses `cabal-install-head` and various GHC versions in [HVR's PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc).

Lately there was a lot of changes.

Now you can now use a **config file**, so you don't need to supply all options as
command line arguments.  I hope that will be useful for maintainers of multiple
packages, as the config can be shared between different packages.

*Note:* everything (except constraint-sets) has command-line argument too, see `--help` output.

Second major addition is support of
**[head.hackage](https://github.com/hvr/head.hackage) overlay package index**
for GHC head (and GHC-8.4.1 which is currently in alpha). Generated script will
add `head.hackage` repository, and `--allow-newer` `base`, `template-haskell`,
`Cabal` and `ghc`, so you don't need to relax upper bounds too early.

Another big changes are: the script supports
**[cabal.project](http://cabal.readthedocs.io/en/latest/nix-local-build.html#configuring-builds-with-cabal-project)**
repositories i.e. repositories with multiple cabal packages,
(thanks to  Merijn Verstraaten), and **--osx** builds (thanks to [https://haskell.futurice.com/](https://haskell.futurice.com/)).

I also have to thank Ryan Scott for dogfooding the script, your feedback
is valuable, thank you.

Other changes can be introduced by going through the sample [`cabal.make-travis-yml`](https://github.com/haskell-hvr/multi-ghc-travis/blob/master/cabal.make-travis-yml).
First the config file part, then the explanation.

```sql
-- Cabal:GHC jobs
jobs: 2:2
```

Jobs let's you configure cabal's `-jN` and `--ghc-options=-jM` with single
`N:M` jobs. You can specify only other one too `N` or `:M`. At the moment,
I don't know which setting is the best. Also given different Travis-environments,
you have to find the one giving fastest builds for you.


```sql
-- travis Caching
cache: True
```

You can disable Travis caching. If your package lies very low in the dependency
graph and have cheap test dependencies (e.g. depends on boot-libraries mostly),
you'll end up caching trivial data only.

```sql
-- remove cabal noise from test output
cabal-noise: False
```

Addition by Merijn Verstraaten to filter out cabal-bits of output
from test sections. I.e. you will only see the output of the test-suites.

```sql
-- Run cabal check
cabal-check: True

-- Install dependencies in a separate step
-- If your project has inplace packages, you want to disable this.
install-dependencies-step: True

-- --no-tests --no-benchmarks build is useful to verify that package
-- builds when less constrained
no-tests-no-bench: True

-- --constraint='<boot-packages> installed' is useful to verivy that
-- there is a valid build-plan with GHC-bundled versions of libraries
build-with-installed-step: True
```

This are flags to disable some steps.
`install-dependencies-step` is a workaround for `cabal new-build --dep`
behaving badly when there is an in-place package.

```sql
-- Options for local packages
local-ghc-options: -Werror
```

With `local-ghc-options` the `package` sections are generated in `cabal.project`
for each of the local packages, with the given `ghc-options`.
This is a workaround to a missing feature in cabal-install
[#3883](https://github.com/haskell/cabal/issues/3883).

```sql
-- Build only these branches
branches: master
```

Builds only given branch. In my projects I build only `master` (and release) branches,
PRs are still built, as they are effectively a merge commits to `master`.
This way when I push a branch and make a PR, only a PR is built.

```sql
-- Enable IRC notifications to the given channel
name: fancy-name
irc-channels: irc.freenode.org#my-channel
```

If you want to have IRC notifications, you can!

```sql
-- Sections to fold in the build output
-- Possible values: all, all-but-test, sdist, unpack, build,
-- build-installed, build-everything, test, haddock, stackage,
-- check, doctest, hlint, constraint-sets
folds: constraint-sets
```

Another feature made by Merijn Verstraaten. Travis supports "folds",
and this options let's you fold parts of the build output.

See [https://travis-ci.org/merijn/broadcast-chan/jobs/325839627](https://travis-ci.org/merijn/broadcast-chan/jobs/325839627) for an example build
with `cabal-noise: False` and folds in use.

```sql
-- Run HLint (needs GHC-8.2.2 job)
hlint: True
hlint-yaml: .hlint.yaml
hlint-version: ==2.0.*
```

The script knows how to install and run `hlint`!

```sql
-- Run doctest (on GHC-8.0.2+ which support .ghc.environment)
doctest: True
doctest-options: --fast
doctest-version: ==0.13.*
```

Starting with version 8.0.2 GHC can read environment files to get package
dependencies (`.ghc.environment.*` files generated by `cabal new-build`).
We can use them to make lightweight `doctest` testing.
The problem is how to pass proper dependencies to `doctest`, environment
files solves this.
Cons: `doctest` testing for GHC-8.0.2+ only, Pros: your package can stay `build-type: Simple`.

```sql
-- Constraint sets
-- Package will be build with different constraints.
constraint-set deepseq-1.4
  ghc: (>= 7.8 && <7.10) || == 8.2.2
  constraints: deepseq ==1.4.*
```

Constraint sets let's you build package with constraints,
I took them into use already in
[`http-api-data`](https://github.com/fizruk/http-api-data/blob/c62306520f94084e6bc6be2fcb38ffad6e104bcb/cabal.make-travis-yml#L8-L38)
so the lower-bounds are verified by Travis-CI.

It's very easy to forget to bump lower-bound when taking into use new features
from dependencies. The next section about `trustee` illustrates that:

## trustee

Lately I got an access to multi core box, so I finally got time to make
a tool to automate (and parallelise) some Hackage Trustee tasks:
https://github.com/phadej/trustee So the tool is very new and has rough edges.
You have been warned.

I'll show the tool usage by going through a simple QA made to [`distributive`](http://hackage.haskell.org/package/distributive) package's master.

For some commands I mention how long it took to run them.
The numbers are lower then on the first run, because dependencies
are already cached, which isn't always true as the build-plans are non-orthodox (`cabal-install` solver won't usually pick such weird combinations).
That to point out that these checks aren't fast (= free).

First we check `distributive`'s lower bounds:

```
trustee bounds --lower
```

The result looked like this for me:

<div style="padding-left: 1em">
<img src="../images/qa-02-haskell-qa-bounds.png" width="750" />
</div>

Almost everything is green except:

- `ghc-prim` is blue *no-plan* because `contravariant` asks for `ghc-prim ==0.2`, version which isn't in the `01-index.tar.gz`. I should teach `trustee` tool to read `ghc-pkg dump`.
- `transformers` versions for 8.0.2 and 8.2.2 are in cyan, that's because the major version *0.5* doesn't match major lower-bound *0.2*.
   If the whole row would be cyan, it's a sign that we could bump the lower-bounds safely, or maybe even should as currently we cannot test the compatibility.

*Note:* there is also an `--upper` flag, useful to check that the upper bounds are "reachable".
Sometimes you relax the bound, but due transitive dependencies, you cannot yet test with it.

We can proceed with more expensive check, which actually tries to build these configurations:

```
trustee bounds --lower --verify
```

The command took *real 0m47.176s; user 6m1.984s; sys 1m0.908s*.
This might look weird, there's simple explanation. The command spent 47 seconds
["wall-clock time"](https://en.wikipedia.org/wiki/Elapsed_real_time), the actual time taken from start to the end,
and used 9 cores in average: *47sec * 9 ≈ 7min*. 7 minutes is the [CPU time](https://en.wikipedia.org/wiki/CPU_time), from
which 6 minutes are "user execution" (`user`), and one minute system time (`sys`,
kernel calls, e.g. accessing a file-system). My laptop has only 2 real cores (4 HT),
so it would take more time if run on it (not that easy, my laptop has less cores, but faster ones).

This result looks a bit worse, the numbers are the same, but there are few red spots:

<div style="padding-left: 1em">
<img src="../images/qa-03-haskell-qa-bounds-red.png" width="750"/>
</div>

To check what's wrong with `base-orphans-0.5.0` we can run a build on that row.
The `trustee new-build` commands builds with many GHC at once:

```
trustee new-build -- --constraint=base-orphans==0.5.0 --disable-tests --disable-benchmarks
```

We get a lot of output with the bottom looking like:

<div style="padding-left: 1em">
<img src="../images/qa-04-new-build.png" width="750" />
</div>

Looks like we need `Functor` instances for `GHC.Generics` types, but there aren't.
We can check from `base-orphans` [changelog](https://hackage.haskell.org/package/base-orphans-0.6/changelog) (Please: write changelogs!)
that instances are added in version 0.5.2. It's simple to verify the hypothesis:

```
trustee new-build -- --constraint=base-orphans==0.5.1 --disable-tests --disable-benchmarks
trustee new-build -- --constraint=base-orphans==0.5.2 --disable-tests --disable-benchmarks
```

The first command fails similarly, but the second one succeeds!

<div style="padding-left: 1em">
<img src="../images/qa-05-new-build-constraint.png" width="750"/>
</div>

The next step for is to check if the incorrect lower bound is also in released versions
of the package. If we check the [`distributive`'s changelog](https://hackage.haskell.org/package/distributive-0.5.3/changelog) (did I already asked, please, write changelogs!)
we see the *Add `Distributive` instances for datatypes from `Data.Semigroup` and `GHC.Generics`* entry for 0.5.1.
So the hypothesis is that older versions than 0.5.1 should compile with older base-orphans too.

For that we *get* few last releases of `distributive`. The command will
not only `cabal get` the package versions matching predicate (omit for all),
but also do `git init; git add .; git commit -m "trustee get"`, so we can track local changes.

```
trustee get distributive '>=0.5.0.2'
```

<div style="padding-left: 1em">
<img src="../images/qa-06-get.png" width="750" />
</div>

After that we can build a matrix, this takes some time:

```
trustee matrix distributive-*
```

<div style="padding-left: 1em">
<img src="../images/qa-07-matrix.png" width="750" />
</div>

The command took *real 1m54.321s; user 8m25.080s; sys 3m23.444s*.

The output resembles the https://matrix.hackage.haskell.org/package/distributive page:

<div style="padding-left: 1em">
<img src="../images/qa-08-real-matrix.png" width="600" />
</div>

Looks like there are no problems. However, we can rebuild the matrix
with `base-orphans<0.5.2` constraint:

```
trustee matrix distributive-* --constraint='base-orphans<0.5.2'
```

<div style="padding-left: 1em">
<img src="../images/qa-09-failing-matrix.png" width="750" />
</div>

*Note:* I'll soon teach `matrix` command [`--index-state`](http://cabal.readthedocs.io/en/latest/nix-local-build.html#cfg-field-index-state).
Then it will be possible to go back in time, by building against past states of Hackage index.

We can see that `0.5.0.2` version is not affected, which is good news.
To be sure we also check that `base-orphans==0.5.2` is first version
which is enough:

```
trustee matrix distributive-* --constraint='base-orphans==0.5.2'
```

The result is nice green matrix as the unrestricted build.

The next step is [to contact the maintainer and make necessary revisions](https://github.com/ekmett/distributive/pull/43).

## Conclusion

I showed you a few tools and what's new in them.
If you find them useful, please comment on
[Twitter](https://twitter.com/phadej/status/950330849752420352) or [Reddit](https://www.reddit.com/r/haskell/comments/7oxzjr/new_things_in_haskell_package_qa_tools_cabalplan/).

Thanks to Herbert for making `cabal-plan`, `multi-ghc-travis`, and 
https://matrix.hackage.haskell.org/ (which `trustee` mimics as a cli tool),
as well as commenting on this post.

