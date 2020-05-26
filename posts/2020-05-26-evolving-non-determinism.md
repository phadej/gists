---
title: Evolving non-determinism
author: Oleg Grenrus
---

While reading about sorting networks in general for
[my previous blog post](https://oleg.fi/gists/posts/2020-05-19-bitonic-sort.html),
I noticed *END* method being mentioned.

*Evolving non-determinism* is an optimization method. It was new for me.
(It doesn't have own wikipedia page!)
I have previously only really used [simulated
annealing](https://en.wikipedia.org/wiki/Simulated_annealing) for finding magic
constants for 32-bit `splitmix` variant.
While SA  was good fit for that problem, it's not easily applicable
for all problems. END is similarly "the way as nature works" method,
and also is also applicable for specific problems, such that:

- a solution consists of discrete *steps*, there are no gradient
  so Hill-Climbing is not applicable
- good first steps are important for the optimality of solution
- each *partial solution* can be extended into *complete solution*

In other words, the methods works well when finding *a solution* is easy,
but finding a rather good or optimal solution is *the problem*.
It doesn't work for constraint solving problems where finding *a solution*
is the problem in itself.

I decided to play with END. It was fun.
In this blog post I will show some code, and then some example problems with results.

The method is described in great detail in the tech report 
*Evolving Non-Determinism:
An Inventive and Efficient Tool
for Optimization and Discovery of Strategies*
by Hugues Juillé
http://demo.cs.brandeis.edu/papers/END_tech_report.pdf

The report is from 1995. Surely I can replicate it on late 201x hardware.
(Spoiler: not really, but close).

Luckily Haskell is great language for prototyping and modeling.
Let us use it to understand how END works.

First the `Problem` data type.

```haskell
data Problem metric solution = Problem
    { fitness           :: solution -> metric
    , maximumFitness    :: metric
    , completeSolution  :: SMGen -> solution -> solution
    , initialSolution   :: solution
    , commitment        :: Int -> solution -> solution
    , initialCommitment :: Int
    }
```

There are six fields:

1. `fitness` function takes a `solution` and returns some `metric`.
   The END will then try to find global maximum (you can use `Down` to search for minimums).

2. `maximumFitness` is one of termination criteria.
   If we find a solution with `fitness` greater or equal, we stop early.
   This allows to search for "good enough" results.

3. `completeSolution` takes a possibly partial solution and
   completes it using non-determinism provided by random number generator.

4. `initialSolution` is &mdash; as name implies &mdash; an initial solution.
   It doesn't necessarily needs to be empty if we are interested
   in solutions starting with some known prefix.
   (Will see later that "prefix" is an abstract notion).

5. `commitment` is a function taking a *commitment degree* and
   a solution and truncating that solution to a given degree.
   For example *the first N steps*.

6. `initialCommitment` is then the initial commitment degree.
   It should be the "length" of `initialSolution` plus one.

Then given these data algorithm will proceed as follows:

1. Generate the square grid with solutions
   starting from `initialSolution` using `completeSolution`.

2. Then for each cell on the grid, pick the best solution from a neighborhood
   based on `fitness` metric, and replace it with it.
   Thus best solutions will conquer the area.

3. Truncate solutions with `commitment` using current degree

4. Regenerate the solutions, and go to step 2.

The algorithm works well when the initial step(s) are important for the
good `fitness` outcome. I decided to parameterize the algorithm
parameters as type-level `Nat`s:

```haskell
data Config
    (threads      :: Nat)
    (gridside     :: Nat)
    (threshold    :: Nat)
    (eralength    :: Nat)
    (maxsteps     :: Nat)
    (neighborhood :: Type -> Type)
    = Config
```

- `threads` tells how many threads to use to run the algorithm
  (it's very parallel).
- `gridside` is the length of the side of the grid (easier for calculations)
- `maxsteps` is the absolute maximum of steps algorithm will perform
- `neighborhood` describes how we define it. Whether we use max or Manhattan metric,
  and with which radius.
- the remaining `eralength` and `threshold` are related to
  configuring when commitment degree is *increased*.

When solutions are compared with each other,
the better prefixes will start to dominate.
However always truncating to one single step won't produce good outcomes:
algorithm will start from almost the beginning.
To narrow down the solution space we will increate the commitment degree,
when the overall population is still diverse,
but there are emerging dominating species.
This way simple-1-step-prefix solutions become more complex 2-step-prefix solutions,
simple "species" evolve into more complex "subspecies".

The `eralength` parameter is fixed amount of steps after which
the commitment degree will be increased.
`threshold` parameter is for *disorder* metric of the population:
when population disorder goes below this threshold, we will increase the commitment degree.

Ramps and Disorder
------------------

The tech report doesn't describe how their disorder metric is calculated,
but I think I figured it out.

Calculate amount of different solution-prefixes in the adjacent cells
of the grid.
Tech report seems to count double, but it is irrelevant.
This reminds me of [Ising model](https://en.wikipedia.org/wiki/Ising_model)
where the *energy* of configuration is calculated similarly.
It's not an *entropy* but close enough for our purposes.

The figures below are generated using a very simple reference problem:
The amount of *borders* is the disorder measure.
It's minimum is zero when there is only single "species",
and the maximum is the double of grid size.
You can count the border around small islands of ones in
the last (step 50) population drawn. There are 14 + 20.

<blockquote>
Given numbers 0..9 arrange them so that there are maximum amount of
<em>ascents</em>. This is very trivial problem,
as the optimal answer is 0123456789 (and 987654321 is worst),
but it is hard for generic algorithms:
naive crossover is difficult, as the resulting solution
might be invalid. Also algorithm easily runs into finding
a sub optimal solution with a single descent like 1345890267
</blockquote>

The code explains the details. Solution type is `[Int]` and metric is `Int`
as.

```haskell
type Solution = [Int]
type Metric   = Int
```

The `Problem` fields are defined as follows.
Initial solution and commitment degree, and maximum fitness are trivial.

```haskell
initialSolution   = []    :: Solution
initialCommitment = 1     :: Int
maximumFitness    = maxN  :: Metric
```

Commitment is implemented by taking N elements from the end:

```haskell
commitment        :: Int -> Solution -> Solution
commitment n      = reverse . take n . reverse
```

This is because we are using `(:)` to append a value to the "end" of solution.
The fitness functions calculates the ascents (with `>`, because order is reversed).

```haskell
fitness          :: Solution -> Metric
fitness xs       = length $ filter (uncurry (>)) $ pairs xs

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
```

and the most interesting part is the completion of partial solutions.
We calculate the set of remaining elements and randomly pick one
adding it to the solution. (`bitmaskwithRejection64 n` function returns a random number `0 <= x < n`).

```haskell
completeSolution :: SM.SMGen -> [Int] -> [Int]
completeSolution g0 xs0 =
    go g0 (fullnodes `Set.difference` Set.fromList xs0)
  where
    go :: SM.SMGen -> Set.Set Int -> [Int]
    go g rest
        | Set.null rest = xs0
        | otherwise     =
            let (w, g') = SM.bitmaskWithRejection64
                          (fromIntegral (Set.size rest)) g
                x       = Set.elemAt (fromIntegral w) rest
            in x : go g' (Set.delete x rest)
```

The evolving non-determinism performs *very well*.
After 25 steps the population is dominated with solution
starting with 0 or 1. If we don't increase commitment decree
slowly the zeros will dominate the whole grid.

Determining what is the good disorder threshold for your problem
is tricky. I used 15-20% for the problems presented. At that
stage there is still some diversity left and that threshold
is usually achieved quickly.

**Step 2, Disorder 234 (45.7%)**

\begin{tikzpicture}
\draw (0.50,0.50) node {3};
\draw (0.50,1.00) node {3};
\draw (0.50,1.50) node {3};
\draw[thick] (0.75,1.25) -- (0.75,1.75);
\draw (0.50,2.00) node {3};
\draw[thick] (0.75,1.75) -- (0.75,2.25);
\draw (0.50,2.50) node {3};
\draw[thick] (0.25,2.75) -- (0.75,2.75);
\draw[thick] (0.75,2.25) -- (0.75,2.75);
\draw (0.50,3.00) node {2};
\draw[thick] (0.25,3.25) -- (0.75,3.25);
\draw[thick] (0.75,2.75) -- (0.75,3.25);
\draw (0.50,3.50) node {3};
\draw[thick] (0.25,3.75) -- (0.75,3.75);
\draw[thick] (0.75,3.25) -- (0.75,3.75);
\draw (0.50,4.00) node {1};
\draw[thick] (0.25,4.25) -- (0.75,4.25);
\draw[thick] (0.75,3.75) -- (0.75,4.25);
\draw (0.50,4.50) node {5};
\draw[thick] (0.75,4.25) -- (0.75,4.75);
\draw (0.50,5.00) node {5};
\draw (0.50,5.50) node {5};
\draw[thick] (0.75,5.25) -- (0.75,5.75);
\draw (0.50,6.00) node {5};
\draw[thick] (0.25,6.25) -- (0.75,6.25);
\draw[thick] (0.75,5.75) -- (0.75,6.25);
\draw (0.50,6.50) node {2};
\draw (0.50,7.00) node {2};
\draw[thick] (0.25,7.25) -- (0.75,7.25);
\draw (0.50,7.50) node {6};
\draw[thick] (0.75,7.25) -- (0.75,7.75);
\draw (0.50,8.00) node {6};
\draw[thick] (0.25,8.25) -- (0.75,8.25);
\draw (1.00,0.50) node {3};
\draw (1.00,1.00) node {3};
\draw[thick] (0.75,1.25) -- (1.25,1.25);
\draw (1.00,1.50) node {0};
\draw[thick] (0.75,1.75) -- (1.25,1.75);
\draw[thick] (1.25,1.25) -- (1.25,1.75);
\draw (1.00,2.00) node {1};
\draw (1.00,2.50) node {1};
\draw (1.00,3.00) node {1};
\draw (1.00,3.50) node {1};
\draw[thick] (0.75,3.75) -- (1.25,3.75);
\draw[thick] (1.25,3.25) -- (1.25,3.75);
\draw (1.00,4.00) node {4};
\draw[thick] (0.75,4.25) -- (1.25,4.25);
\draw (1.00,4.50) node {1};
\draw[thick] (0.75,4.75) -- (1.25,4.75);
\draw[thick] (1.25,4.25) -- (1.25,4.75);
\draw (1.00,5.00) node {5};
\draw[thick] (0.75,5.25) -- (1.25,5.25);
\draw[thick] (1.25,4.75) -- (1.25,5.25);
\draw (1.00,5.50) node {3};
\draw[thick] (0.75,5.75) -- (1.25,5.75);
\draw[thick] (1.25,5.25) -- (1.25,5.75);
\draw (1.00,6.00) node {2};
\draw (1.00,6.50) node {2};
\draw (1.00,7.00) node {2};
\draw (1.00,7.50) node {2};
\draw[thick] (0.75,7.75) -- (1.25,7.75);
\draw (1.00,8.00) node {6};
\draw[thick] (0.75,8.25) -- (1.25,8.25);
\draw[thick] (1.25,7.75) -- (1.25,8.25);
\draw (1.50,0.50) node {3};
\draw (1.50,1.00) node {3};
\draw (1.50,1.50) node {3};
\draw[thick] (1.25,1.75) -- (1.75,1.75);
\draw[thick] (1.75,1.25) -- (1.75,1.75);
\draw (1.50,2.00) node {1};
\draw (1.50,2.50) node {1};
\draw (1.50,3.00) node {1};
\draw[thick] (1.25,3.25) -- (1.75,3.25);
\draw[thick] (1.75,2.75) -- (1.75,3.25);
\draw (1.50,3.50) node {4};
\draw (1.50,4.00) node {4};
\draw (1.50,4.50) node {4};
\draw[thick] (1.25,4.75) -- (1.75,4.75);
\draw[thick] (1.75,4.25) -- (1.75,4.75);
\draw (1.50,5.00) node {1};
\draw[thick] (1.25,5.25) -- (1.75,5.25);
\draw (1.50,5.50) node {2};
\draw[thick] (1.75,5.25) -- (1.75,5.75);
\draw (1.50,6.00) node {2};
\draw (1.50,6.50) node {2};
\draw[thick] (1.75,6.25) -- (1.75,6.75);
\draw (1.50,7.00) node {2};
\draw (1.50,7.50) node {2};
\draw[thick] (1.25,7.75) -- (1.75,7.75);
\draw (1.50,8.00) node {3};
\draw[thick] (1.75,7.75) -- (1.75,8.25);
\draw (2.00,0.50) node {3};
\draw[thick] (2.25,0.25) -- (2.25,0.75);
\draw (2.00,1.00) node {3};
\draw[thick] (1.75,1.25) -- (2.25,1.25);
\draw[thick] (2.25,0.75) -- (2.25,1.25);
\draw (2.00,1.50) node {0};
\draw[thick] (1.75,1.75) -- (2.25,1.75);
\draw (2.00,2.00) node {1};
\draw[thick] (2.25,1.75) -- (2.25,2.25);
\draw (2.00,2.50) node {1};
\draw[thick] (1.75,2.75) -- (2.25,2.75);
\draw[thick] (2.25,2.25) -- (2.25,2.75);
\draw (2.00,3.00) node {4};
\draw[thick] (2.25,2.75) -- (2.25,3.25);
\draw (2.00,3.50) node {4};
\draw[thick] (2.25,3.25) -- (2.25,3.75);
\draw (2.00,4.00) node {4};
\draw[thick] (1.75,4.25) -- (2.25,4.25);
\draw[thick] (2.25,3.75) -- (2.25,4.25);
\draw (2.00,4.50) node {1};
\draw (2.00,5.00) node {1};
\draw[thick] (2.25,4.75) -- (2.25,5.25);
\draw (2.00,5.50) node {1};
\draw[thick] (1.75,5.75) -- (2.25,5.75);
\draw (2.00,6.00) node {2};
\draw[thick] (1.75,6.25) -- (2.25,6.25);
\draw[thick] (2.25,5.75) -- (2.25,6.25);
\draw (2.00,6.50) node {4};
\draw[thick] (1.75,6.75) -- (2.25,6.75);
\draw (2.00,7.00) node {2};
\draw[thick] (2.25,6.75) -- (2.25,7.25);
\draw (2.00,7.50) node {2};
\draw[thick] (2.25,7.25) -- (2.25,7.75);
\draw (2.00,8.00) node {2};
\draw[thick] (1.75,8.25) -- (2.25,8.25);
\draw[thick] (2.25,7.75) -- (2.25,8.25);
\draw (2.50,0.50) node {6};
\draw[thick] (2.25,0.75) -- (2.75,0.75);
\draw[thick] (2.75,0.25) -- (2.75,0.75);
\draw (2.50,1.00) node {5};
\draw[thick] (2.25,1.25) -- (2.75,1.25);
\draw (2.50,1.50) node {0};
\draw[thick] (2.25,1.75) -- (2.75,1.75);
\draw[thick] (2.75,1.25) -- (2.75,1.75);
\draw (2.50,2.00) node {2};
\draw (2.50,2.50) node {2};
\draw (2.50,3.00) node {2};
\draw (2.50,3.50) node {2};
\draw[thick] (2.25,3.75) -- (2.75,3.75);
\draw[thick] (2.75,3.25) -- (2.75,3.75);
\draw (2.50,4.00) node {1};
\draw (2.50,4.50) node {1};
\draw[thick] (2.25,4.75) -- (2.75,4.75);
\draw (2.50,5.00) node {7};
\draw[thick] (2.25,5.25) -- (2.75,5.25);
\draw (2.50,5.50) node {1};
\draw[thick] (2.25,5.75) -- (2.75,5.75);
\draw (2.50,6.00) node {4};
\draw[thick] (2.75,5.75) -- (2.75,6.25);
\draw (2.50,6.50) node {4};
\draw (2.50,7.00) node {4};
\draw[thick] (2.25,7.25) -- (2.75,7.25);
\draw[thick] (2.75,6.75) -- (2.75,7.25);
\draw (2.50,7.50) node {3};
\draw[thick] (2.25,7.75) -- (2.75,7.75);
\draw (2.50,8.00) node {6};
\draw[thick] (2.75,7.75) -- (2.75,8.25);
\draw (3.00,0.50) node {5};
\draw[thick] (3.25,0.25) -- (3.25,0.75);
\draw (3.00,1.00) node {5};
\draw[thick] (2.75,1.25) -- (3.25,1.25);
\draw (3.00,1.50) node {2};
\draw[thick] (3.25,1.25) -- (3.25,1.75);
\draw (3.00,2.00) node {2};
\draw (3.00,2.50) node {2};
\draw[thick] (3.25,2.25) -- (3.25,2.75);
\draw (3.00,3.00) node {2};
\draw[thick] (2.75,3.25) -- (3.25,3.25);
\draw[thick] (3.25,2.75) -- (3.25,3.25);
\draw (3.00,3.50) node {1};
\draw (3.00,4.00) node {1};
\draw (3.00,4.50) node {1};
\draw[thick] (2.75,4.75) -- (3.25,4.75);
\draw (3.00,5.00) node {7};
\draw[thick] (2.75,5.25) -- (3.25,5.25);
\draw (3.00,5.50) node {1};
\draw (3.00,6.00) node {1};
\draw[thick] (2.75,6.25) -- (3.25,6.25);
\draw (3.00,6.50) node {4};
\draw[thick] (2.75,6.75) -- (3.25,6.75);
\draw[thick] (3.25,6.25) -- (3.25,6.75);
\draw (3.00,7.00) node {3};
\draw (3.00,7.50) node {3};
\draw (3.00,8.00) node {3};
\draw[thick] (2.75,8.25) -- (3.25,8.25);
\draw (3.50,0.50) node {3};
\draw[thick] (3.25,0.75) -- (3.75,0.75);
\draw[thick] (3.75,0.25) -- (3.75,0.75);
\draw (3.50,1.00) node {5};
\draw (3.50,1.50) node {5};
\draw[thick] (3.25,1.75) -- (3.75,1.75);
\draw (3.50,2.00) node {2};
\draw[thick] (3.25,2.25) -- (3.75,2.25);
\draw[thick] (3.75,1.75) -- (3.75,2.25);
\draw (3.50,2.50) node {0};
\draw[thick] (3.25,2.75) -- (3.75,2.75);
\draw (3.50,3.00) node {1};
\draw[thick] (3.75,2.75) -- (3.75,3.25);
\draw (3.50,3.50) node {1};
\draw (3.50,4.00) node {1};
\draw (3.50,4.50) node {1};
\draw[thick] (3.25,4.75) -- (3.75,4.75);
\draw[thick] (3.75,4.25) -- (3.75,4.75);
\draw (3.50,5.00) node {7};
\draw[thick] (3.25,5.25) -- (3.75,5.25);
\draw[thick] (3.75,4.75) -- (3.75,5.25);
\draw (3.50,5.50) node {1};
\draw (3.50,6.00) node {1};
\draw[thick] (3.25,6.25) -- (3.75,6.25);
\draw[thick] (3.75,5.75) -- (3.75,6.25);
\draw (3.50,6.50) node {7};
\draw[thick] (3.25,6.75) -- (3.75,6.75);
\draw[thick] (3.75,6.25) -- (3.75,6.75);
\draw (3.50,7.00) node {3};
\draw[thick] (3.75,6.75) -- (3.75,7.25);
\draw (3.50,7.50) node {3};
\draw (3.50,8.00) node {3};
\draw[thick] (3.75,7.75) -- (3.75,8.25);
\draw (4.00,0.50) node {5};
\draw[thick] (4.25,0.25) -- (4.25,0.75);
\draw (4.00,1.00) node {5};
\draw[thick] (4.25,0.75) -- (4.25,1.25);
\draw (4.00,1.50) node {5};
\draw[thick] (3.75,1.75) -- (4.25,1.75);
\draw[thick] (4.25,1.25) -- (4.25,1.75);
\draw (4.00,2.00) node {0};
\draw[thick] (4.25,1.75) -- (4.25,2.25);
\draw (4.00,2.50) node {0};
\draw (4.00,3.00) node {0};
\draw[thick] (3.75,3.25) -- (4.25,3.25);
\draw[thick] (4.25,2.75) -- (4.25,3.25);
\draw (4.00,3.50) node {1};
\draw (4.00,4.00) node {1};
\draw[thick] (3.75,4.25) -- (4.25,4.25);
\draw[thick] (4.25,3.75) -- (4.25,4.25);
\draw (4.00,4.50) node {2};
\draw (4.00,5.00) node {2};
\draw[thick] (3.75,5.25) -- (4.25,5.25);
\draw (4.00,5.50) node {1};
\draw[thick] (3.75,5.75) -- (4.25,5.75);
\draw[thick] (4.25,5.25) -- (4.25,5.75);
\draw (4.00,6.00) node {2};
\draw[thick] (3.75,6.25) -- (4.25,6.25);
\draw (4.00,6.50) node {3};
\draw[thick] (3.75,6.75) -- (4.25,6.75);
\draw[thick] (4.25,6.25) -- (4.25,6.75);
\draw (4.00,7.00) node {0};
\draw[thick] (3.75,7.25) -- (4.25,7.25);
\draw (4.00,7.50) node {3};
\draw[thick] (3.75,7.75) -- (4.25,7.75);
\draw[thick] (4.25,7.25) -- (4.25,7.75);
\draw (4.00,8.00) node {9};
\draw[thick] (3.75,8.25) -- (4.25,8.25);
\draw[thick] (4.25,7.75) -- (4.25,8.25);
\draw (4.50,0.50) node {3};
\draw[thick] (4.75,0.25) -- (4.75,0.75);
\draw (4.50,1.00) node {3};
\draw[thick] (4.25,1.25) -- (4.75,1.25);
\draw[thick] (4.75,0.75) -- (4.75,1.25);
\draw (4.50,1.50) node {1};
\draw[thick] (4.25,1.75) -- (4.75,1.75);
\draw (4.50,2.00) node {4};
\draw[thick] (4.25,2.25) -- (4.75,2.25);
\draw[thick] (4.75,1.75) -- (4.75,2.25);
\draw (4.50,2.50) node {0};
\draw[thick] (4.25,2.75) -- (4.75,2.75);
\draw[thick] (4.75,2.25) -- (4.75,2.75);
\draw (4.50,3.00) node {4};
\draw[thick] (4.25,3.25) -- (4.75,3.25);
\draw[thick] (4.75,2.75) -- (4.75,3.25);
\draw (4.50,3.50) node {1};
\draw[thick] (4.25,3.75) -- (4.75,3.75);
\draw[thick] (4.75,3.25) -- (4.75,3.75);
\draw (4.50,4.00) node {2};
\draw[thick] (4.75,3.75) -- (4.75,4.25);
\draw (4.50,4.50) node {2};
\draw[thick] (4.75,4.25) -- (4.75,4.75);
\draw (4.50,5.00) node {2};
\draw[thick] (4.75,4.75) -- (4.75,5.25);
\draw (4.50,5.50) node {2};
\draw[thick] (4.75,5.25) -- (4.75,5.75);
\draw (4.50,6.00) node {2};
\draw[thick] (4.25,6.25) -- (4.75,6.25);
\draw[thick] (4.75,5.75) -- (4.75,6.25);
\draw (4.50,6.50) node {0};
\draw (4.50,7.00) node {0};
\draw[thick] (4.25,7.25) -- (4.75,7.25);
\draw[thick] (4.75,6.75) -- (4.75,7.25);
\draw (4.50,7.50) node {1};
\draw (4.50,8.00) node {1};
\draw[thick] (4.25,8.25) -- (4.75,8.25);
\draw (5.00,0.50) node {1};
\draw (5.00,1.00) node {1};
\draw (5.00,1.50) node {1};
\draw (5.00,2.00) node {1};
\draw[thick] (4.75,2.25) -- (5.25,2.25);
\draw[thick] (5.25,1.75) -- (5.25,2.25);
\draw (5.00,2.50) node {4};
\draw[thick] (4.75,2.75) -- (5.25,2.75);
\draw (5.00,3.00) node {2};
\draw[thick] (5.25,2.75) -- (5.25,3.25);
\draw (5.00,3.50) node {2};
\draw[thick] (4.75,3.75) -- (5.25,3.75);
\draw[thick] (5.25,3.25) -- (5.25,3.75);
\draw (5.00,4.00) node {1};
\draw[thick] (5.25,3.75) -- (5.25,4.25);
\draw (5.00,4.50) node {1};
\draw (5.00,5.00) node {1};
\draw (5.00,5.50) node {1};
\draw (5.00,6.00) node {1};
\draw[thick] (4.75,6.25) -- (5.25,6.25);
\draw[thick] (5.25,5.75) -- (5.25,6.25);
\draw (5.00,6.50) node {0};
\draw[thick] (4.75,6.75) -- (5.25,6.75);
\draw (5.00,7.00) node {1};
\draw (5.00,7.50) node {1};
\draw (5.00,8.00) node {1};
\draw (5.50,0.50) node {1};
\draw (5.50,1.00) node {1};
\draw (5.50,1.50) node {1};
\draw[thick] (5.25,1.75) -- (5.75,1.75);
\draw (5.50,2.00) node {4};
\draw (5.50,2.50) node {4};
\draw (5.50,3.00) node {4};
\draw[thick] (5.75,2.75) -- (5.75,3.25);
\draw (5.50,3.50) node {4};
\draw[thick] (5.25,3.75) -- (5.75,3.75);
\draw[thick] (5.75,3.25) -- (5.75,3.75);
\draw (5.50,4.00) node {0};
\draw[thick] (5.25,4.25) -- (5.75,4.25);
\draw (5.50,4.50) node {1};
\draw[thick] (5.75,4.25) -- (5.75,4.75);
\draw (5.50,5.00) node {1};
\draw[thick] (5.75,4.75) -- (5.75,5.25);
\draw (5.50,5.50) node {1};
\draw[thick] (5.25,5.75) -- (5.75,5.75);
\draw[thick] (5.75,5.25) -- (5.75,5.75);
\draw (5.50,6.00) node {0};
\draw[thick] (5.75,5.75) -- (5.75,6.25);
\draw (5.50,6.50) node {0};
\draw[thick] (5.25,6.75) -- (5.75,6.75);
\draw[thick] (5.75,6.25) -- (5.75,6.75);
\draw (5.50,7.00) node {1};
\draw (5.50,7.50) node {1};
\draw (5.50,8.00) node {1};
\draw (6.00,0.50) node {1};
\draw (6.00,1.00) node {1};
\draw (6.00,1.50) node {1};
\draw[thick] (5.75,1.75) -- (6.25,1.75);
\draw[thick] (6.25,1.25) -- (6.25,1.75);
\draw (6.00,2.00) node {4};
\draw (6.00,2.50) node {4};
\draw[thick] (5.75,2.75) -- (6.25,2.75);
\draw (6.00,3.00) node {0};
\draw (6.00,3.50) node {0};
\draw (6.00,4.00) node {0};
\draw[thick] (5.75,4.25) -- (6.25,4.25);
\draw (6.00,4.50) node {3};
\draw[thick] (6.25,4.25) -- (6.25,4.75);
\draw (6.00,5.00) node {3};
\draw[thick] (6.25,4.75) -- (6.25,5.25);
\draw (6.00,5.50) node {3};
\draw[thick] (6.25,5.25) -- (6.25,5.75);
\draw (6.00,6.00) node {3};
\draw (6.00,6.50) node {3};
\draw[thick] (5.75,6.75) -- (6.25,6.75);
\draw (6.00,7.00) node {1};
\draw[thick] (6.25,6.75) -- (6.25,7.25);
\draw (6.00,7.50) node {1};
\draw (6.00,8.00) node {1};
\draw (6.50,0.50) node {1};
\draw (6.50,1.00) node {1};
\draw[thick] (6.25,1.25) -- (6.75,1.25);
\draw[thick] (6.75,0.75) -- (6.75,1.25);
\draw (6.50,1.50) node {4};
\draw[thick] (6.75,1.25) -- (6.75,1.75);
\draw (6.50,2.00) node {4};
\draw[thick] (6.75,1.75) -- (6.75,2.25);
\draw (6.50,2.50) node {4};
\draw[thick] (6.25,2.75) -- (6.75,2.75);
\draw[thick] (6.75,2.25) -- (6.75,2.75);
\draw (6.50,3.00) node {0};
\draw (6.50,3.50) node {0};
\draw (6.50,4.00) node {0};
\draw[thick] (6.25,4.25) -- (6.75,4.25);
\draw (6.50,4.50) node {1};
\draw[thick] (6.75,4.25) -- (6.75,4.75);
\draw (6.50,5.00) node {1};
\draw (6.50,5.50) node {1};
\draw[thick] (6.25,5.75) -- (6.75,5.75);
\draw[thick] (6.75,5.25) -- (6.75,5.75);
\draw (6.50,6.00) node {3};
\draw (6.50,6.50) node {3};
\draw (6.50,7.00) node {3};
\draw[thick] (6.25,7.25) -- (6.75,7.25);
\draw (6.50,7.50) node {1};
\draw (6.50,8.00) node {1};
\draw (7.00,0.50) node {1};
\draw[thick] (6.75,0.75) -- (7.25,0.75);
\draw[thick] (7.25,0.25) -- (7.25,0.75);
\draw (7.00,1.00) node {6};
\draw[thick] (6.75,1.25) -- (7.25,1.25);
\draw[thick] (7.25,0.75) -- (7.25,1.25);
\draw (7.00,1.50) node {9};
\draw[thick] (6.75,1.75) -- (7.25,1.75);
\draw[thick] (7.25,1.25) -- (7.25,1.75);
\draw (7.00,2.00) node {2};
\draw[thick] (6.75,2.25) -- (7.25,2.25);
\draw (7.00,2.50) node {0};
\draw[thick] (7.25,2.25) -- (7.25,2.75);
\draw (7.00,3.00) node {0};
\draw (7.00,3.50) node {0};
\draw (7.00,4.00) node {0};
\draw[thick] (7.25,3.75) -- (7.25,4.25);
\draw (7.00,4.50) node {0};
\draw[thick] (6.75,4.75) -- (7.25,4.75);
\draw[thick] (7.25,4.25) -- (7.25,4.75);
\draw (7.00,5.00) node {1};
\draw[thick] (6.75,5.25) -- (7.25,5.25);
\draw[thick] (7.25,4.75) -- (7.25,5.25);
\draw (7.00,5.50) node {5};
\draw[thick] (6.75,5.75) -- (7.25,5.75);
\draw (7.00,6.00) node {3};
\draw (7.00,6.50) node {3};
\draw (7.00,7.00) node {3};
\draw[thick] (6.75,7.25) -- (7.25,7.25);
\draw (7.00,7.50) node {1};
\draw (7.00,8.00) node {1};
\draw (7.50,0.50) node {3};
\draw (7.50,1.00) node {3};
\draw[thick] (7.25,1.25) -- (7.75,1.25);
\draw (7.50,1.50) node {2};
\draw[thick] (7.75,1.25) -- (7.75,1.75);
\draw (7.50,2.00) node {2};
\draw[thick] (7.75,1.75) -- (7.75,2.25);
\draw (7.50,2.50) node {2};
\draw[thick] (7.25,2.75) -- (7.75,2.75);
\draw[thick] (7.75,2.25) -- (7.75,2.75);
\draw (7.50,3.00) node {0};
\draw[thick] (7.75,2.75) -- (7.75,3.25);
\draw (7.50,3.50) node {0};
\draw[thick] (7.25,3.75) -- (7.75,3.75);
\draw[thick] (7.75,3.25) -- (7.75,3.75);
\draw (7.50,4.00) node {4};
\draw[thick] (7.75,3.75) -- (7.75,4.25);
\draw (7.50,4.50) node {4};
\draw[thick] (7.25,4.75) -- (7.75,4.75);
\draw[thick] (7.75,4.25) -- (7.75,4.75);
\draw (7.50,5.00) node {5};
\draw (7.50,5.50) node {5};
\draw[thick] (7.25,5.75) -- (7.75,5.75);
\draw (7.50,6.00) node {3};
\draw[thick] (7.75,5.75) -- (7.75,6.25);
\draw (7.50,6.50) node {3};
\draw (7.50,7.00) node {3};
\draw[thick] (7.25,7.25) -- (7.75,7.25);
\draw[thick] (7.75,6.75) -- (7.75,7.25);
\draw (7.50,7.50) node {1};
\draw[thick] (7.75,7.25) -- (7.75,7.75);
\draw (7.50,8.00) node {1};
\draw[thick] (7.25,8.25) -- (7.75,8.25);
\draw (8.00,0.50) node {3};
\draw (8.00,1.00) node {3};
\draw (8.00,1.50) node {3};
\draw (8.00,2.00) node {3};
\draw (8.00,2.50) node {3};
\draw[thick] (7.75,2.75) -- (8.25,2.75);
\draw (8.00,3.00) node {7};
\draw[thick] (7.75,3.25) -- (8.25,3.25);
\draw[thick] (8.25,2.75) -- (8.25,3.25);
\draw (8.00,3.50) node {3};
\draw (8.00,4.00) node {3};
\draw[thick] (7.75,4.25) -- (8.25,4.25);
\draw[thick] (8.25,3.75) -- (8.25,4.25);
\draw (8.00,4.50) node {5};
\draw (8.00,5.00) node {5};
\draw (8.00,5.50) node {5};
\draw (8.00,6.00) node {5};
\draw[thick] (7.75,6.25) -- (8.25,6.25);
\draw (8.00,6.50) node {3};
\draw[thick] (7.75,6.75) -- (8.25,6.75);
\draw[thick] (8.25,6.25) -- (8.25,6.75);
\draw (8.00,7.00) node {2};
\draw[thick] (7.75,7.25) -- (8.25,7.25);
\draw (8.00,7.50) node {6};
\draw[thick] (7.75,7.75) -- (8.25,7.75);
\draw (8.00,8.00) node {1};
\draw[thick] (7.75,8.25) -- (8.25,8.25);
\draw[thick] (8.25,7.75) -- (8.25,8.25);
\end{tikzpicture}

**Step 5, Disorder 186 (36.3%)**

\begin{tikzpicture}
\draw (0.50,0.50) node {3};
\draw[thick] (0.75,0.25) -- (0.75,0.75);
\draw (0.50,1.00) node {3};
\draw (0.50,1.50) node {3};
\draw[thick] (0.25,1.75) -- (0.75,1.75);
\draw[thick] (0.75,1.25) -- (0.75,1.75);
\draw (0.50,2.00) node {1};
\draw[thick] (0.25,2.25) -- (0.75,2.25);
\draw (0.50,2.50) node {2};
\draw (0.50,3.00) node {2};
\draw[thick] (0.25,3.25) -- (0.75,3.25);
\draw (0.50,3.50) node {1};
\draw[thick] (0.75,3.25) -- (0.75,3.75);
\draw (0.50,4.00) node {1};
\draw (0.50,4.50) node {1};
\draw (0.50,5.00) node {1};
\draw (0.50,5.50) node {1};
\draw[thick] (0.25,5.75) -- (0.75,5.75);
\draw[thick] (0.75,5.25) -- (0.75,5.75);
\draw (0.50,6.00) node {2};
\draw (0.50,6.50) node {2};
\draw (0.50,7.00) node {2};
\draw[thick] (0.25,7.25) -- (0.75,7.25);
\draw (0.50,7.50) node {3};
\draw (0.50,8.00) node {3};
\draw (1.00,0.50) node {0};
\draw[thick] (0.75,0.75) -- (1.25,0.75);
\draw[thick] (1.25,0.25) -- (1.25,0.75);
\draw (1.00,1.00) node {3};
\draw[thick] (0.75,1.25) -- (1.25,1.25);
\draw (1.00,1.50) node {0};
\draw[thick] (0.75,1.75) -- (1.25,1.75);
\draw[thick] (1.25,1.25) -- (1.25,1.75);
\draw (1.00,2.00) node {1};
\draw[thick] (0.75,2.25) -- (1.25,2.25);
\draw (1.00,2.50) node {2};
\draw[thick] (1.25,2.25) -- (1.25,2.75);
\draw (1.00,3.00) node {2};
\draw[thick] (1.25,2.75) -- (1.25,3.25);
\draw (1.00,3.50) node {2};
\draw[thick] (0.75,3.75) -- (1.25,3.75);
\draw[thick] (1.25,3.25) -- (1.25,3.75);
\draw (1.00,4.00) node {1};
\draw (1.00,4.50) node {1};
\draw (1.00,5.00) node {1};
\draw[thick] (0.75,5.25) -- (1.25,5.25);
\draw (1.00,5.50) node {2};
\draw[thick] (1.25,5.25) -- (1.25,5.75);
\draw (1.00,6.00) node {2};
\draw (1.00,6.50) node {2};
\draw (1.00,7.00) node {2};
\draw[thick] (0.75,7.25) -- (1.25,7.25);
\draw[thick] (1.25,6.75) -- (1.25,7.25);
\draw (1.00,7.50) node {3};
\draw (1.00,8.00) node {3};
\draw[thick] (0.75,8.25) -- (1.25,8.25);
\draw (1.50,0.50) node {3};
\draw[thick] (1.75,0.25) -- (1.75,0.75);
\draw (1.50,1.00) node {3};
\draw[thick] (1.25,1.25) -- (1.75,1.25);
\draw (1.50,1.50) node {1};
\draw (1.50,2.00) node {1};
\draw (1.50,2.50) node {1};
\draw (1.50,3.00) node {1};
\draw[thick] (1.25,3.25) -- (1.75,3.25);
\draw (1.50,3.50) node {4};
\draw[thick] (1.25,3.75) -- (1.75,3.75);
\draw[thick] (1.75,3.25) -- (1.75,3.75);
\draw (1.50,4.00) node {1};
\draw[thick] (1.75,3.75) -- (1.75,4.25);
\draw (1.50,4.50) node {1};
\draw (1.50,5.00) node {1};
\draw (1.50,5.50) node {1};
\draw[thick] (1.25,5.75) -- (1.75,5.75);
\draw (1.50,6.00) node {2};
\draw[thick] (1.75,5.75) -- (1.75,6.25);
\draw (1.50,6.50) node {2};
\draw[thick] (1.25,6.75) -- (1.75,6.75);
\draw (1.50,7.00) node {3};
\draw[thick] (1.75,6.75) -- (1.75,7.25);
\draw (1.50,7.50) node {3};
\draw[thick] (1.75,7.25) -- (1.75,7.75);
\draw (1.50,8.00) node {3};
\draw[thick] (1.75,7.75) -- (1.75,8.25);
\draw (2.00,0.50) node {2};
\draw[thick] (1.75,0.75) -- (2.25,0.75);
\draw[thick] (2.25,0.25) -- (2.25,0.75);
\draw (2.00,1.00) node {3};
\draw[thick] (1.75,1.25) -- (2.25,1.25);
\draw[thick] (2.25,0.75) -- (2.25,1.25);
\draw (2.00,1.50) node {1};
\draw[thick] (2.25,1.25) -- (2.25,1.75);
\draw (2.00,2.00) node {1};
\draw[thick] (2.25,1.75) -- (2.25,2.25);
\draw (2.00,2.50) node {1};
\draw[thick] (2.25,2.25) -- (2.25,2.75);
\draw (2.00,3.00) node {1};
\draw (2.00,3.50) node {1};
\draw[thick] (1.75,3.75) -- (2.25,3.75);
\draw (2.00,4.00) node {4};
\draw[thick] (1.75,4.25) -- (2.25,4.25);
\draw[thick] (2.25,3.75) -- (2.25,4.25);
\draw (2.00,4.50) node {1};
\draw (2.00,5.00) node {1};
\draw (2.00,5.50) node {1};
\draw (2.00,6.00) node {1};
\draw[thick] (1.75,6.25) -- (2.25,6.25);
\draw (2.00,6.50) node {2};
\draw[thick] (2.25,6.25) -- (2.25,6.75);
\draw (2.00,7.00) node {2};
\draw (2.00,7.50) node {2};
\draw[thick] (2.25,7.25) -- (2.25,7.75);
\draw (2.00,8.00) node {2};
\draw (2.50,0.50) node {6};
\draw[thick] (2.25,0.75) -- (2.75,0.75);
\draw[thick] (2.75,0.25) -- (2.75,0.75);
\draw (2.50,1.00) node {0};
\draw[thick] (2.75,0.75) -- (2.75,1.25);
\draw (2.50,1.50) node {0};
\draw (2.50,2.00) node {0};
\draw (2.50,2.50) node {0};
\draw[thick] (2.25,2.75) -- (2.75,2.75);
\draw (2.50,3.00) node {1};
\draw (2.50,3.50) node {1};
\draw (2.50,4.00) node {1};
\draw (2.50,4.50) node {1};
\draw (2.50,5.00) node {1};
\draw (2.50,5.50) node {1};
\draw (2.50,6.00) node {1};
\draw (2.50,6.50) node {1};
\draw[thick] (2.25,6.75) -- (2.75,6.75);
\draw[thick] (2.75,6.25) -- (2.75,6.75);
\draw (2.50,7.00) node {2};
\draw[thick] (2.25,7.25) -- (2.75,7.25);
\draw[thick] (2.75,6.75) -- (2.75,7.25);
\draw (2.50,7.50) node {3};
\draw[thick] (2.25,7.75) -- (2.75,7.75);
\draw (2.50,8.00) node {2};
\draw[thick] (2.25,8.25) -- (2.75,8.25);
\draw[thick] (2.75,7.75) -- (2.75,8.25);
\draw (3.00,0.50) node {3};
\draw (3.00,1.00) node {3};
\draw[thick] (2.75,1.25) -- (3.25,1.25);
\draw[thick] (3.25,0.75) -- (3.25,1.25);
\draw (3.00,1.50) node {0};
\draw (3.00,2.00) node {0};
\draw (3.00,2.50) node {0};
\draw[thick] (2.75,2.75) -- (3.25,2.75);
\draw (3.00,3.00) node {1};
\draw (3.00,3.50) node {1};
\draw (3.00,4.00) node {1};
\draw (3.00,4.50) node {1};
\draw (3.00,5.00) node {1};
\draw[thick] (3.25,4.75) -- (3.25,5.25);
\draw (3.00,5.50) node {1};
\draw (3.00,6.00) node {1};
\draw[thick] (2.75,6.25) -- (3.25,6.25);
\draw (3.00,6.50) node {3};
\draw (3.00,7.00) node {3};
\draw (3.00,7.50) node {3};
\draw[thick] (3.25,7.25) -- (3.25,7.75);
\draw (3.00,8.00) node {3};
\draw (3.50,0.50) node {3};
\draw[thick] (3.25,0.75) -- (3.75,0.75);
\draw (3.50,1.00) node {0};
\draw (3.50,1.50) node {0};
\draw (3.50,2.00) node {0};
\draw (3.50,2.50) node {0};
\draw[thick] (3.25,2.75) -- (3.75,2.75);
\draw (3.50,3.00) node {1};
\draw[thick] (3.75,2.75) -- (3.75,3.25);
\draw (3.50,3.50) node {1};
\draw[thick] (3.75,3.25) -- (3.75,3.75);
\draw (3.50,4.00) node {1};
\draw (3.50,4.50) node {1};
\draw[thick] (3.25,4.75) -- (3.75,4.75);
\draw (3.50,5.00) node {7};
\draw[thick] (3.25,5.25) -- (3.75,5.25);
\draw[thick] (3.75,4.75) -- (3.75,5.25);
\draw (3.50,5.50) node {1};
\draw[thick] (3.75,5.25) -- (3.75,5.75);
\draw (3.50,6.00) node {1};
\draw[thick] (3.25,6.25) -- (3.75,6.25);
\draw[thick] (3.75,5.75) -- (3.75,6.25);
\draw (3.50,6.50) node {3};
\draw[thick] (3.75,6.25) -- (3.75,6.75);
\draw (3.50,7.00) node {3};
\draw[thick] (3.25,7.25) -- (3.75,7.25);
\draw (3.50,7.50) node {0};
\draw[thick] (3.25,7.75) -- (3.75,7.75);
\draw[thick] (3.75,7.25) -- (3.75,7.75);
\draw (3.50,8.00) node {3};
\draw (4.00,0.50) node {3};
\draw[thick] (3.75,0.75) -- (4.25,0.75);
\draw[thick] (4.25,0.25) -- (4.25,0.75);
\draw (4.00,1.00) node {0};
\draw[thick] (4.25,0.75) -- (4.25,1.25);
\draw (4.00,1.50) node {0};
\draw[thick] (4.25,1.25) -- (4.25,1.75);
\draw (4.00,2.00) node {0};
\draw (4.00,2.50) node {0};
\draw[thick] (3.75,2.75) -- (4.25,2.75);
\draw[thick] (4.25,2.25) -- (4.25,2.75);
\draw (4.00,3.00) node {4};
\draw[thick] (3.75,3.25) -- (4.25,3.25);
\draw (4.00,3.50) node {0};
\draw[thick] (3.75,3.75) -- (4.25,3.75);
\draw[thick] (4.25,3.25) -- (4.25,3.75);
\draw (4.00,4.00) node {1};
\draw[thick] (4.25,3.75) -- (4.25,4.25);
\draw (4.00,4.50) node {1};
\draw[thick] (4.25,4.25) -- (4.25,4.75);
\draw (4.00,5.00) node {1};
\draw[thick] (3.75,5.25) -- (4.25,5.25);
\draw[thick] (4.25,4.75) -- (4.25,5.25);
\draw (4.00,5.50) node {2};
\draw[thick] (3.75,5.75) -- (4.25,5.75);
\draw (4.00,6.00) node {3};
\draw[thick] (3.75,6.25) -- (4.25,6.25);
\draw[thick] (4.25,5.75) -- (4.25,6.25);
\draw (4.00,6.50) node {2};
\draw[thick] (3.75,6.75) -- (4.25,6.75);
\draw (4.00,7.00) node {3};
\draw[thick] (4.25,6.75) -- (4.25,7.25);
\draw (4.00,7.50) node {3};
\draw[thick] (4.25,7.25) -- (4.25,7.75);
\draw (4.00,8.00) node {3};
\draw (4.50,0.50) node {1};
\draw (4.50,1.00) node {1};
\draw (4.50,1.50) node {1};
\draw[thick] (4.25,1.75) -- (4.75,1.75);
\draw (4.50,2.00) node {0};
\draw[thick] (4.25,2.25) -- (4.75,2.25);
\draw[thick] (4.75,1.75) -- (4.75,2.25);
\draw (4.50,2.50) node {4};
\draw[thick] (4.75,2.25) -- (4.75,2.75);
\draw (4.50,3.00) node {4};
\draw (4.50,3.50) node {4};
\draw[thick] (4.25,3.75) -- (4.75,3.75);
\draw[thick] (4.75,3.25) -- (4.75,3.75);
\draw (4.50,4.00) node {2};
\draw[thick] (4.75,3.75) -- (4.75,4.25);
\draw (4.50,4.50) node {2};
\draw[thick] (4.75,4.25) -- (4.75,4.75);
\draw (4.50,5.00) node {2};
\draw (4.50,5.50) node {2};
\draw[thick] (4.25,5.75) -- (4.75,5.75);
\draw[thick] (4.75,5.25) -- (4.75,5.75);
\draw (4.50,6.00) node {1};
\draw[thick] (4.25,6.25) -- (4.75,6.25);
\draw (4.50,6.50) node {2};
\draw[thick] (4.75,6.25) -- (4.75,6.75);
\draw (4.50,7.00) node {2};
\draw[thick] (4.25,7.25) -- (4.75,7.25);
\draw[thick] (4.75,6.75) -- (4.75,7.25);
\draw (4.50,7.50) node {1};
\draw[thick] (4.25,7.75) -- (4.75,7.75);
\draw (4.50,8.00) node {3};
\draw[thick] (4.25,8.25) -- (4.75,8.25);
\draw[thick] (4.75,7.75) -- (4.75,8.25);
\draw (5.00,0.50) node {1};
\draw (5.00,1.00) node {1};
\draw (5.00,1.50) node {1};
\draw (5.00,2.00) node {1};
\draw (5.00,2.50) node {1};
\draw[thick] (4.75,2.75) -- (5.25,2.75);
\draw[thick] (5.25,2.25) -- (5.25,2.75);
\draw (5.00,3.00) node {4};
\draw[thick] (4.75,3.25) -- (5.25,3.25);
\draw[thick] (5.25,2.75) -- (5.25,3.25);
\draw (5.00,3.50) node {1};
\draw (5.00,4.00) node {1};
\draw (5.00,4.50) node {1};
\draw[thick] (4.75,4.75) -- (5.25,4.75);
\draw (5.00,5.00) node {2};
\draw[thick] (4.75,5.25) -- (5.25,5.25);
\draw[thick] (5.25,4.75) -- (5.25,5.25);
\draw (5.00,5.50) node {1};
\draw[thick] (5.25,5.25) -- (5.25,5.75);
\draw (5.00,6.00) node {1};
\draw (5.00,6.50) node {1};
\draw (5.00,7.00) node {1};
\draw (5.00,7.50) node {1};
\draw (5.00,8.00) node {1};
\draw (5.50,0.50) node {1};
\draw (5.50,1.00) node {1};
\draw (5.50,1.50) node {1};
\draw (5.50,2.00) node {1};
\draw[thick] (5.25,2.25) -- (5.75,2.25);
\draw[thick] (5.75,1.75) -- (5.75,2.25);
\draw (5.50,2.50) node {4};
\draw[thick] (5.25,2.75) -- (5.75,2.75);
\draw (5.50,3.00) node {1};
\draw[thick] (5.75,2.75) -- (5.75,3.25);
\draw (5.50,3.50) node {1};
\draw[thick] (5.75,3.25) -- (5.75,3.75);
\draw (5.50,4.00) node {1};
\draw[thick] (5.75,3.75) -- (5.75,4.25);
\draw (5.50,4.50) node {1};
\draw (5.50,5.00) node {1};
\draw[thick] (5.25,5.25) -- (5.75,5.25);
\draw[thick] (5.75,4.75) -- (5.75,5.25);
\draw (5.50,5.50) node {3};
\draw[thick] (5.25,5.75) -- (5.75,5.75);
\draw (5.50,6.00) node {1};
\draw (5.50,6.50) node {1};
\draw (5.50,7.00) node {1};
\draw (5.50,7.50) node {1};
\draw (5.50,8.00) node {1};
\draw (6.00,0.50) node {1};
\draw (6.00,1.00) node {1};
\draw[thick] (6.25,0.75) -- (6.25,1.25);
\draw (6.00,1.50) node {1};
\draw[thick] (5.75,1.75) -- (6.25,1.75);
\draw[thick] (6.25,1.25) -- (6.25,1.75);
\draw (6.00,2.00) node {4};
\draw[thick] (6.25,1.75) -- (6.25,2.25);
\draw (6.00,2.50) node {4};
\draw[thick] (5.75,2.75) -- (6.25,2.75);
\draw (6.00,3.00) node {0};
\draw (6.00,3.50) node {0};
\draw (6.00,4.00) node {0};
\draw[thick] (5.75,4.25) -- (6.25,4.25);
\draw (6.00,4.50) node {1};
\draw[thick] (5.75,4.75) -- (6.25,4.75);
\draw[thick] (6.25,4.25) -- (6.25,4.75);
\draw (6.00,5.00) node {3};
\draw (6.00,5.50) node {3};
\draw[thick] (5.75,5.75) -- (6.25,5.75);
\draw (6.00,6.00) node {1};
\draw[thick] (6.25,5.75) -- (6.25,6.25);
\draw (6.00,6.50) node {1};
\draw[thick] (6.25,6.25) -- (6.25,6.75);
\draw (6.00,7.00) node {1};
\draw (6.00,7.50) node {1};
\draw (6.00,8.00) node {1};
\draw (6.50,0.50) node {1};
\draw[thick] (6.25,0.75) -- (6.75,0.75);
\draw[thick] (6.75,0.25) -- (6.75,0.75);
\draw (6.50,1.00) node {2};
\draw (6.50,1.50) node {2};
\draw (6.50,2.00) node {2};
\draw[thick] (6.25,2.25) -- (6.75,2.25);
\draw (6.50,2.50) node {4};
\draw[thick] (6.25,2.75) -- (6.75,2.75);
\draw[thick] (6.75,2.25) -- (6.75,2.75);
\draw (6.50,3.00) node {0};
\draw (6.50,3.50) node {0};
\draw (6.50,4.00) node {0};
\draw (6.50,4.50) node {0};
\draw[thick] (6.25,4.75) -- (6.75,4.75);
\draw (6.50,5.00) node {3};
\draw[thick] (6.75,4.75) -- (6.75,5.25);
\draw (6.50,5.50) node {3};
\draw (6.50,6.00) node {3};
\draw (6.50,6.50) node {3};
\draw[thick] (6.25,6.75) -- (6.75,6.75);
\draw (6.50,7.00) node {1};
\draw (6.50,7.50) node {1};
\draw (6.50,8.00) node {1};
\draw (7.00,0.50) node {2};
\draw[thick] (7.25,0.25) -- (7.25,0.75);
\draw (7.00,1.00) node {2};
\draw (7.00,1.50) node {2};
\draw (7.00,2.00) node {2};
\draw (7.00,2.50) node {2};
\draw[thick] (6.75,2.75) -- (7.25,2.75);
\draw (7.00,3.00) node {0};
\draw[thick] (7.25,2.75) -- (7.25,3.25);
\draw (7.00,3.50) node {0};
\draw (7.00,4.00) node {0};
\draw (7.00,4.50) node {0};
\draw (7.00,5.00) node {0};
\draw[thick] (6.75,5.25) -- (7.25,5.25);
\draw[thick] (7.25,4.75) -- (7.25,5.25);
\draw (7.00,5.50) node {3};
\draw (7.00,6.00) node {3};
\draw (7.00,6.50) node {3};
\draw[thick] (6.75,6.75) -- (7.25,6.75);
\draw (7.00,7.00) node {1};
\draw (7.00,7.50) node {1};
\draw (7.00,8.00) node {1};
\draw[thick] (6.75,8.25) -- (7.25,8.25);
\draw (7.50,0.50) node {3};
\draw[thick] (7.25,0.75) -- (7.75,0.75);
\draw (7.50,1.00) node {2};
\draw[thick] (7.75,0.75) -- (7.75,1.25);
\draw (7.50,1.50) node {2};
\draw (7.50,2.00) node {2};
\draw (7.50,2.50) node {2};
\draw (7.50,3.00) node {2};
\draw[thick] (7.25,3.25) -- (7.75,3.25);
\draw[thick] (7.75,2.75) -- (7.75,3.25);
\draw (7.50,3.50) node {0};
\draw[thick] (7.75,3.25) -- (7.75,3.75);
\draw (7.50,4.00) node {0};
\draw[thick] (7.75,3.75) -- (7.75,4.25);
\draw (7.50,4.50) node {0};
\draw[thick] (7.25,4.75) -- (7.75,4.75);
\draw[thick] (7.75,4.25) -- (7.75,4.75);
\draw (7.50,5.00) node {1};
\draw[thick] (7.25,5.25) -- (7.75,5.25);
\draw (7.50,5.50) node {3};
\draw (7.50,6.00) node {3};
\draw (7.50,6.50) node {3};
\draw[thick] (7.25,6.75) -- (7.75,6.75);
\draw (7.50,7.00) node {1};
\draw[thick] (7.75,6.75) -- (7.75,7.25);
\draw (7.50,7.50) node {1};
\draw[thick] (7.75,7.25) -- (7.75,7.75);
\draw (7.50,8.00) node {1};
\draw[thick] (7.25,8.25) -- (7.75,8.25);
\draw[thick] (7.75,7.75) -- (7.75,8.25);
\draw (8.00,0.50) node {3};
\draw (8.00,1.00) node {3};
\draw[thick] (7.75,1.25) -- (8.25,1.25);
\draw (8.00,1.50) node {2};
\draw[thick] (8.25,1.25) -- (8.25,1.75);
\draw (8.00,2.00) node {2};
\draw[thick] (8.25,1.75) -- (8.25,2.25);
\draw (8.00,2.50) node {2};
\draw[thick] (7.75,2.75) -- (8.25,2.75);
\draw (8.00,3.00) node {3};
\draw[thick] (8.25,2.75) -- (8.25,3.25);
\draw (8.00,3.50) node {3};
\draw[thick] (7.75,3.75) -- (8.25,3.75);
\draw[thick] (8.25,3.25) -- (8.25,3.75);
\draw (8.00,4.00) node {1};
\draw (8.00,4.50) node {1};
\draw (8.00,5.00) node {1};
\draw[thick] (7.75,5.25) -- (8.25,5.25);
\draw (8.00,5.50) node {3};
\draw[thick] (8.25,5.25) -- (8.25,5.75);
\draw (8.00,6.00) node {3};
\draw[thick] (8.25,5.75) -- (8.25,6.25);
\draw (8.00,6.50) node {3};
\draw[thick] (7.75,6.75) -- (8.25,6.75);
\draw[thick] (8.25,6.25) -- (8.25,6.75);
\draw (8.00,7.00) node {2};
\draw[thick] (7.75,7.25) -- (8.25,7.25);
\draw (8.00,7.50) node {3};
\draw (8.00,8.00) node {3};
\end{tikzpicture}

**Step 25, Disorder 110 (21%)**

\begin{tikzpicture}
\draw (0.50,0.50) node {2};
\draw[thick] (0.25,0.75) -- (0.75,0.75);
\draw (0.50,1.00) node {1};
\draw[thick] (0.75,0.75) -- (0.75,1.25);
\draw (0.50,1.50) node {1};
\draw (0.50,2.00) node {1};
\draw (0.50,2.50) node {1};
\draw[thick] (0.25,2.75) -- (0.75,2.75);
\draw (0.50,3.00) node {0};
\draw[thick] (0.25,3.25) -- (0.75,3.25);
\draw[thick] (0.75,2.75) -- (0.75,3.25);
\draw (0.50,3.50) node {2};
\draw[thick] (0.25,3.75) -- (0.75,3.75);
\draw (0.50,4.00) node {0};
\draw[thick] (0.75,3.75) -- (0.75,4.25);
\draw (0.50,4.50) node {0};
\draw[thick] (0.75,4.25) -- (0.75,4.75);
\draw (0.50,5.00) node {0};
\draw[thick] (0.25,5.25) -- (0.75,5.25);
\draw[thick] (0.75,4.75) -- (0.75,5.25);
\draw (0.50,5.50) node {1};
\draw[thick] (0.25,5.75) -- (0.75,5.75);
\draw (0.50,6.00) node {0};
\draw[thick] (0.75,5.75) -- (0.75,6.25);
\draw (0.50,6.50) node {0};
\draw[thick] (0.25,6.75) -- (0.75,6.75);
\draw[thick] (0.75,6.25) -- (0.75,6.75);
\draw (0.50,7.00) node {1};
\draw[thick] (0.25,7.25) -- (0.75,7.25);
\draw (0.50,7.50) node {0};
\draw[thick] (0.75,7.25) -- (0.75,7.75);
\draw (0.50,8.00) node {0};
\draw[thick] (0.25,8.25) -- (0.75,8.25);
\draw[thick] (0.75,7.75) -- (0.75,8.25);
\draw (1.00,0.50) node {2};
\draw[thick] (1.25,0.25) -- (1.25,0.75);
\draw (1.00,1.00) node {2};
\draw[thick] (0.75,1.25) -- (1.25,1.25);
\draw[thick] (1.25,0.75) -- (1.25,1.25);
\draw (1.00,1.50) node {1};
\draw (1.00,2.00) node {1};
\draw (1.00,2.50) node {1};
\draw[thick] (0.75,2.75) -- (1.25,2.75);
\draw (1.00,3.00) node {2};
\draw[thick] (1.25,2.75) -- (1.25,3.25);
\draw (1.00,3.50) node {2};
\draw (1.00,4.00) node {2};
\draw[thick] (0.75,4.25) -- (1.25,4.25);
\draw[thick] (1.25,3.75) -- (1.25,4.25);
\draw (1.00,4.50) node {1};
\draw (1.00,5.00) node {1};
\draw (1.00,5.50) node {1};
\draw (1.00,6.00) node {1};
\draw (1.00,6.50) node {1};
\draw (1.00,7.00) node {1};
\draw (1.00,7.50) node {1};
\draw[thick] (0.75,7.75) -- (1.25,7.75);
\draw (1.00,8.00) node {2};
\draw[thick] (1.25,7.75) -- (1.25,8.25);
\draw (1.50,0.50) node {1};
\draw (1.50,1.00) node {1};
\draw (1.50,1.50) node {1};
\draw (1.50,2.00) node {1};
\draw (1.50,2.50) node {1};
\draw (1.50,3.00) node {1};
\draw[thick] (1.25,3.25) -- (1.75,3.25);
\draw (1.50,3.50) node {2};
\draw[thick] (1.25,3.75) -- (1.75,3.75);
\draw[thick] (1.75,3.25) -- (1.75,3.75);
\draw (1.50,4.00) node {1};
\draw (1.50,4.50) node {1};
\draw (1.50,5.00) node {1};
\draw (1.50,5.50) node {1};
\draw (1.50,6.00) node {1};
\draw (1.50,6.50) node {1};
\draw (1.50,7.00) node {1};
\draw (1.50,7.50) node {1};
\draw (1.50,8.00) node {1};
\draw (2.00,0.50) node {1};
\draw (2.00,1.00) node {1};
\draw (2.00,1.50) node {1};
\draw (2.00,2.00) node {1};
\draw (2.00,2.50) node {1};
\draw (2.00,3.00) node {1};
\draw (2.00,3.50) node {1};
\draw (2.00,4.00) node {1};
\draw (2.00,4.50) node {1};
\draw (2.00,5.00) node {1};
\draw (2.00,5.50) node {1};
\draw (2.00,6.00) node {1};
\draw (2.00,6.50) node {1};
\draw (2.00,7.00) node {1};
\draw (2.00,7.50) node {1};
\draw (2.00,8.00) node {1};
\draw[thick] (2.25,7.75) -- (2.25,8.25);
\draw (2.50,0.50) node {1};
\draw[thick] (2.75,0.25) -- (2.75,0.75);
\draw (2.50,1.00) node {1};
\draw (2.50,1.50) node {1};
\draw (2.50,2.00) node {1};
\draw (2.50,2.50) node {1};
\draw (2.50,3.00) node {1};
\draw (2.50,3.50) node {1};
\draw[thick] (2.75,3.25) -- (2.75,3.75);
\draw (2.50,4.00) node {1};
\draw (2.50,4.50) node {1};
\draw (2.50,5.00) node {1};
\draw (2.50,5.50) node {1};
\draw (2.50,6.00) node {1};
\draw (2.50,6.50) node {1};
\draw (2.50,7.00) node {1};
\draw (2.50,7.50) node {1};
\draw[thick] (2.25,7.75) -- (2.75,7.75);
\draw (2.50,8.00) node {0};
\draw[thick] (2.25,8.25) -- (2.75,8.25);
\draw[thick] (2.75,7.75) -- (2.75,8.25);
\draw (3.00,0.50) node {0};
\draw[thick] (2.75,0.75) -- (3.25,0.75);
\draw[thick] (3.25,0.25) -- (3.25,0.75);
\draw (3.00,1.00) node {1};
\draw[thick] (3.25,0.75) -- (3.25,1.25);
\draw (3.00,1.50) node {1};
\draw (3.00,2.00) node {1};
\draw (3.00,2.50) node {1};
\draw (3.00,3.00) node {1};
\draw[thick] (2.75,3.25) -- (3.25,3.25);
\draw[thick] (3.25,2.75) -- (3.25,3.25);
\draw (3.00,3.50) node {0};
\draw[thick] (2.75,3.75) -- (3.25,3.75);
\draw (3.00,4.00) node {1};
\draw (3.00,4.50) node {1};
\draw (3.00,5.00) node {1};
\draw (3.00,5.50) node {1};
\draw (3.00,6.00) node {1};
\draw (3.00,6.50) node {1};
\draw (3.00,7.00) node {1};
\draw (3.00,7.50) node {1};
\draw (3.00,8.00) node {1};
\draw[thick] (2.75,8.25) -- (3.25,8.25);
\draw (3.50,0.50) node {1};
\draw[thick] (3.25,0.75) -- (3.75,0.75);
\draw[thick] (3.75,0.25) -- (3.75,0.75);
\draw (3.50,1.00) node {0};
\draw[thick] (3.25,1.25) -- (3.75,1.25);
\draw (3.50,1.50) node {1};
\draw[thick] (3.75,1.25) -- (3.75,1.75);
\draw (3.50,2.00) node {1};
\draw[thick] (3.75,1.75) -- (3.75,2.25);
\draw (3.50,2.50) node {1};
\draw[thick] (3.25,2.75) -- (3.75,2.75);
\draw (3.50,3.00) node {0};
\draw[thick] (3.75,2.75) -- (3.75,3.25);
\draw (3.50,3.50) node {0};
\draw[thick] (3.25,3.75) -- (3.75,3.75);
\draw (3.50,4.00) node {1};
\draw (3.50,4.50) node {1};
\draw[thick] (3.75,4.25) -- (3.75,4.75);
\draw (3.50,5.00) node {1};
\draw (3.50,5.50) node {1};
\draw (3.50,6.00) node {1};
\draw (3.50,6.50) node {1};
\draw (3.50,7.00) node {1};
\draw (3.50,7.50) node {1};
\draw (3.50,8.00) node {1};
\draw (4.00,0.50) node {0};
\draw[thick] (4.25,0.25) -- (4.25,0.75);
\draw (4.00,1.00) node {0};
\draw[thick] (4.25,0.75) -- (4.25,1.25);
\draw (4.00,1.50) node {0};
\draw (4.00,2.00) node {0};
\draw[thick] (3.75,2.25) -- (4.25,2.25);
\draw (4.00,2.50) node {1};
\draw[thick] (4.25,2.25) -- (4.25,2.75);
\draw (4.00,3.00) node {1};
\draw[thick] (3.75,3.25) -- (4.25,3.25);
\draw[thick] (4.25,2.75) -- (4.25,3.25);
\draw (4.00,3.50) node {0};
\draw[thick] (3.75,3.75) -- (4.25,3.75);
\draw (4.00,4.00) node {1};
\draw[thick] (3.75,4.25) -- (4.25,4.25);
\draw[thick] (4.25,3.75) -- (4.25,4.25);
\draw (4.00,4.50) node {0};
\draw[thick] (3.75,4.75) -- (4.25,4.75);
\draw[thick] (4.25,4.25) -- (4.25,4.75);
\draw (4.00,5.00) node {1};
\draw (4.00,5.50) node {1};
\draw (4.00,6.00) node {1};
\draw (4.00,6.50) node {1};
\draw (4.00,7.00) node {1};
\draw (4.00,7.50) node {1};
\draw (4.00,8.00) node {1};
\draw[thick] (3.75,8.25) -- (4.25,8.25);
\draw (4.50,0.50) node {1};
\draw (4.50,1.00) node {1};
\draw[thick] (4.25,1.25) -- (4.75,1.25);
\draw (4.50,1.50) node {0};
\draw[thick] (4.75,1.25) -- (4.75,1.75);
\draw (4.50,2.00) node {0};
\draw[thick] (4.75,1.75) -- (4.75,2.25);
\draw (4.50,2.50) node {0};
\draw (4.50,3.00) node {0};
\draw (4.50,3.50) node {0};
\draw (4.50,4.00) node {0};
\draw[thick] (4.25,4.25) -- (4.75,4.25);
\draw (4.50,4.50) node {1};
\draw[thick] (4.75,4.25) -- (4.75,4.75);
\draw (4.50,5.00) node {1};
\draw[thick] (4.75,4.75) -- (4.75,5.25);
\draw (4.50,5.50) node {1};
\draw (4.50,6.00) node {1};
\draw (4.50,6.50) node {1};
\draw (4.50,7.00) node {1};
\draw (4.50,7.50) node {1};
\draw (4.50,8.00) node {1};
\draw (5.00,0.50) node {1};
\draw (5.00,1.00) node {1};
\draw (5.00,1.50) node {1};
\draw (5.00,2.00) node {1};
\draw[thick] (4.75,2.25) -- (5.25,2.25);
\draw[thick] (5.25,1.75) -- (5.25,2.25);
\draw (5.00,2.50) node {0};
\draw (5.00,3.00) node {0};
\draw (5.00,3.50) node {0};
\draw (5.00,4.00) node {0};
\draw (5.00,4.50) node {0};
\draw (5.00,5.00) node {0};
\draw[thick] (4.75,5.25) -- (5.25,5.25);
\draw (5.00,5.50) node {1};
\draw[thick] (5.25,5.25) -- (5.25,5.75);
\draw (5.00,6.00) node {1};
\draw (5.00,6.50) node {1};
\draw (5.00,7.00) node {1};
\draw (5.00,7.50) node {1};
\draw (5.00,8.00) node {1};
\draw (5.50,0.50) node {1};
\draw[thick] (5.75,0.25) -- (5.75,0.75);
\draw (5.50,1.00) node {1};
\draw[thick] (5.75,0.75) -- (5.75,1.25);
\draw (5.50,1.50) node {1};
\draw[thick] (5.25,1.75) -- (5.75,1.75);
\draw[thick] (5.75,1.25) -- (5.75,1.75);
\draw (5.50,2.00) node {0};
\draw (5.50,2.50) node {0};
\draw (5.50,3.00) node {0};
\draw (5.50,3.50) node {0};
\draw (5.50,4.00) node {0};
\draw (5.50,4.50) node {0};
\draw (5.50,5.00) node {0};
\draw (5.50,5.50) node {0};
\draw[thick] (5.25,5.75) -- (5.75,5.75);
\draw (5.50,6.00) node {1};
\draw[thick] (5.75,5.75) -- (5.75,6.25);
\draw (5.50,6.50) node {1};
\draw[thick] (5.75,6.25) -- (5.75,6.75);
\draw (5.50,7.00) node {1};
\draw (5.50,7.50) node {1};
\draw (5.50,8.00) node {1};
\draw (6.00,0.50) node {2};
\draw (6.00,1.00) node {2};
\draw[thick] (5.75,1.25) -- (6.25,1.25);
\draw (6.00,1.50) node {0};
\draw (6.00,2.00) node {0};
\draw (6.00,2.50) node {0};
\draw (6.00,3.00) node {0};
\draw (6.00,3.50) node {0};
\draw (6.00,4.00) node {0};
\draw (6.00,4.50) node {0};
\draw (6.00,5.00) node {0};
\draw (6.00,5.50) node {0};
\draw (6.00,6.00) node {0};
\draw (6.00,6.50) node {0};
\draw[thick] (5.75,6.75) -- (6.25,6.75);
\draw (6.00,7.00) node {1};
\draw[thick] (6.25,6.75) -- (6.25,7.25);
\draw (6.00,7.50) node {1};
\draw (6.00,8.00) node {1};
\draw[thick] (5.75,8.25) -- (6.25,8.25);
\draw (6.50,0.50) node {2};
\draw (6.50,1.00) node {2};
\draw[thick] (6.25,1.25) -- (6.75,1.25);
\draw[thick] (6.75,0.75) -- (6.75,1.25);
\draw (6.50,1.50) node {0};
\draw (6.50,2.00) node {0};
\draw (6.50,2.50) node {0};
\draw (6.50,3.00) node {0};
\draw (6.50,3.50) node {0};
\draw (6.50,4.00) node {0};
\draw (6.50,4.50) node {0};
\draw (6.50,5.00) node {0};
\draw (6.50,5.50) node {0};
\draw (6.50,6.00) node {0};
\draw (6.50,6.50) node {0};
\draw (6.50,7.00) node {0};
\draw[thick] (6.25,7.25) -- (6.75,7.25);
\draw[thick] (6.75,6.75) -- (6.75,7.25);
\draw (6.50,7.50) node {1};
\draw[thick] (6.75,7.25) -- (6.75,7.75);
\draw (6.50,8.00) node {1};
\draw[thick] (6.25,8.25) -- (6.75,8.25);
\draw[thick] (6.75,7.75) -- (6.75,8.25);
\draw (7.00,0.50) node {2};
\draw[thick] (6.75,0.75) -- (7.25,0.75);
\draw (7.00,1.00) node {0};
\draw[thick] (7.25,0.75) -- (7.25,1.25);
\draw (7.00,1.50) node {0};
\draw (7.00,2.00) node {0};
\draw (7.00,2.50) node {0};
\draw (7.00,3.00) node {0};
\draw (7.00,3.50) node {0};
\draw (7.00,4.00) node {0};
\draw (7.00,4.50) node {0};
\draw (7.00,5.00) node {0};
\draw (7.00,5.50) node {0};
\draw (7.00,6.00) node {0};
\draw (7.00,6.50) node {0};
\draw[thick] (6.75,6.75) -- (7.25,6.75);
\draw (7.00,7.00) node {1};
\draw[thick] (6.75,7.25) -- (7.25,7.25);
\draw[thick] (7.25,6.75) -- (7.25,7.25);
\draw (7.00,7.50) node {0};
\draw (7.00,8.00) node {0};
\draw[thick] (6.75,8.25) -- (7.25,8.25);
\draw (7.50,0.50) node {2};
\draw (7.50,1.00) node {2};
\draw[thick] (7.25,1.25) -- (7.75,1.25);
\draw (7.50,1.50) node {0};
\draw[thick] (7.75,1.25) -- (7.75,1.75);
\draw (7.50,2.00) node {0};
\draw (7.50,2.50) node {0};
\draw (7.50,3.00) node {0};
\draw (7.50,3.50) node {0};
\draw (7.50,4.00) node {0};
\draw (7.50,4.50) node {0};
\draw (7.50,5.00) node {0};
\draw (7.50,5.50) node {0};
\draw (7.50,6.00) node {0};
\draw (7.50,6.50) node {0};
\draw (7.50,7.00) node {0};
\draw (7.50,7.50) node {0};
\draw (7.50,8.00) node {0};
\draw[thick] (7.25,8.25) -- (7.75,8.25);
\draw (8.00,0.50) node {2};
\draw (8.00,1.00) node {2};
\draw[thick] (8.25,0.75) -- (8.25,1.25);
\draw (8.00,1.50) node {2};
\draw[thick] (7.75,1.75) -- (8.25,1.75);
\draw[thick] (8.25,1.25) -- (8.25,1.75);
\draw (8.00,2.00) node {0};
\draw[thick] (8.25,1.75) -- (8.25,2.25);
\draw (8.00,2.50) node {0};
\draw[thick] (8.25,2.25) -- (8.25,2.75);
\draw (8.00,3.00) node {0};
\draw (8.00,3.50) node {0};
\draw[thick] (8.25,3.25) -- (8.25,3.75);
\draw (8.00,4.00) node {0};
\draw (8.00,4.50) node {0};
\draw (8.00,5.00) node {0};
\draw (8.00,5.50) node {0};
\draw[thick] (8.25,5.25) -- (8.25,5.75);
\draw (8.00,6.00) node {0};
\draw (8.00,6.50) node {0};
\draw (8.00,7.00) node {0};
\draw[thick] (8.25,6.75) -- (8.25,7.25);
\draw (8.00,7.50) node {0};
\draw (8.00,8.00) node {0};
\draw[thick] (7.75,8.25) -- (8.25,8.25);
\end{tikzpicture}

**Step 50, Disorder 34 (6.7%)**

\begin{tikzpicture}
\draw (0.50,0.50) node {0};
\draw (0.50,1.00) node {0};
\draw (0.50,1.50) node {0};
\draw (0.50,2.00) node {0};
\draw (0.50,2.50) node {0};
\draw (0.50,3.00) node {0};
\draw (0.50,3.50) node {0};
\draw (0.50,4.00) node {0};
\draw (0.50,4.50) node {0};
\draw (0.50,5.00) node {0};
\draw (0.50,5.50) node {0};
\draw (0.50,6.00) node {0};
\draw (0.50,6.50) node {0};
\draw (0.50,7.00) node {0};
\draw (0.50,7.50) node {0};
\draw (0.50,8.00) node {0};
\draw (1.00,0.50) node {0};
\draw (1.00,1.00) node {0};
\draw (1.00,1.50) node {0};
\draw[thick] (1.25,1.25) -- (1.25,1.75);
\draw (1.00,2.00) node {0};
\draw[thick] (1.25,1.75) -- (1.25,2.25);
\draw (1.00,2.50) node {0};
\draw[thick] (1.25,2.25) -- (1.25,2.75);
\draw (1.00,3.00) node {0};
\draw[thick] (1.25,2.75) -- (1.25,3.25);
\draw (1.00,3.50) node {0};
\draw (1.00,4.00) node {0};
\draw (1.00,4.50) node {0};
\draw (1.00,5.00) node {0};
\draw (1.00,5.50) node {0};
\draw (1.00,6.00) node {0};
\draw (1.00,6.50) node {0};
\draw (1.00,7.00) node {0};
\draw (1.00,7.50) node {0};
\draw (1.00,8.00) node {0};
\draw (1.50,0.50) node {0};
\draw (1.50,1.00) node {0};
\draw[thick] (1.25,1.25) -- (1.75,1.25);
\draw (1.50,1.50) node {1};
\draw[thick] (1.75,1.25) -- (1.75,1.75);
\draw (1.50,2.00) node {1};
\draw[thick] (1.75,1.75) -- (1.75,2.25);
\draw (1.50,2.50) node {1};
\draw (1.50,3.00) node {1};
\draw[thick] (1.25,3.25) -- (1.75,3.25);
\draw (1.50,3.50) node {0};
\draw[thick] (1.75,3.25) -- (1.75,3.75);
\draw (1.50,4.00) node {0};
\draw (1.50,4.50) node {0};
\draw (1.50,5.00) node {0};
\draw (1.50,5.50) node {0};
\draw[thick] (1.75,5.25) -- (1.75,5.75);
\draw (1.50,6.00) node {0};
\draw (1.50,6.50) node {0};
\draw (1.50,7.00) node {0};
\draw (1.50,7.50) node {0};
\draw (1.50,8.00) node {0};
\draw (2.00,0.50) node {0};
\draw (2.00,1.00) node {0};
\draw (2.00,1.50) node {0};
\draw (2.00,2.00) node {0};
\draw[thick] (1.75,2.25) -- (2.25,2.25);
\draw (2.00,2.50) node {1};
\draw[thick] (2.25,2.25) -- (2.25,2.75);
\draw (2.00,3.00) node {1};
\draw[thick] (2.25,2.75) -- (2.25,3.25);
\draw (2.00,3.50) node {1};
\draw[thick] (1.75,3.75) -- (2.25,3.75);
\draw[thick] (2.25,3.25) -- (2.25,3.75);
\draw (2.00,4.00) node {0};
\draw (2.00,4.50) node {0};
\draw (2.00,5.00) node {0};
\draw[thick] (1.75,5.25) -- (2.25,5.25);
\draw[thick] (2.25,4.75) -- (2.25,5.25);
\draw (2.00,5.50) node {1};
\draw[thick] (1.75,5.75) -- (2.25,5.75);
\draw (2.00,6.00) node {0};
\draw (2.00,6.50) node {0};
\draw (2.00,7.00) node {0};
\draw (2.00,7.50) node {0};
\draw (2.00,8.00) node {0};
\draw (2.50,0.50) node {0};
\draw (2.50,1.00) node {0};
\draw (2.50,1.50) node {0};
\draw (2.50,2.00) node {0};
\draw (2.50,2.50) node {0};
\draw (2.50,3.00) node {0};
\draw (2.50,3.50) node {0};
\draw (2.50,4.00) node {0};
\draw[thick] (2.75,3.75) -- (2.75,4.25);
\draw (2.50,4.50) node {0};
\draw[thick] (2.25,4.75) -- (2.75,4.75);
\draw (2.50,5.00) node {1};
\draw (2.50,5.50) node {1};
\draw[thick] (2.25,5.75) -- (2.75,5.75);
\draw (2.50,6.00) node {0};
\draw (2.50,6.50) node {0};
\draw (2.50,7.00) node {0};
\draw (2.50,7.50) node {0};
\draw (2.50,8.00) node {0};
\draw (3.00,0.50) node {0};
\draw (3.00,1.00) node {0};
\draw (3.00,1.50) node {0};
\draw (3.00,2.00) node {0};
\draw (3.00,2.50) node {0};
\draw (3.00,3.00) node {0};
\draw (3.00,3.50) node {0};
\draw[thick] (2.75,3.75) -- (3.25,3.75);
\draw[thick] (3.25,3.25) -- (3.25,3.75);
\draw (3.00,4.00) node {1};
\draw[thick] (2.75,4.25) -- (3.25,4.25);
\draw (3.00,4.50) node {0};
\draw[thick] (2.75,4.75) -- (3.25,4.75);
\draw[thick] (3.25,4.25) -- (3.25,4.75);
\draw (3.00,5.00) node {1};
\draw (3.00,5.50) node {1};
\draw[thick] (2.75,5.75) -- (3.25,5.75);
\draw[thick] (3.25,5.25) -- (3.25,5.75);
\draw (3.00,6.00) node {0};
\draw (3.00,6.50) node {0};
\draw (3.00,7.00) node {0};
\draw (3.00,7.50) node {0};
\draw (3.00,8.00) node {0};
\draw (3.50,0.50) node {0};
\draw (3.50,1.00) node {0};
\draw (3.50,1.50) node {0};
\draw (3.50,2.00) node {0};
\draw (3.50,2.50) node {0};
\draw (3.50,3.00) node {0};
\draw[thick] (3.25,3.25) -- (3.75,3.25);
\draw (3.50,3.50) node {1};
\draw[thick] (3.75,3.25) -- (3.75,3.75);
\draw (3.50,4.00) node {1};
\draw[thick] (3.75,3.75) -- (3.75,4.25);
\draw (3.50,4.50) node {1};
\draw[thick] (3.75,4.25) -- (3.75,4.75);
\draw (3.50,5.00) node {1};
\draw[thick] (3.25,5.25) -- (3.75,5.25);
\draw[thick] (3.75,4.75) -- (3.75,5.25);
\draw (3.50,5.50) node {0};
\draw (3.50,6.00) node {0};
\draw (3.50,6.50) node {0};
\draw (3.50,7.00) node {0};
\draw (3.50,7.50) node {0};
\draw (3.50,8.00) node {0};
\draw (4.00,0.50) node {0};
\draw (4.00,1.00) node {0};
\draw (4.00,1.50) node {0};
\draw (4.00,2.00) node {0};
\draw (4.00,2.50) node {0};
\draw (4.00,3.00) node {0};
\draw (4.00,3.50) node {0};
\draw (4.00,4.00) node {0};
\draw (4.00,4.50) node {0};
\draw (4.00,5.00) node {0};
\draw (4.00,5.50) node {0};
\draw (4.00,6.00) node {0};
\draw (4.00,6.50) node {0};
\draw (4.00,7.00) node {0};
\draw (4.00,7.50) node {0};
\draw (4.00,8.00) node {0};
\draw (4.50,0.50) node {0};
\draw (4.50,1.00) node {0};
\draw (4.50,1.50) node {0};
\draw (4.50,2.00) node {0};
\draw (4.50,2.50) node {0};
\draw (4.50,3.00) node {0};
\draw (4.50,3.50) node {0};
\draw (4.50,4.00) node {0};
\draw (4.50,4.50) node {0};
\draw (4.50,5.00) node {0};
\draw (4.50,5.50) node {0};
\draw (4.50,6.00) node {0};
\draw (4.50,6.50) node {0};
\draw (4.50,7.00) node {0};
\draw (4.50,7.50) node {0};
\draw (4.50,8.00) node {0};
\draw (5.00,0.50) node {0};
\draw (5.00,1.00) node {0};
\draw (5.00,1.50) node {0};
\draw (5.00,2.00) node {0};
\draw (5.00,2.50) node {0};
\draw (5.00,3.00) node {0};
\draw (5.00,3.50) node {0};
\draw (5.00,4.00) node {0};
\draw (5.00,4.50) node {0};
\draw (5.00,5.00) node {0};
\draw (5.00,5.50) node {0};
\draw (5.00,6.00) node {0};
\draw (5.00,6.50) node {0};
\draw (5.00,7.00) node {0};
\draw (5.00,7.50) node {0};
\draw (5.00,8.00) node {0};
\draw (5.50,0.50) node {0};
\draw (5.50,1.00) node {0};
\draw (5.50,1.50) node {0};
\draw (5.50,2.00) node {0};
\draw (5.50,2.50) node {0};
\draw (5.50,3.00) node {0};
\draw (5.50,3.50) node {0};
\draw (5.50,4.00) node {0};
\draw (5.50,4.50) node {0};
\draw (5.50,5.00) node {0};
\draw (5.50,5.50) node {0};
\draw (5.50,6.00) node {0};
\draw (5.50,6.50) node {0};
\draw (5.50,7.00) node {0};
\draw (5.50,7.50) node {0};
\draw (5.50,8.00) node {0};
\draw (6.00,0.50) node {0};
\draw (6.00,1.00) node {0};
\draw (6.00,1.50) node {0};
\draw (6.00,2.00) node {0};
\draw (6.00,2.50) node {0};
\draw (6.00,3.00) node {0};
\draw (6.00,3.50) node {0};
\draw (6.00,4.00) node {0};
\draw (6.00,4.50) node {0};
\draw (6.00,5.00) node {0};
\draw (6.00,5.50) node {0};
\draw (6.00,6.00) node {0};
\draw (6.00,6.50) node {0};
\draw (6.00,7.00) node {0};
\draw (6.00,7.50) node {0};
\draw (6.00,8.00) node {0};
\draw (6.50,0.50) node {0};
\draw (6.50,1.00) node {0};
\draw (6.50,1.50) node {0};
\draw (6.50,2.00) node {0};
\draw (6.50,2.50) node {0};
\draw (6.50,3.00) node {0};
\draw (6.50,3.50) node {0};
\draw (6.50,4.00) node {0};
\draw (6.50,4.50) node {0};
\draw (6.50,5.00) node {0};
\draw (6.50,5.50) node {0};
\draw (6.50,6.00) node {0};
\draw (6.50,6.50) node {0};
\draw (6.50,7.00) node {0};
\draw (6.50,7.50) node {0};
\draw (6.50,8.00) node {0};
\draw (7.00,0.50) node {0};
\draw (7.00,1.00) node {0};
\draw (7.00,1.50) node {0};
\draw (7.00,2.00) node {0};
\draw (7.00,2.50) node {0};
\draw (7.00,3.00) node {0};
\draw (7.00,3.50) node {0};
\draw (7.00,4.00) node {0};
\draw (7.00,4.50) node {0};
\draw (7.00,5.00) node {0};
\draw (7.00,5.50) node {0};
\draw (7.00,6.00) node {0};
\draw (7.00,6.50) node {0};
\draw (7.00,7.00) node {0};
\draw (7.00,7.50) node {0};
\draw (7.00,8.00) node {0};
\draw (7.50,0.50) node {0};
\draw (7.50,1.00) node {0};
\draw (7.50,1.50) node {0};
\draw (7.50,2.00) node {0};
\draw (7.50,2.50) node {0};
\draw (7.50,3.00) node {0};
\draw (7.50,3.50) node {0};
\draw (7.50,4.00) node {0};
\draw (7.50,4.50) node {0};
\draw (7.50,5.00) node {0};
\draw (7.50,5.50) node {0};
\draw (7.50,6.00) node {0};
\draw (7.50,6.50) node {0};
\draw (7.50,7.00) node {0};
\draw (7.50,7.50) node {0};
\draw (7.50,8.00) node {0};
\draw (8.00,0.50) node {0};
\draw (8.00,1.00) node {0};
\draw (8.00,1.50) node {0};
\draw (8.00,2.00) node {0};
\draw (8.00,2.50) node {0};
\draw (8.00,3.00) node {0};
\draw (8.00,3.50) node {0};
\draw (8.00,4.00) node {0};
\draw (8.00,4.50) node {0};
\draw (8.00,5.00) node {0};
\draw (8.00,5.50) node {0};
\draw (8.00,6.00) node {0};
\draw (8.00,6.50) node {0};
\draw (8.00,7.00) node {0};
\draw (8.00,7.50) node {0};
\draw (8.00,8.00) node {0};
\end{tikzpicture}


Sorting networks
----------------

[Sorting networks](https://en.wikipedia.org/wiki/Sorting_network)
is the second problem I tried to work with.
I'm surely missed some low-level bit-trickery-hackery detail to make
the evolving part of the problem *very fast*.
But I got it fast enough to find the solutions below.

They are beautiful in own irregularity. Would you design such?
(I also not sure if they are proper sorting networks, I haven't tried them).

**7 inputs, length 16, depth 6**

\begin{tikzpicture}
\foreach \a in {0,...,6}
  \draw[thick] (0,\a) -- ++(9,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,5);
\filldraw (1,2) circle (3pt);
\filldraw (1,5) circle (3pt);
\draw[thick] (1.5,3) -- (1.5,6);
\filldraw (1.5,3) circle (3pt);
\filldraw (1.5,6) circle (3pt);
\draw[thick] (2.5,0) -- (2.5,3);
\filldraw (2.5,0) circle (3pt);
\filldraw (2.5,3) circle (3pt);
\draw[thick] (3,1) -- (3,6);
\filldraw (3,1) circle (3pt);
\filldraw (3,6) circle (3pt);
\draw[thick] (3.5,4) -- (3.5,5);
\filldraw (3.5,4) circle (3pt);
\filldraw (3.5,5) circle (3pt);
\draw[thick] (4.5,1) -- (4.5,2);
\filldraw (4.5,1) circle (3pt);
\filldraw (4.5,2) circle (3pt);
\draw[thick] (4.5,3) -- (4.5,4);
\filldraw (4.5,3) circle (3pt);
\filldraw (4.5,4) circle (3pt);
\draw[thick] (4.5,5) -- (4.5,6);
\filldraw (4.5,5) circle (3pt);
\filldraw (4.5,6) circle (3pt);
\draw[thick] (5.5,1) -- (5.5,3);
\filldraw (5.5,1) circle (3pt);
\filldraw (5.5,3) circle (3pt);
\draw[thick] (5.5,4) -- (5.5,5);
\filldraw (5.5,4) circle (3pt);
\filldraw (5.5,5) circle (3pt);
\draw[thick] (6.5,0) -- (6.5,3);
\filldraw (6.5,0) circle (3pt);
\filldraw (6.5,3) circle (3pt);
\draw[thick] (7,2) -- (7,4);
\filldraw (7,2) circle (3pt);
\filldraw (7,4) circle (3pt);
\draw[thick] (8,0) -- (8,1);
\filldraw (8,0) circle (3pt);
\filldraw (8,1) circle (3pt);
\draw[thick] (8,2) -- (8,3);
\filldraw (8,2) circle (3pt);
\filldraw (8,3) circle (3pt);
\draw[thick] (8,4) -- (8,5);
\filldraw (8,4) circle (3pt);
\filldraw (8,5) circle (3pt);
\end{tikzpicture}

or

\begin{tikzpicture}
\foreach \a in {0,...,6}
  \draw[thick] (0,\a) -- ++(9,0);
\draw[thick] (1,0) -- (1,4);
\filldraw (1,0) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (1.5,1) -- (1.5,5);
\filldraw (1.5,1) circle (3pt);
\filldraw (1.5,5) circle (3pt);
\draw[thick] (2,3) -- (2,6);
\filldraw (2,3) circle (3pt);
\filldraw (2,6) circle (3pt);
\draw[thick] (3,0) -- (3,3);
\filldraw (3,0) circle (3pt);
\filldraw (3,3) circle (3pt);
\draw[thick] (3.5,1) -- (3.5,2);
\filldraw (3.5,1) circle (3pt);
\filldraw (3.5,2) circle (3pt);
\draw[thick] (3,5) -- (3,6);
\filldraw (3,5) circle (3pt);
\filldraw (3,6) circle (3pt);
\draw[thick] (4.5,0) -- (4.5,1);
\filldraw (4.5,0) circle (3pt);
\filldraw (4.5,1) circle (3pt);
\draw[thick] (4.5,2) -- (4.5,3);
\filldraw (4.5,2) circle (3pt);
\filldraw (4.5,3) circle (3pt);
\draw[thick] (4.5,4) -- (4.5,5);
\filldraw (4.5,4) circle (3pt);
\filldraw (4.5,5) circle (3pt);
\draw[thick] (5.5,1) -- (5.5,2);
\filldraw (5.5,1) circle (3pt);
\filldraw (5.5,2) circle (3pt);
\draw[thick] (5.5,3) -- (5.5,6);
\filldraw (5.5,3) circle (3pt);
\filldraw (5.5,6) circle (3pt);
\draw[thick] (6.5,2) -- (6.5,4);
\filldraw (6.5,2) circle (3pt);
\filldraw (6.5,4) circle (3pt);
\draw[thick] (7,3) -- (7,5);
\filldraw (7,3) circle (3pt);
\filldraw (7,5) circle (3pt);
\draw[thick] (8,1) -- (8,2);
\filldraw (8,1) circle (3pt);
\filldraw (8,2) circle (3pt);
\draw[thick] (8,3) -- (8,4);
\filldraw (8,3) circle (3pt);
\filldraw (8,4) circle (3pt);
\draw[thick] (8,5) -- (8,6);
\filldraw (8,5) circle (3pt);
\filldraw (8,6) circle (3pt);
\end{tikzpicture}

or

\begin{tikzpicture}
\foreach \a in {0,...,6}
  \draw[thick] (0,\a) -- ++(9,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,4);
\filldraw (1,2) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (1,5) -- (1,6);
\filldraw (1,5) circle (3pt);
\filldraw (1,6) circle (3pt);
\draw[thick] (2,0) -- (2,5);
\filldraw (2,0) circle (3pt);
\filldraw (2,5) circle (3pt);
\draw[thick] (2.5,1) -- (2.5,6);
\filldraw (2.5,1) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (3,3) -- (3,4);
\filldraw (3,3) circle (3pt);
\filldraw (3,4) circle (3pt);
\draw[thick] (4,1) -- (4,3);
\filldraw (4,1) circle (3pt);
\filldraw (4,3) circle (3pt);
\draw[thick] (4.5,2) -- (4.5,5);
\filldraw (4.5,2) circle (3pt);
\filldraw (4.5,5) circle (3pt);
\draw[thick] (5,4) -- (5,6);
\filldraw (5,4) circle (3pt);
\filldraw (5,6) circle (3pt);
\draw[thick] (6,0) -- (6,2);
\filldraw (6,0) circle (3pt);
\filldraw (6,2) circle (3pt);
\draw[thick] (6,3) -- (6,4);
\filldraw (6,3) circle (3pt);
\filldraw (6,4) circle (3pt);
\draw[thick] (7,1) -- (7,2);
\filldraw (7,1) circle (3pt);
\filldraw (7,2) circle (3pt);
\draw[thick] (7,3) -- (7,5);
\filldraw (7,3) circle (3pt);
\filldraw (7,5) circle (3pt);
\draw[thick] (8,0) -- (8,1);
\filldraw (8,0) circle (3pt);
\filldraw (8,1) circle (3pt);
\draw[thick] (8,2) -- (8,3);
\filldraw (8,2) circle (3pt);
\filldraw (8,3) circle (3pt);
\draw[thick] (8,4) -- (8,5);
\filldraw (8,4) circle (3pt);
\filldraw (8,5) circle (3pt);
\end{tikzpicture}

**8 inputs, length 19, depth 6**

The number eight asks for some regularity, it is $2^3$ after all.
But no, random number generator disagrees:

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(10.5,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,3);
\filldraw (1,2) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (1,4) -- (1,5);
\filldraw (1,4) circle (3pt);
\filldraw (1,5) circle (3pt);
\draw[thick] (1,6) -- (1,7);
\filldraw (1,6) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (2,0) -- (2,4);
\filldraw (2,0) circle (3pt);
\filldraw (2,4) circle (3pt);
\draw[thick] (2.5,1) -- (2.5,7);
\filldraw (2.5,1) circle (3pt);
\filldraw (2.5,7) circle (3pt);
\draw[thick] (3,2) -- (3,6);
\filldraw (3,2) circle (3pt);
\filldraw (3,6) circle (3pt);
\draw[thick] (3.5,3) -- (3.5,5);
\filldraw (3.5,3) circle (3pt);
\filldraw (3.5,5) circle (3pt);
\draw[thick] (4.5,0) -- (4.5,2);
\filldraw (4.5,0) circle (3pt);
\filldraw (4.5,2) circle (3pt);
\draw[thick] (5,1) -- (5,3);
\filldraw (5,1) circle (3pt);
\filldraw (5,3) circle (3pt);
\draw[thick] (4.5,4) -- (4.5,6);
\filldraw (4.5,4) circle (3pt);
\filldraw (4.5,6) circle (3pt);
\draw[thick] (6,2) -- (6,4);
\filldraw (6,2) circle (3pt);
\filldraw (6,4) circle (3pt);
\draw[thick] (6.5,3) -- (6.5,6);
\filldraw (6.5,3) circle (3pt);
\filldraw (6.5,6) circle (3pt);
\draw[thick] (7,5) -- (7,7);
\filldraw (7,5) circle (3pt);
\filldraw (7,7) circle (3pt);
\draw[thick] (8,1) -- (8,4);
\filldraw (8,1) circle (3pt);
\filldraw (8,4) circle (3pt);
\draw[thick] (8.5,3) -- (8.5,5);
\filldraw (8.5,3) circle (3pt);
\filldraw (8.5,5) circle (3pt);
\draw[thick] (9.5,1) -- (9.5,2);
\filldraw (9.5,1) circle (3pt);
\filldraw (9.5,2) circle (3pt);
\draw[thick] (9.5,3) -- (9.5,4);
\filldraw (9.5,3) circle (3pt);
\filldraw (9.5,4) circle (3pt);
\draw[thick] (9.5,5) -- (9.5,6);
\filldraw (9.5,5) circle (3pt);
\filldraw (9.5,6) circle (3pt);
\end{tikzpicture}

or

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(10,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,4);
\filldraw (1,2) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (1.5,3) -- (1.5,5);
\filldraw (1.5,3) circle (3pt);
\filldraw (1.5,5) circle (3pt);
\draw[thick] (1,6) -- (1,7);
\filldraw (1,6) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (2.5,0) -- (2.5,6);
\filldraw (2.5,0) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (3,1) -- (3,7);
\filldraw (3,1) circle (3pt);
\filldraw (3,7) circle (3pt);
\draw[thick] (3.5,2) -- (3.5,3);
\filldraw (3.5,2) circle (3pt);
\filldraw (3.5,3) circle (3pt);
\draw[thick] (3.5,4) -- (3.5,5);
\filldraw (3.5,4) circle (3pt);
\filldraw (3.5,5) circle (3pt);
\draw[thick] (4.5,0) -- (4.5,2);
\filldraw (4.5,0) circle (3pt);
\filldraw (4.5,2) circle (3pt);
\draw[thick] (5,1) -- (5,6);
\filldraw (5,1) circle (3pt);
\filldraw (5,6) circle (3pt);
\draw[thick] (5.5,3) -- (5.5,4);
\filldraw (5.5,3) circle (3pt);
\filldraw (5.5,4) circle (3pt);
\draw[thick] (5.5,5) -- (5.5,7);
\filldraw (5.5,5) circle (3pt);
\filldraw (5.5,7) circle (3pt);
\draw[thick] (6.5,1) -- (6.5,3);
\filldraw (6.5,1) circle (3pt);
\filldraw (6.5,3) circle (3pt);
\draw[thick] (6.5,4) -- (6.5,6);
\filldraw (6.5,4) circle (3pt);
\filldraw (6.5,6) circle (3pt);
\draw[thick] (7.5,2) -- (7.5,4);
\filldraw (7.5,2) circle (3pt);
\filldraw (7.5,4) circle (3pt);
\draw[thick] (8,3) -- (8,5);
\filldraw (8,3) circle (3pt);
\filldraw (8,5) circle (3pt);
\draw[thick] (9,1) -- (9,2);
\filldraw (9,1) circle (3pt);
\filldraw (9,2) circle (3pt);
\draw[thick] (9,3) -- (9,4);
\filldraw (9,3) circle (3pt);
\filldraw (9,4) circle (3pt);
\draw[thick] (9,5) -- (9,6);
\filldraw (9,5) circle (3pt);
\filldraw (9,6) circle (3pt);
\end{tikzpicture}

or

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(10,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,5);
\filldraw (1,2) circle (3pt);
\filldraw (1,5) circle (3pt);
\draw[thick] (1.5,3) -- (1.5,4);
\filldraw (1.5,3) circle (3pt);
\filldraw (1.5,4) circle (3pt);
\draw[thick] (1,6) -- (1,7);
\filldraw (1,6) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (2.5,0) -- (2.5,2);
\filldraw (2.5,0) circle (3pt);
\filldraw (2.5,2) circle (3pt);
\draw[thick] (3,1) -- (3,5);
\filldraw (3,1) circle (3pt);
\filldraw (3,5) circle (3pt);
\draw[thick] (3.5,3) -- (3.5,6);
\filldraw (3.5,3) circle (3pt);
\filldraw (3.5,6) circle (3pt);
\draw[thick] (4,4) -- (4,7);
\filldraw (4,4) circle (3pt);
\filldraw (4,7) circle (3pt);
\draw[thick] (5,0) -- (5,3);
\filldraw (5,0) circle (3pt);
\filldraw (5,3) circle (3pt);
\draw[thick] (5.5,1) -- (5.5,6);
\filldraw (5.5,1) circle (3pt);
\filldraw (5.5,6) circle (3pt);
\draw[thick] (6,2) -- (6,4);
\filldraw (6,2) circle (3pt);
\filldraw (6,4) circle (3pt);
\draw[thick] (6,5) -- (6,7);
\filldraw (6,5) circle (3pt);
\filldraw (6,7) circle (3pt);
\draw[thick] (7,1) -- (7,3);
\filldraw (7,1) circle (3pt);
\filldraw (7,3) circle (3pt);
\draw[thick] (7,4) -- (7,6);
\filldraw (7,4) circle (3pt);
\filldraw (7,6) circle (3pt);
\draw[thick] (8,2) -- (8,3);
\filldraw (8,2) circle (3pt);
\filldraw (8,3) circle (3pt);
\draw[thick] (8,4) -- (8,5);
\filldraw (8,4) circle (3pt);
\filldraw (8,5) circle (3pt);
\draw[thick] (9,1) -- (9,2);
\filldraw (9,1) circle (3pt);
\filldraw (9,2) circle (3pt);
\draw[thick] (9,3) -- (9,4);
\filldraw (9,3) circle (3pt);
\filldraw (9,4) circle (3pt);
\draw[thick] (9,5) -- (9,6);
\filldraw (9,5) circle (3pt);
\filldraw (9,6) circle (3pt);
\end{tikzpicture}

Compare these to (boring and regular) [bitonic sorting network](https://oleg.fi/gists/posts/2020-05-19-bitonic-sort.html)
with also depth 6, but using 24 comparators!

\begin{tikzpicture}
\foreach \a in {0,...,7}
  \draw[thick] (0,\a) -- ++(9.5,0);
\draw[thick] (1,0) -- (1,1);
\filldraw (1,0) circle (3pt);
\filldraw (1,1) circle (3pt);
\draw[thick] (1,2) -- (1,3);
\filldraw (1,2) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (2,0) -- (2,3);
\filldraw (2,0) circle (3pt);
\filldraw (2,3) circle (3pt);
\draw[thick] (2.5,1) -- (2.5,2);
\filldraw (2.5,1) circle (3pt);
\filldraw (2.5,2) circle (3pt);
\draw[thick] (3.5,0) -- (3.5,1);
\filldraw (3.5,0) circle (3pt);
\filldraw (3.5,1) circle (3pt);
\draw[thick] (3.5,2) -- (3.5,3);
\filldraw (3.5,2) circle (3pt);
\filldraw (3.5,3) circle (3pt);
\draw[thick] (1,4) -- (1,5);
\filldraw (1,4) circle (3pt);
\filldraw (1,5) circle (3pt);
\draw[thick] (1,6) -- (1,7);
\filldraw (1,6) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (2,4) -- (2,7);
\filldraw (2,4) circle (3pt);
\filldraw (2,7) circle (3pt);
\draw[thick] (2.5,5) -- (2.5,6);
\filldraw (2.5,5) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (3.5,4) -- (3.5,5);
\filldraw (3.5,4) circle (3pt);
\filldraw (3.5,5) circle (3pt);
\draw[thick] (3.5,6) -- (3.5,7);
\filldraw (3.5,6) circle (3pt);
\filldraw (3.5,7) circle (3pt);
\draw[thick] (4.5,0) -- (4.5,7);
\filldraw (4.5,0) circle (3pt);
\filldraw (4.5,7) circle (3pt);
\draw[thick] (5,1) -- (5,6);
\filldraw (5,1) circle (3pt);
\filldraw (5,6) circle (3pt);
\draw[thick] (5.5,2) -- (5.5,5);
\filldraw (5.5,2) circle (3pt);
\filldraw (5.5,5) circle (3pt);
\draw[thick] (6,3) -- (6,4);
\filldraw (6,3) circle (3pt);
\filldraw (6,4) circle (3pt);
\draw[thick] (7,0) -- (7,2);
\filldraw (7,0) circle (3pt);
\filldraw (7,2) circle (3pt);
\draw[thick] (7.5,1) -- (7.5,3);
\filldraw (7.5,1) circle (3pt);
\filldraw (7.5,3) circle (3pt);
\draw[thick] (8.5,0) -- (8.5,1);
\filldraw (8.5,0) circle (3pt);
\filldraw (8.5,1) circle (3pt);
\draw[thick] (8.5,2) -- (8.5,3);
\filldraw (8.5,2) circle (3pt);
\filldraw (8.5,3) circle (3pt);
\draw[thick] (7,4) -- (7,6);
\filldraw (7,4) circle (3pt);
\filldraw (7,6) circle (3pt);
\draw[thick] (7.5,5) -- (7.5,7);
\filldraw (7.5,5) circle (3pt);
\filldraw (7.5,7) circle (3pt);
\draw[thick] (8.5,4) -- (8.5,5);
\filldraw (8.5,4) circle (3pt);
\filldraw (8.5,5) circle (3pt);
\draw[thick] (8.5,6) -- (8.5,7);
\filldraw (8.5,6) circle (3pt);
\filldraw (8.5,7) circle (3pt);
\end{tikzpicture}


**9 inputs, length 25, depth 7**

Nine. Another odd number of inputs
opens up the creativity of the machine:

\begin{tikzpicture}
\foreach \a in {0,...,8}
  \draw[thick] (0,\a) -- ++(12.5,0);
\draw[thick] (1,0) -- (1,3);
\filldraw (1,0) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (1.5,1) -- (1.5,2);
\filldraw (1.5,1) circle (3pt);
\filldraw (1.5,2) circle (3pt);
\draw[thick] (1,4) -- (1,6);
\filldraw (1,4) circle (3pt);
\filldraw (1,6) circle (3pt);
\draw[thick] (1,7) -- (1,8);
\filldraw (1,7) circle (3pt);
\filldraw (1,8) circle (3pt);
\draw[thick] (2.5,0) -- (2.5,1);
\filldraw (2.5,0) circle (3pt);
\filldraw (2.5,1) circle (3pt);
\draw[thick] (2.5,2) -- (2.5,3);
\filldraw (2.5,2) circle (3pt);
\filldraw (2.5,3) circle (3pt);
\draw[thick] (2.5,4) -- (2.5,7);
\filldraw (2.5,4) circle (3pt);
\filldraw (2.5,7) circle (3pt);
\draw[thick] (3,6) -- (3,8);
\filldraw (3,6) circle (3pt);
\filldraw (3,8) circle (3pt);
\draw[thick] (4,0) -- (4,4);
\filldraw (4,0) circle (3pt);
\filldraw (4,4) circle (3pt);
\draw[thick] (4.5,1) -- (4.5,2);
\filldraw (4.5,1) circle (3pt);
\filldraw (4.5,2) circle (3pt);
\draw[thick] (4.5,3) -- (4.5,8);
\filldraw (4.5,3) circle (3pt);
\filldraw (4.5,8) circle (3pt);
\draw[thick] (5,5) -- (5,7);
\filldraw (5,5) circle (3pt);
\filldraw (5,7) circle (3pt);
\draw[thick] (6,1) -- (6,6);
\filldraw (6,1) circle (3pt);
\filldraw (6,6) circle (3pt);
\draw[thick] (6.5,2) -- (6.5,7);
\filldraw (6.5,2) circle (3pt);
\filldraw (6.5,7) circle (3pt);
\draw[thick] (7,4) -- (7,5);
\filldraw (7,4) circle (3pt);
\filldraw (7,5) circle (3pt);
\draw[thick] (8,1) -- (8,4);
\filldraw (8,1) circle (3pt);
\filldraw (8,4) circle (3pt);
\draw[thick] (8.5,2) -- (8.5,5);
\filldraw (8.5,2) circle (3pt);
\filldraw (8.5,5) circle (3pt);
\draw[thick] (9,3) -- (9,6);
\filldraw (9,3) circle (3pt);
\filldraw (9,6) circle (3pt);
\draw[thick] (8,7) -- (8,8);
\filldraw (8,7) circle (3pt);
\filldraw (8,8) circle (3pt);
\draw[thick] (10,0) -- (10,1);
\filldraw (10,0) circle (3pt);
\filldraw (10,1) circle (3pt);
\draw[thick] (10,2) -- (10,4);
\filldraw (10,2) circle (3pt);
\filldraw (10,4) circle (3pt);
\draw[thick] (10.5,3) -- (10.5,5);
\filldraw (10.5,3) circle (3pt);
\filldraw (10.5,5) circle (3pt);
\draw[thick] (10,6) -- (10,7);
\filldraw (10,6) circle (3pt);
\filldraw (10,7) circle (3pt);
\draw[thick] (11.5,3) -- (11.5,4);
\filldraw (11.5,3) circle (3pt);
\filldraw (11.5,4) circle (3pt);
\draw[thick] (11.5,5) -- (11.5,6);
\filldraw (11.5,5) circle (3pt);
\filldraw (11.5,6) circle (3pt);
\end{tikzpicture}

It is a [theorem](https://arxiv.org/pdf/1507.01428.pdf)
that in optimal sorting networks the last layer consist of $(i, i+1)$
comparators. The search indeed finds networks with this property.

\begin{tikzpicture}
\foreach \a in {0,...,8}
  \draw[thick] (0,\a) -- ++(12,0);
\draw[thick] (1,1) -- (1,4);
\filldraw (1,1) circle (3pt);
\filldraw (1,4) circle (3pt);
\draw[thick] (1.5,2) -- (1.5,3);
\filldraw (1.5,2) circle (3pt);
\filldraw (1.5,3) circle (3pt);
\draw[thick] (1,5) -- (1,8);
\filldraw (1,5) circle (3pt);
\filldraw (1,8) circle (3pt);
\draw[thick] (1.5,6) -- (1.5,7);
\filldraw (1.5,6) circle (3pt);
\filldraw (1.5,7) circle (3pt);
\draw[thick] (2.5,0) -- (2.5,1);
\filldraw (2.5,0) circle (3pt);
\filldraw (2.5,1) circle (3pt);
\draw[thick] (2.5,3) -- (2.5,4);
\filldraw (2.5,3) circle (3pt);
\filldraw (2.5,4) circle (3pt);
\draw[thick] (2.5,5) -- (2.5,6);
\filldraw (2.5,5) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (2.5,7) -- (2.5,8);
\filldraw (2.5,7) circle (3pt);
\filldraw (2.5,8) circle (3pt);
\draw[thick] (3.5,0) -- (3.5,2);
\filldraw (3.5,0) circle (3pt);
\filldraw (3.5,2) circle (3pt);
\draw[thick] (4,1) -- (4,3);
\filldraw (4,1) circle (3pt);
\filldraw (4,3) circle (3pt);
\draw[thick] (3.5,4) -- (3.5,8);
\filldraw (3.5,4) circle (3pt);
\filldraw (3.5,8) circle (3pt);
\draw[thick] (4,6) -- (4,7);
\filldraw (4,6) circle (3pt);
\filldraw (4,7) circle (3pt);
\draw[thick] (5,0) -- (5,5);
\filldraw (5,0) circle (3pt);
\filldraw (5,5) circle (3pt);
\draw[thick] (5.5,1) -- (5.5,6);
\filldraw (5.5,1) circle (3pt);
\filldraw (5.5,6) circle (3pt);
\draw[thick] (6,2) -- (6,7);
\filldraw (6,2) circle (3pt);
\filldraw (6,7) circle (3pt);
\draw[thick] (6.5,3) -- (6.5,4);
\filldraw (6.5,3) circle (3pt);
\filldraw (6.5,4) circle (3pt);
\draw[thick] (7.5,2) -- (7.5,5);
\filldraw (7.5,2) circle (3pt);
\filldraw (7.5,5) circle (3pt);
\draw[thick] (8,3) -- (8,6);
\filldraw (8,3) circle (3pt);
\filldraw (8,6) circle (3pt);
\draw[thick] (8.5,4) -- (8.5,7);
\filldraw (8.5,4) circle (3pt);
\filldraw (8.5,7) circle (3pt);
\draw[thick] (9.5,1) -- (9.5,2);
\filldraw (9.5,1) circle (3pt);
\filldraw (9.5,2) circle (3pt);
\draw[thick] (9.5,3) -- (9.5,5);
\filldraw (9.5,3) circle (3pt);
\filldraw (9.5,5) circle (3pt);
\draw[thick] (10,4) -- (10,6);
\filldraw (10,4) circle (3pt);
\filldraw (10,6) circle (3pt);
\draw[thick] (11,2) -- (11,3);
\filldraw (11,2) circle (3pt);
\filldraw (11,3) circle (3pt);
\draw[thick] (11,4) -- (11,5);
\filldraw (11,4) circle (3pt);
\filldraw (11,5) circle (3pt);
\draw[thick] (11,7) -- (11,8);
\filldraw (11,7) circle (3pt);
\filldraw (11,8) circle (3pt);
\end{tikzpicture}

Finding a 9 input network takes about 5 minutes on my machine.
I haven't tried *very hard* to see if it can be made faster.
(E.g. I didn't profile, nor thought too hard about algorithmic design).
The resulting `RTS -s` output brings *pure joy*:

```plain
# 16 threads, no parallel GC, bigger nursery
cabal run end-example -- sn9 +RTS -s -N16 -qg -A128M

  INIT    time    0.027s  (  0.042s elapsed)
  MUT     time 3691.333s  (246.617s elapsed)
  GC      time   12.775s  ( 12.728s elapsed)
  EXIT    time    0.008s  (  0.005s elapsed)
  Total   time 3704.144s  (259.392s elapsed)

3698,08s user 6,18s system 1427% cpu 4:19,51 total
```

(I screwed something in recent refactorings, so garbage collection takes some time;
previously the solver was essentially non-allocating).
The speedup is almost linear, compare to `-N8`:

```plain
cabal run end-example -- sn9 +RTS -s -N8 -qg -A128M

  INIT    time    0.014s  (  0.013s elapsed)
  MUT     time 3466.720s  (442.997s elapsed)
  GC      time   14.807s  ( 14.806s elapsed)
  EXIT    time    0.004s  (  0.007s elapsed)
  Total   time 3481.545s  (457.822s elapsed)
```

Notice that the total time is about the same but the elapsed time is almost
doubled! Such "easy" speedup by increasing `-N` is quite rare
in my experience. The END method is just so well suited for parallel execution.

At this point you definitely should ask yourself,
why solving problems already solved?
Especially if you don't have access to supercomputers to tackle
bigger problems.

The answer is that when you know how they are solved,
you may adjust the method to solve slightly different problems
which are still solvable on modern desktop hardware.

For example and pure fun, the next sorting network
is (hopefully valid) two part network,
where the second part is the mirror image of the first part.
That is non-sense problem in practice but it makes you a nice looking diagram.
(Using the same network twice or more times to get eventually sorted
result could be useful, e.g. when you are limited in "code size").

\begin{tikzpicture}
\foreach \a in {0,...,8}
  \draw[thick] (0,\a) -- ++(15,0);
\draw[thick] (1,0) -- (1,3);
\filldraw (1,0) circle (3pt);
\filldraw (1,3) circle (3pt);
\draw[thick] (1.5,1) -- (1.5,2);
\filldraw (1.5,1) circle (3pt);
\filldraw (1.5,2) circle (3pt);
\draw[thick] (1,4) -- (1,7);
\filldraw (1,4) circle (3pt);
\filldraw (1,7) circle (3pt);
\draw[thick] (1.5,6) -- (1.5,8);
\filldraw (1.5,6) circle (3pt);
\filldraw (1.5,8) circle (3pt);
\draw[thick] (2.5,0) -- (2.5,1);
\filldraw (2.5,0) circle (3pt);
\filldraw (2.5,1) circle (3pt);
\draw[thick] (2.5,2) -- (2.5,3);
\filldraw (2.5,2) circle (3pt);
\filldraw (2.5,3) circle (3pt);
\draw[thick] (2.5,4) -- (2.5,6);
\filldraw (2.5,4) circle (3pt);
\filldraw (2.5,6) circle (3pt);
\draw[thick] (2.5,7) -- (2.5,8);
\filldraw (2.5,7) circle (3pt);
\filldraw (2.5,8) circle (3pt);
\draw[thick] (3.5,2) -- (3.5,4);
\filldraw (3.5,2) circle (3pt);
\filldraw (3.5,4) circle (3pt);
\draw[thick] (4,3) -- (4,5);
\filldraw (4,3) circle (3pt);
\filldraw (4,5) circle (3pt);
\draw[thick] (3.5,6) -- (3.5,7);
\filldraw (3.5,6) circle (3pt);
\filldraw (3.5,7) circle (3pt);
\draw[thick] (5,5) -- (5,6);
\filldraw (5,5) circle (3pt);
\filldraw (5,6) circle (3pt);
\draw[thick] (6,1) -- (6,3);
\filldraw (6,1) circle (3pt);
\filldraw (6,3) circle (3pt);
\draw[thick] (6,4) -- (6,5);
\filldraw (6,4) circle (3pt);
\filldraw (6,5) circle (3pt);
\draw[thick] (7,2) -- (7,3);
\filldraw (7,2) circle (3pt);
\filldraw (7,3) circle (3pt);
\draw[thick] (8,5) -- (8,8);
\filldraw (8,5) circle (3pt);
\filldraw (8,8) circle (3pt);
\draw[thick] (8.5,6) -- (8.5,7);
\filldraw (8.5,6) circle (3pt);
\filldraw (8.5,7) circle (3pt);
\draw[thick] (8,1) -- (8,4);
\filldraw (8,1) circle (3pt);
\filldraw (8,4) circle (3pt);
\draw[thick] (8.5,0) -- (8.5,2);
\filldraw (8.5,0) circle (3pt);
\filldraw (8.5,2) circle (3pt);
\draw[thick] (9.5,7) -- (9.5,8);
\filldraw (9.5,7) circle (3pt);
\filldraw (9.5,8) circle (3pt);
\draw[thick] (9.5,5) -- (9.5,6);
\filldraw (9.5,5) circle (3pt);
\filldraw (9.5,6) circle (3pt);
\draw[thick] (9.5,2) -- (9.5,4);
\filldraw (9.5,2) circle (3pt);
\filldraw (9.5,4) circle (3pt);
\draw[thick] (9.5,0) -- (9.5,1);
\filldraw (9.5,0) circle (3pt);
\filldraw (9.5,1) circle (3pt);
\draw[thick] (10.5,4) -- (10.5,6);
\filldraw (10.5,4) circle (3pt);
\filldraw (10.5,6) circle (3pt);
\draw[thick] (11,3) -- (11,5);
\filldraw (11,3) circle (3pt);
\filldraw (11,5) circle (3pt);
\draw[thick] (10.5,1) -- (10.5,2);
\filldraw (10.5,1) circle (3pt);
\filldraw (10.5,2) circle (3pt);
\draw[thick] (12,2) -- (12,3);
\filldraw (12,2) circle (3pt);
\filldraw (12,3) circle (3pt);
\draw[thick] (13,5) -- (13,7);
\filldraw (13,5) circle (3pt);
\filldraw (13,7) circle (3pt);
\draw[thick] (13,3) -- (13,4);
\filldraw (13,3) circle (3pt);
\filldraw (13,4) circle (3pt);
\draw[thick] (14,5) -- (14,6);
\filldraw (14,5) circle (3pt);
\filldraw (14,6) circle (3pt);
\end{tikzpicture}


Finnish Randonneur
------------------

The last problem I tackled is a version
of [Traveling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem).
Because I like cycling and live in Finland,
this instantiation of it is named *Finnish [Randonneur](https://en.wikipedia.org/wiki/Randonneuring)*.

The problem is to find the shortest loop through 30 larges cities of Finland.

The 30 largest cities of Finland ([according to Wikipedia](https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_Finland)) are:
Helsinki (population 658650),
Espoo,
Tampere,
Vantaa,
Oulu,
Turku,
Jyväskylä,
Lahti,
Kuopio,
Pori,
Kouvola,
Joensuu,
Lappeenranta,
Hämeenlinna,
Vaasa,
Seinäjoki,
Rovaniemi,
Mikkeli,
Kotka,
Salo,
Porvoo,
Kokkola,
Hyvinkää,
Lohja,
Jarvenpää,
Rauma,
Kajaani,
Kerava,
Savonlinna and
Nokia (population 33403),

The problem itself is very similar to ramp problem. Except here instead
of 10 elements we have 30, so there are 

$$
29! = 8841761993739701954543616000000
$$

possible permutations.
Notice not $30!$, as start&end-point is irrelevant.
You can cut the search space even more by using some symmetries,
and properties of the problem. But why would you!?

Because we are searching for the loop,
I made algorithm extend the path in both directions,
this way the commitment preserved both the way out and way in
into the fixed starting city (which I "arbitrarily" pick to be Helsinki).

On my machine solutions are found in a minute and a half.
Quite a lot faster than naive enumeration!

Using "as the crow flies" distances between cities
the loops are

- 2446.96
- 2449.45
- 2460.59

kilometers long. The actual road distances are larger (close to 3000km),
and the last one seems to be in fact the shorter one.

In the pictures below I omitted the visit to the Northern Finland,
each solution went from Vaasa to Oulu, then to Rovaniemi and to Kajaani.
In the routes draw we short cut from Vaasa to Kajaani.
The topography in the southern part is mostly
the same in the two first routes, the difference is whether
one goes through Mikkeli on the way up or down.
Third route is more different, once one reaches Tampere,
you turn north and rather collect the most of the checkpoints
on the way back south.
Here you have there options for 2-3 week cycling trip around Finland
(I do not recommend taking the drawn big roads, there
are nicer alternatives for cycling).

The pictures below are screenshots from https://maps.openrouteservice.org/

![Finland Tour 1](finland-route-1.png)

![Finland Tour 2](finland-route-2.png)

![Finland Tour 3](finland-route-3.png)

Conclusion
----------

In this blog post I briefly described the *evolving non-determinism*
search method. There are three examples, and some pretty pictures.

If you want to look at the code, I put it on 
[GitHub in phadej/end repository](https://github.com/phadej/end).
