---
title: Do you have a problem? Write a compiler!
author: Oleg Grenrus
---

These are notes of the talk I gave at [ClojuTre 2019](https://clojutre.org/2019/).
There is [a video recording of the talk](https://www.youtube.com/watch?v=kOXfdZRD0wM),
and [the slide deck as a PDF](https://oleg.fi/pdfs/write-a-compiler.pdf).

[<img title="cc-by-sa" src="../images/by-sa.svg" />
This work is licensed under a “CC BY SA 4.0” license.
](https://creativecommons.org/licenses/by-sa/2.0/)

<div style="padding-left: 1em"><img title="slide-00" src="../images/write-a-compiler-00-fs8.png" /></div>

Hello ClojuTre. I'm Oleg from Helsinki.

---

<div style="padding-left: 1em"><img title="slide-01" src="../images/write-a-compiler-01-fs8.png" /></div>

Imagine you are writing a cool new rogue-like game.
So cool many have no idea what's going on.
A definitive character of rogue-likes is procedural generation of content.
You'll need a random number generator for that.

---

<div style="padding-left: 1em"><img title="slide-02" src="../images/write-a-compiler-02-fs8.png" /></div>

SplitMix is a fast, splittable pseudorandom number generator.
Being able to split a generator into two independent generators is a property you'll want if you use the generator in a functional programming language. Look it up.
We'll concentrate on the being fast part.
Obviously you want things to be fast.

SplitMix is fast. It does only 9 operations per generated number,
one addition to advance the random seed, and `xor`, `shift`, `multiply`
`xor`, `shift`, `multiply`, `xor` and `shift`. 9 in total.

However, for the maximum reach, we want our game to run in the web browsers.

---

<div style="padding-left: 1em"><img title="slide-03" src="../images/write-a-compiler-03-fs8.png" /></div>

- JavaScript is a great platform
- But a terrible programming language

---

<div style="padding-left: 1em"><img title="slide-04" src="../images/write-a-compiler-04-fs8.png" /></div>

Look carefully. When we multiply two "big" odd numbers, we do get even result.
We shouldn't: product of two odd numbers is odd.

JavaScript numbers are a mess.
Next a bit of arithmetics.

---

<div style="padding-left: 1em"><img title="slide-05" src="../images/write-a-compiler-05-fs8.png" /></div>

Instead of multiplying two big numbers, we split them in high and low parts
and multiply many small numbers. Like in an elementary school.
Then we'll have enough precision.

---

<div style="padding-left: 1em"><img title="slide-06" src="../images/write-a-compiler-06-fs8.png" /></div>

So as we are multiplying 32bit numbers, and are interested only in lower 32 bits
of 64 bit result. Then we need to do only three multiplications.

At the end, we can get correct results, i.e. good random numbers even in JavaScript.

---

<div style="padding-left: 1em"><img title="slide-07" src="../images/write-a-compiler-07-fs8.png" /></div>

Next if we replace the multiplication in `xor-shift-multiply` with
a macro doing the right thing, and actually change everything to be a macro,
and expand we'll get...

----

<div style="padding-left: 1em"><img title="slide-08" src="../images/write-a-compiler-08-fs8.png" /></div>

a lot of code which barely fits on the slide.

I had to shorted `bit-shift-left` to `bsl` and `unsigned-bit-shift-right` to `ubsr`.

---

<div style="padding-left: 1em"><img title="slide-09" src="../images/write-a-compiler-09-fs8.png" /></div>

Look closely. Would you write this kind of code.

---

<div style="padding-left: 1em"><img title="slide-10" src="../images/write-a-compiler-10-fs8.png" /></div>

We bind ("assign") a constant value to `uv` variable.

---

<div style="padding-left: 1em"><img title="slide-11" src="../images/write-a-compiler-11-fs8.png" /></div>

And the calculate the high and low 16 bit parts of it.
Something we could write directly: `let [u 0x85eb v ca6b]`,
i.e. *optimize by hand*. Should we do so?

---

<div style="padding-left: 1em"><img title="slide-12" src="../images/write-a-compiler-12-fs8.png" /></div>

No. Let's rather write an (optimizing) compiler.
We don't have time to optimize by hand.

---

<div style="padding-left: 1em"><img title="slide-13" src="../images/write-a-compiler-13-fs8.png" /></div>

Recall, we have a very specific problem.
Working with a very very tiny subset of Clojure.
Some simple arithmetic and bit mangling of 32bit numbers.

---

<div style="padding-left: 1em"><img title="slide-14" src="../images/write-a-compiler-14-fs8.png" /></div>

We'd like to add a little of `magic`, which would make the program magically run faster.
In my toy-micro benchmarks the speed-up is 10 percent.
It's definitely worth it.

---

<div style="padding-left: 1em"><img title="slide-15" src="../images/write-a-compiler-15-fs8.png" /></div>

What is this magic?

---

<div style="padding-left: 1em"><img title="slide-16" src="../images/write-a-compiler-16-fs8.png" /></div>

A little cute macro. Of course.

---

<div style="padding-left: 1em"><img title="slide-17" src="../images/write-a-compiler-17-fs8.png" /></div>

We get a `form` and convert it into internal representation on a way in,
and back to Clojure on the way back.

Working the whole Clojure syntax directly is insane task.
We want only deal with a small sane subset of it.
Also in a easier to manipulate format.
Clojure as a pragmatic language is still optimized for writing and reading,
not machine manipulation so much.

---

<div style="padding-left: 1em"><img title="slide-18" src="../images/write-a-compiler-18-fs8.png" /></div>

The next step is to expand a multiplication using a trick we have seen previously.
We could have done it already in `from-clojure` step,
but it's good engineering to have only one thing per step.

---

<div style="padding-left: 1em"><img title="slide-19" src="../images/write-a-compiler-19-fs8.png" /></div>

And the important part is the `optimise` function.

---

<div style="padding-left: 1em"><img title="slide-20" src="../images/write-a-compiler-20-fs8.png" /></div>

The internal representation I used is nested vectors with a node type as a first element.
An uniform representation made it way easier to do everything else.
Note how literals 1 and 3 are wrapped into vector too.
We can simply look at the head of a vector to know what we are dealing with.

Code is Data.

---

<div style="padding-left: 1em"><img title="slide-21" src="../images/write-a-compiler-21-fs8.png" /></div>

A difficult part in *Code is Data* are local variables.
I chose to use *de Bruijn* indices, so instead of names: `x` and `y` or
`a` and `b` there are numbers counting towards the corresponding `let`
(which binds only one variable at the time by the way).

de Bruijn indices are tricky to grok.
Look at the colors, they are there to help. The blue `:var 1` references "one away" `let`.

I don't expect you to understand them. It's one way to represent bindings.
Perfectly there should be a library, so you don't need to think about low-level details.

---

<div style="padding-left: 1em"><img title="slide-22" src="../images/write-a-compiler-22-fs8.png" /></div>

Once we got rid out of names, we can still keep them around using metadata.
That's a really cool feature in Clojure, I have to admit.
The metadata is there, but not in your way.
And having names around is actually useful when you try to debug things.

---

<div style="padding-left: 1em"><img title="slide-23" src="../images/write-a-compiler-23-fs8.png" /></div>

Now we have a setup done. Let's jump into optimizations.

---

<div style="padding-left: 1em"><img title="slide-24" src="../images/write-a-compiler-24-fs8.png" /></div>

Recall our code snippet.
There's `uv` which is bound to a constant.
And then it's used an expression which could simplify if we do this and that...

---

<div style="padding-left: 1em"><img title="slide-25" src="../images/write-a-compiler-25-fs8.png" /></div>

Let's keep it super simple.

- We can have a small set of simple local rewrite rules.
  Local meaning, we don't need to look around, only at one subexpression at the time.
- Then we try to match the rule everywhere, and if it match, perform the rewrite.
- And loop until there's nothing to do.

---

<div style="padding-left: 1em"><img title="slide-26" src="../images/write-a-compiler-26-fs8.png" /></div>

The first optimization is inlining.
In a sense it's most powerful one, as it makes opportunities for other optimizations to fire,
even that on itself it doesn't do much.

So if we have a let-binding, then in some cases we perform a substitution.
Replace all `x`s with an `expression` inside a body.

`let x 1 y 1 (+ x y)` to a lot simpler `(+ 1 2)`. But nothing more, just that.

---

<div style="padding-left: 1em"><img title="slide-27" src="../images/write-a-compiler-27-fs8.png" /></div>

I need to point out, that optimizing is somewhat of an art.
Sometimes it work, sometimes it don't.

For example, we don't want to duplicate an expensive `(fibonacci 100)`
expression. We want to evaluate it once and share the result.

On the other hand, if someone already went and computed the value,
then we can push it to the leaves of an expression tree.

Heuristics are tricky.

Luckily for our needs simple heuristics work well.

---

<div style="padding-left: 1em"><img title="slide-28" src="../images/write-a-compiler-28-fs8.png" /></div>

When *inlining* is a valid rewrite?

---

<div style="padding-left: 1em"><img title="slide-29" src="../images/write-a-compiler-29-fs8.png" /></div>

Not in every language.
Consider this not-so-functional example.

If we substitute everything, the `(do-foo)` and `(do-bar)` would be in different order.
And `(do-quux)` will be gone completely, hopefully it didn't anything important!

We need a language where there are no side-effects, nor there are so much
difference in the execution order.
Whole Clojure isn't such language. Our tiny subset is.

I have heard there are programming languages which behave like our small one,
but are more general purpose!

OK. Let's move to the next optimization.

---

<div style="padding-left: 1em"><img title="slide-30" src="../images/write-a-compiler-30-fs8.png" /></div>

When we have something simple as `(+ 1 2)`, let us evaluate it already at compile time.

For every primitive operation, if the arguments are known constants, just do it.

---

<div style="padding-left: 1em"><img title="slide-31" src="../images/write-a-compiler-31-fs8.png" /></div>

Now, I ask you when *constant folding* is a valid rewrite.

---

<div style="padding-left: 1em"><img title="slide-32" src="../images/write-a-compiler-32-fs8.png" /></div>

Well, that was a trick question.
It really depends on primitives, whether it make sense to perform them at compile time.
(Even pure languages have primitives to print stuff on a screen).

But you could think about that `precalculate` example.
When does it makes to perform the calculation (assuming we somehow know it terminates):

- At compile time?
- At start up?
- At first access?

It really depends, and there are no single simple answer.

Again, optimizations is an art.

---

<div style="padding-left: 1em"><img title="slide-33" src="../images/write-a-compiler-33-fs8.png" /></div>

Because we work with nice internal representation, writing individual optimizations is so nice.

- if a node is not one of special nodes
- and all node arguments are constants
- evaluate it.

The code is shorter than my explanation. And still understandable.

---

<div style="padding-left: 1em"><img title="slide-34" src="../images/write-a-compiler-34-fs8.png" /></div>

With these two optimizations, inlining and constant folding, we get from this
big (and repetitive) code blob to...

---

<div style="padding-left: 1em"><img title="slide-35" src="../images/write-a-compiler-35-fs8.png" /></div>

Something which actually fits on the slide without font size scaling.
It's not super-pretty, but it's hard to spot if something can be done there.

---

<div style="padding-left: 1em"><img title="slide-36" src="../images/write-a-compiler-36-fs8.png" /></div>

We can make it look nice with one more optimization.
When you bind a let-expression to a variable in outer let-expression,
we can float out the inner one.

A very old idea it is.

---

<div style="padding-left: 1em"><img title="slide-37" src="../images/write-a-compiler-37-fs8.png" /></div>

And then we get (in my opinion) a very nice direct code.
Six intermediate results to get final one.

---

<div style="padding-left: 1em"><img title="slide-38" src="../images/write-a-compiler-38-fs8.png" /></div>

That's what I wanted to tell you.

---

<div style="padding-left: 1em"><img title="slide-39" src="../images/write-a-compiler-39-fs8.png" /></div>

- Implementing small (domain specific) languages is fun.
- If you approach problems with "let's write a programming language to describe them" -attitude
  there a lot of big hammers in your disposal. A lot of wheels is already invented.
- Languages don't need only be about numerics, it could be HTTP routing, authorisation rules, UI-workflows, CI-scripts (I could bash about bash), data descriptions, you name it.
- But make your languages *typed, lazy, pure, total or and even dependent* for extra fun and interesting new problems. ;)

Thank you.
