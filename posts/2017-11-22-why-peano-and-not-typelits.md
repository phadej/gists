---
title: Is there a reason to prefer Peano numbers to type-level arithmetic built in GHC?
author: Oleg Grenrus
tags: 
---

I was asked: *Is there a reason to prefer Peano numbers to type-level arithmetic built in GHC?*
in the [fin and vec -announcement thread on Reddit](https://www.reddit.com/r/haskell/comments/7ehiso/ann_fin_vec_packages/dq59hh3/).

That's a good question.

My answer was:

> One way to see that is: If you use a library using type-level naturals, than
> `GHC.TypeNats` -based would be nicer to use (more convenient, maybe faster).
> OTOH, when writing a library, less "magic" is better.

It can be reprhased into:

- GHC.TypeLits is magic, and to work with them you'd need more magic
- Peano numbers provide structural invariants, which are great for type-driven development.

---

Let's expand the first point. Let's look into [`vector-sized`](https://hackage.haskell.org/package/vector-sized-0.6.1.0) library,
which in the version 0.6.1.0 uses [`finite-typelits`](https://hackage.haskell.org/package/finite-typelits) for
some parts.

We can create a vector:

```haskell
λ> :set -XDataKinds 
λ> let vec = generate (\x -> x * x) :: Vector 10 Int
λ> vec
Vector [0,1,4,9,16,25,36,49,64,81]
```

and query elements:

```
λ> index vec 3
9
λ> index vec 11
*** Exception: fromInteger: Integer 11 is not representable in Finite 10
```

The author of `finite-typelits` chose to error if we try to create invalid `Finite` number,
which is fair choice. So far so good.

Looking at the documentation of `vector-sized` you may notice the type of `izipWith` is

```haskell
izipWith :: (Int -> a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
```

You'd want to go all in, and use `Finite n` there instead of an `Int`.
Luckily, there are building blocks:

```haskell
generate_ :: forall n a. KnownNat n => (Finite n -> a) -> Vector n a
zipWith3 :: (a -> b -> c -> d)
         -> Vector n a -> Vector n b -> Vector n c -> Vector n d
```

so implementing `fzipWith` is simple:

```haskell
λ> let fzipWith f = V.zipWith3 f (V.generate_ id)
λ> :t fzipWith 
fzipWith
  :: KnownNat n => (Finite n -> b -> c -> d)
     -> Vector n b -> Vector n c -> Vector n d
```

but imagine, there weren't `generate_` available. Then we'd need to implement it:

```haskell
generate_ :: forall v n a. (KnownNat n, VG.Vector v a)
          => (Finite n -> a) -> Vector v n a
generate_ f = Vector (VG.generate i (f . Finite . fromIntegral))
  where i = fromInteger (natVal (Proxy :: Proxy n))
```

Note the use of **unsafe** `Finite` constructor.
As `Finite` invariant isn't structural you need to verify it manually.
As a library writer (you wear a library writer hat, when a library is missing the feature, and you are adding it, either to the original library or in your code),
You have to *verify that your code is correct by hand*. **Compiler doesn't help you here**.

---

Compare to the similar function in `vec` library: there's nothing unsafe (`P.Vec n a ~ Fin n -> a`):

```haskell
fromPull :: forall n a. N.SNatI n => P.Vec n a -> Vec n a
fromPull (P.Vec f) = case N.snat :: N.SNat n of
    N.SZ -> VNil
    N.SS -> f F.Z ::: fromPull (P.Vec (f . F.S))
```

Not only compiler verifies that code, it *also helped writing it*.
In the process I probably wrote the zero length/empty vector case first:

```haskell
fromPull :: forall n a. N.SNatI n => P.Vec n a -> Vec n a
fromPull (P.Vec f) = case N.snat :: N.SNat n of
    N.SZ -> VNil
```

```
Pattern match(es) are non-exhaustive
In a case alternative: Patterns not matched: N.SS
```

I'm missing a non-empty list case, let's add one:

```haskell
fromPull :: forall n a. N.SNatI n => P.Vec n a -> Vec n a
fromPull (P.Vec f) = case N.snat :: N.SNat n of
    N.SZ -> VNil
    N.SS -> _h ::: _t
```

```
• Found hole: _h :: a
• Found hole: _t :: Vec n1 a
```

and so on. Let's have a *dialogue with the compiler*.

If I mess up:

```haskell
fromPull :: forall n a. N.SNatI n => P.Vec n a -> Vec n a
fromPull (P.Vec f) = case N.snat :: N.SNat n of
    N.SZ -> VNil
    N.SS -> f F.Z ::: fromPull (P.Vec f)
```

```
• Could not deduce: (n1 :: Nat) ~ ('S n1 :: Nat)
  from the context: ((n :: Nat) ~ ('S n1 :: Nat), N.SNatI n1)
```

Once you get used to typed holes, there is no going back.
Don't even try Agda with its `C-c C-a` or `C-c C-r`, that's the right kind of magic.

If you are still Agda-curious, watch [Conor McBride - SpaceMonads](https://www.youtube.com/watch?v=QojLQY5H0RI&t=2203s), but than fall back to reading
[Hasochism: the pleasure and pain of dependently typed haskell programming](https://dl.acm.org/citation.cfm?id=2503786) (similar tiling thing, but in Haskell. Google for PDF), because we write Haskell, don't we? :)
