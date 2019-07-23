```haskell
minimiseFin
    :: Fin ('S n)
    -> (forall p. N.SNatI p => LEProof p n -> Fin ('S p) -> r)
    -> r
minimiseFin FZ           k = k LEZero FZ
minimiseFin (FS n@FZ {}) k =
    minimiseFin n $ \proof n' ->
    k (LESucc proof) (FS n')
minimiseFin (FS n@FS {}) k =
    minimiseFin n $ \proof n' ->
    k (LESucc proof) (FS n')

maxLE
    :: LEProof n p -> LEProof m p
    -> Either (LEProof n m) (LEProof m n)
maxLE LEZero     _          = Left LEZero
maxLE _          LEZero     = Right LEZero
maxLE (LESucc p) (LESucc q) = bimap LESucc LESucc (maxLE p q)

weakenFin :: LEProof n m -> Fin ('S n) -> Fin ('S m)
weakenFin _      FZ = FZ
weakenFin LEZero (FS n) = case n of {}
weakenFin (LESucc p) (FS n) = FS $ weakenFin p n

weakenExtract :: LEProof n m -> Extract n p -> Extract m p
weakenExtract _          Done       = Done
weakenExtract (LESucc p) (Step n e) = Step (weakenFin p n) (weakenExtract p e)
```

```haskell
minimise
    :: Extract n m
    -> (forall p. N.SNatI p => LEProof p n -> Extract p m -> r)
    -> r
-- conservative implementation, not minimising at all
-- minimise e k = k e
minimise Done       k = k LEZero (Done :: Extract 'Z 'Z)
minimise (Step n e) k =
    minimiseFin n $ \np n' ->
    minimise e    $ \ep e' ->
    case maxLE np ep of
        Left  proof -> k (LESucc ep) (Step (weakenFin proof n') e')
        Right proof -> k (LESucc np) (Step n' (weakenExtract proof e'))
```

```haskell
decode' :: forall r. (Header r, FromRecord r) => Text -> Either String [r]
decode' contents = do
    (hs,xss) <- prepare contents
    V.reifyList hs $ \hs' -> do
        trc' <- columns (unTagged (header :: Tagged r _)) hs'
        minimise trc' $ \_leproof trc ->
            forM xss $ \xs -> do
                xs' <- maybe (Left "not enough columns") Right
                    $  V.fromListPrefix xs
                fromRecord (extract trc xs')
