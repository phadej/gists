---
title: Existential constraint
author: Oleg Grenrus
---

This is short gist about problem I run today into, and it's solution. It feels
that probably I'm over engineering stuff, so: please comment!

How to write something like:

```hs
instance Renderable C where
    -- 'env' is existential here: for some 'env'
    type RenderableC C m = (MonadReader env m, HasUTCTime env)

    ...
```

[Gist source](https://gist.github.com/phadej/29335981507d81b4b2f219961772de25)
