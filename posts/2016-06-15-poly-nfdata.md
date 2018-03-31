---
title: Polykinded NFData
author: Oleg Grenrus
---

Example how is possible to write polykinded type-classes in GHC-8.0. It's not
that bad or messy, but I'm not sure it's practical either. But at the end you have

```hs
class NFData (a :: k) where
    rnfPoly :: NFDataSig k a  -- magic happens in NFDataSig type family
```

[Gist source](https://gist.github.com/phadej/2fc066c00e33b9486e1a3e5f7767a8d7)
