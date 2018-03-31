---
title: Monad classes
author: Oleg Grenrus
---

A short example, on how to use monad classes to *program against the interface*.

```hs
class Monad m => MonadLog m where
   log :: Text -> m ()

class Monad m => MonadProgress m where
    reportProgress :: Done -> All -> m ()
```

is very same as passing an object implementing `LoggerInterface` or
`ProgressInterface`.

[Gist source](https://gist.github.com/phadej/897fc3a9bee237938f134eb34db7033b)
