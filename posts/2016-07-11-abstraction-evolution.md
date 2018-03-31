---
title: Different levels of abstraction
author: Oleg Grenrus
---

[Inspired by a Next Level MTL - George Wilson - BFPG 2016-06 talk](https://www.youtube.com/watch?v=GZPup5Iuaqw)
and by
[Keynote from Lennart Augustsson - Giving Haskell Types to a Relational Algebra Library in C++](https://skillsmatter.com/skillscasts/6683-keynote-from-lennart-augustsson)

From
```hs
dashboardData :: Manager -> GitHubAuth -> YSState
    -> IO $ Either (Either GitHubError YSError) $ Vector (GitHubUser, YSUser)
dashboardData = {- eight lines of boilerplate #-}
```

to
```hs
dashboardData''' :: (MonadGitHub m, MonadYS m) => m (Vector (GitHubUser, YSUser))
dashboardData''' = mergeUsers <$> getGitHubUsers <*> getYsUsers
```

or something with even more general implementation.


[Gist source](https://gist.github.com/phadej/fedba3ca4492296af61726be7a8a028c)
