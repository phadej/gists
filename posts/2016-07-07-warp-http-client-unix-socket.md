---
title: Warp + http-client usage over UNIX-socket. 
author: Oleg Grenrus
---

As a partial solution for help request for
[servant](https://github.com/haskell-servant/servant/issues/537)

In Haskell it's surpisingly easy to do unix-socket networking, if you ever have
done it before.  Setting things up for communication is very same as in C :P

Ultimately you'll need `openUnixConnection :: String -> IO Connection`, which
has quite trivial implementation, when you know where the building blocks are.

[Gist source](https://gist.github.com/phadej/b78d95b4107e4828119f33cebf38912f)
