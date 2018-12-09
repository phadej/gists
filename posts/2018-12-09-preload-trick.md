---
title: Preload trick
author: Oleg Grenrus
---

This simple idea hit me when I was watching a talk about (dynamic) linking.  On
Linux (at least) we can `LD_PRELOAD` **own** shared objects, to override
symbols in other (maybe even proprietrary) shared objects. This is useful for
various purposes.  Can we do something like that in Haskell?

Not exactly, Haskell is not C. But, as the ecosystem is open, we can simply
vendor in the dependency (as source) and modify it. E.g. add some debug prints
to figure out why something doesn't work as expected? We *preload* on the
source level, i.e.  use the local version of package, not the one on the
Hackage.

However we don't need to stop there. We can mess with the vendored packages
as much as we need to. Imagine that we want to use `github` package,
but with `HsOpenSSL` for encryption, not the `tls` package.

There is a proof-of-concept implementation on
[GitHub phadej/preload-trick](https://github.com/phadej/preload-trick).
The example program needs small adjustement, because every application that
uses `HsOpenSSL` must wrap any operations involving OpenSSL with `withOpenSSL`:

```haskell
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub  as GH

#ifdef MIN_VERSION_HsOpenSSL
import           OpenSSL (withOpenSSL)
#else
withOpenSSL :: IO a -> IO a
withOpenSSL = id
#endif

main :: IO ()
main = withOpenSSL $ do
    possibleUser <- GH.executeRequest' $ GH.userInfoForR "phadej"
    print possibleUser
```

This example works out of the box with `http-client-tls`. It turns out that we
need to *mock* only three packages, if we want to use `HsOpenSSL`:

- **http-client-tls:** This is obvious. `http-client-tls` relies on `tls` for TLS implementation.
  Luckily `github` uses only `tlsManagerSettings` which can be implemented
  as oneliner using `http-client-openssl`!

  ```haskell
  module Network.HTTP.Client.TLS (tlsManagerSettings) where

  import           Network.HTTP.Client         (ManagerSettings)
  import           Network.HTTP.Client.OpenSSL (opensslManagerSettings)
  import qualified OpenSSL.Session as OpenSSL

  tlsManagerSettings :: ManagerSettings
  tlsManagerSettings = opensslManagerSettings OpenSSL.context
  ```

- **tls:** It turns out `github` doesn't use anything from `tls` directly.
  The dependency definition exists there to forbid old versions of
  `tls` (`git blame` points to commit that originally added `tls >=1.3.5` constrait.
  That version of `tls` has *Fix a bug with ECDHE based cipher where serialization*,
  I don't remember whether that is important or not, better to be safe).
  As `tls` isn't used, the mock package is an empty package (I specified a lot
  more than needed):

  ```
  cabal-version: 2.2
  name:          tls
  version:       1.4.1

  synopsis: Example of "preload" trick
  category: Example, Development
  description:
    Mock some of @tls@.

    Actually none is needed

  license:      BSD-3-Clause
  license-file: LICENSE
  author:       Oleg Grenrus <oleg.grenrus@iki.fi>
  maintainer:   Oleg Grenrus <oleg.grenrus@iki.fi>

  library
    default-language: Haskell2010
    build-depends:
      , base            ^>=4.11.1.0
  ```

- **cryptohash:** Here I can blame myself, somehow this (old) dependency
  sneaked in. I (as a maintainer of `github`) should use much lighter
  `cryptohash-sha1` package (or `cryptonite` directly, which I won't for this
  case).

  ```
  module Crypto.Hash (HMAC (..), SHA1, hmac) where

  import qualified Crypto.Hash.SHA1 as SHA1
  import           Data.ByteString  (ByteString)

  data SHA1

  newtype HMAC a = HMAC { hmacGetDigest :: ByteString }

  hmac :: ByteString -> ByteString -> HMAC SHA1
  hmac secret payload = HMAC (SHA1.hmac secret payload)
  ```

All of the mocks are very simple, as we can see: `github` uses only a very
small part of actual packages' APIs.

Is it worth it? Well... Maybe? At the very least, I learned about dependencies
of `github`. It depends on `cryptohash` for no reason.

Also if you carefully inspect *after* dependency graph, you notice that
`http-client` depends on `memory`! Looks like `http-client` uses
`Data.ByteString.Encoding` to do *Base64* encoding (in implementation of
`applyBasicAuth`).  I'd use `base64-bytestring` package for that purpose. So in
this exercise we could mock `memory` also, to only provide the *base64*
encoding. Or patch `http-client`.

In some sense this trick is *poor man's [Backpack](https://plv.mpi-sws.org/backpack/)*.
Hopefully this trick would be useful for someone, or actually hopefully
no one would ever need to rely on it.

<h3>Dependency graphs</h3>

**before**

<img src="https://raw.githubusercontent.com/phadej/preload-trick/master/deps-default.png" />

**after**

<img src="https://raw.githubusercontent.com/phadej/preload-trick/master/deps-preload.png" />
