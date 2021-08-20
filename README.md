# cabal-script

`cabal-script` is a simple alternative to `stack script`: it allows one to directly run a single Haskell source file, as if Haskell is a scripting language.  Similar to `stack script` and `cabal` itself, `cabal-script` creates a fake package and then builds it as an executable.

Features:

- [x] Compiled for maximal performance.
- [x] Cache.
- [x] Auto cleanups of unused caches.
- [ ] Auto detection of imported packages.
- [ ] Integration with Nix.

This package is not yet on Hackage.  You can clone this repo, then run `cabal install`.

## Usage

```shell
cabal-script <HASKELL-FILE> [arguments...]

# Try demo/*.hs!
demo/Simple.hs Hello World
```

The arguments are passed to the built executable.

```haskell
#!/usr/bin/env cabal-script
-- async >= 2.2.3 && < 2.3
-- text
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import qualified Data.Text.IO as T
import System.Environment
import Control.Monad

main :: IO ()
main = do
  concurrently_ (T.putStrLn "cabal") (T.putStrLn "script")
  getArgs >>= (putStrLn . show)
```

The first group of consecutive line-style comments are considered `build-depends`, and the rest of the file is then compiled.

## Technical Details

### Cache

Cached binaries are copied to `~/.cabal/cabal-script/cache/`.  A SQLite3 database `~/.cabal/cabal-script/cache.db` maintains when a specific cached binary is used.  If a binary is not used for some time, it will be automatically deleted.
