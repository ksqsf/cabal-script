# cabal-script

`cabal-script` is a simple alternative to `stack script`: it allows one to directly run a single Haskell source file, as if Haskell is a scripting language.  Similar to `stack script` and `cabal` itself, `cabal-script` creates a fake package and then builds it as an executable.

## Usage

```shell
cabal-script <HASKELL-FILE> [arguments...]

# Try demo/*.hs!
demo/Simple.hs Hello World
```

The arguments are passed to the built executable.  To clean up, do `rm -rf /tmp/cabal-script-*`.

## Cache

Not implemented yet.  Idea: only cache the built binaries.
