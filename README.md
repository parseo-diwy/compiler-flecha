# Compiler $ Flecha

## TL;DR

```sh
git clone https://github.com/parseo-diwy/compiler-flecha.git
cabal build
cabal run flecha -- example.flecha
```

### Test

[Bats](https://github.com/bats-core/bats-core) needed.

```sh
git submodule init
git submodule update --recursive
bats test/flecha_test.sh
```
