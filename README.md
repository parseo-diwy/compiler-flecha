# Compiler $ Flecha

## Install & Build

```sh
git clone https://github.com/parseo-diwy/compiler-flecha.git
cabal build
```

## Run

If you want to get [Mamarracho](https://unqpgc.github.io/files/2018s2/mamarracho.cpp) code then run

```sh
cabal run --verbose=silent flecha -- example.flecha
```

If you want to get AST representation then run

```sh
# AST according to deriving show structure
cabal run --verbose=silent flecha -- example.flecha --ast

# JSON-AST representation
cabal run --verbose=silent flecha -- example.flecha --json
```

## Tests

[Bats](https://github.com/bats-core/bats-core) needed.

```sh
git submodule init
git submodule update --recursive
bats test/tests.sh
```
