# Deutsch-Jozsa

Deutsch-Jozsa quantum algorithm simulation written in pure haskell.

## Build

Build and install to specified path:

```
cabal build
cabal install --prefix=PATH
```

## Usage

Run and provide some constant or balanced functions in format
```
<name> <args> = <expr>
```
Operators:
* ! - unary not
* & - and
* | - or
* ^ - xor

### Examples
```
f x = x
g x = 1
h x y = ((!x) ^ y) ^ 1
```
