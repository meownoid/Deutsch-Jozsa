# Deutsch-Jozsa

Deutsch-Jozsa quantum algorithm simulation written in pure haskell.

## Build

```
stack setup
stack build
stack exec djsim
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

### Example
```
#> f x = x
function f is balansed

#> g x = 1
function g is constant

#> h x y = ((!x) ^ y) ^ 1
function h is balansed

#> f q w e r t y = q ^ w & e | r ^ t & y
function f is constant

#> quit
```
