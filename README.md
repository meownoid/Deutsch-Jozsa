# Deutsch-Jozsa

This is a [Deutsch-Jozsa quantum algorithm](https://en.wikipedia.org/wiki/Deutsch–Jozsa_algorithm)
simulation written in pure haskell.

## Build

```shell script
stack setup
stack build
```

## Run

```shell script
stack exec dj-simulation
```

## Usage

This program provides an interactive environment where you can enter your custom
boolean functions. Four operators are supported:

* ! - unary not
* & - and
* | - or
* ^ - xor

You can write function definitions like following.
```
<name> <args> = <expr>
```

Use `quit` to exit.

## Example

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
