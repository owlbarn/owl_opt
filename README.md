# Owl Opt 

Owl Opt is a gradient-based optimisation library that works well with Owl's automatic differentiation library. Using Owl Opt's ppx deriver, users can define their own heterogeneous optimization parameters without having to worry too much about book keeping. This greatly facilitates fast prototyping. Owl Opt currently provides several popular optimization methods (e.g. Adam, Rmsprop, Lbfgs). With the exception of LBFGS (built ontop of [L-BFGS-ocaml](https://github.com/Chris00/L-BFGS-ocaml)), all methods now support both single and double precisions.

## Installation
```sh
$ dune build @install
$ dune install
```
## Examples 

```sh
$ dune exec examples/test_adam.exe
step: 7670 | loss: 0.003270402
final loss: 0.000704
```
 
