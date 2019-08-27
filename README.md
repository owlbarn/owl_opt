# Owl Opt 

Owl Opt is a gradient-based optimisation module that works well with Owl's automatic differentiation library. With a simple and easy to use ppx deriver, Owl Opt allows the user to define heterogeneous optimization parameters without having to worry too much about book keeping. This greatly facilitate fast prototyping without giving too much on performance. It currently provides several popular optimization methods (e.g. Adam, Rmsprop, Lbfgs) and support both single and double precision for most methods. 

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
 
