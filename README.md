# Owl Opt [![Build Status](https://travis-ci.org/owlbarn/owl_opt.svg?branch=master)](https://travis-ci.org/owlbarn/owl_opt) 

Owl Opt is a gradient-based optimisation library that works well with Owl's automatic differentiation library. Using Owl Opt's ppx deriver, users can define their own record of optimization parameters without having to worry too much about book keeping. This greatly facilitates fast prototyping. 

Owl Opt currently provides several popular optimization methods (e.g. Adam, Rmsprop, Lbfgs). With the exception of LBFGS (built on top of [L-BFGS-ocaml](https://github.com/Chris00/L-BFGS-ocaml)), all methods now support both single and double precision.

Please see the [documentation](https://ocaml.xyz/owl_opt).

## Installation
### Opam
<!-- $MDX skip -->
```sh
$ opam install owl-opt
$ opam install owl-opt-lbfgs
```
### Manual
<!-- $MDX skip -->
```sh
$ dune build @install
$ dune install
```

## Example usage

The following code fragement solves a standard linear regression problem: find paramters `a` and `b` that minimises the l2 loss `sqrt((y-(a*x+b))^2)`. 
The optimisation is carried out using Adam with hyperparameters `beta1=0.99` and `beta2=0.999`.

<!-- $MDX file=examples/opt/readme.ml -->
```ocaml
module Prms = struct
   type 'a t = {a: 'a; b: 'a} [@@deriving prms]
end
open Prms

(* make an Adam optimisation module for the parameter definition Prms *)
module O = Owl_opt.D.Adam.Make (Prms)

let x = Owl.Algodiff.D.Mat.gaussian 5 1
let a = Owl.Algodiff.D.Mat.gaussian 5 5
let b = Owl.Algodiff.D.Mat.gaussian 5 1
let y = Owl.Algodiff.D.Maths.((a *@ x) + b)

(* define the objective function *)
let f _ prms = Owl.Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) 

(* define initial parameters *)
let prms0 = {a = Owl.Algodiff.D.Mat.gaussian 5 5; b = Owl.Algodiff.D.Mat.gaussian 5 1} 

(* define fixed learning rate *)
let lr = Owl_opt.Lr.(Fix 1E-4) 

(* initialise an optimisation session *)
let s = O.init ~prms0 ~beta1:0.99 ~beta2:0.999 ~lr () 

(* define stopping criteria: stop when function value is smaller than 1E-4 *)
let stop fv _s = fv < 1E-4

(* minimise objective [f] and returns final function value *)
let fv = O.min ~stop ~f s

(* final prms *)
let prms = O.prms s

(* get numeric solution *)
let solution = Prms.map ~f:Owl.Algodiff.D.unpack_arr prms
```

 
