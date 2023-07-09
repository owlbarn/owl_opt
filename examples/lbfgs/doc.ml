 module Prms = struct
    type 'a t = {a: 'a; b: 'a} [@@deriving prms]
 end
 open Prms
 (* make an Lbfgs optimisation module for the parameter definition Prms *)
 module O = Owl_opt_lbfgs.D.Make (Prms)
 let x = Owl.Algodiff.D.Mat.gaussian 5 1
 let a = Owl.Algodiff.D.Mat.gaussian 5 5
 let b = Owl.Algodiff.D.Mat.gaussian 5 1
 let y = Owl.Algodiff.D.Maths.((a *@ x) + b)
 (* define the objective function *)
 let f _ prms = Owl.Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) 
 (* define initial parameters *)
 let prms0 = {a = Owl.Algodiff.D.Mat.gaussian 5 5; b = Owl.Algodiff.D.Mat.gaussian 5 1} 
 (* initialise an optimisation session *)
 let s = O.init ~prms0 () 
 (* define stopping criteria: stop when function value is smaller than 1E-4 *)
 let stop fv _s = fv < 1E-4
 (* minimise objective function f *)
 let fv = O.min ~f s
 (* final prms *)
 let prms = O.prms s
 (* get numeric solution *)
 let solution = Prms.map ~f:Owl.Algodiff.D.unpack_arr prms
