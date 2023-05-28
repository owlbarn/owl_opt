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
 (* minimise [f] for session [s] and returns final loss *)
 let fv = O.min ~f ~stop s
 (* get optimized prms *)
 let prms = O.prms s 
