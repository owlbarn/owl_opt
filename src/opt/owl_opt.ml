(** {1 Workflow } *)

(**  + define your optimisation parameter record and apply [@@deriving prms]
     + make an optimisation module [O] using your favourite algorithm functor
     + define an objective function that takes as input your parameter type
     + define initial parameters
     + define learning rates with {!module:Lr}
     + initialise optimisation session [s] with [O.init]
     + define stoping criterion function [stop: state -> bool] 
     + minimise the optimisation session
*)

(** {1 Example} *)

(**
  {[
   module Prms = struct
      type 'a t = {a: 'a; b: 'a} [@@deriving prms]
   end
   (* make an Adam optimisation module for the parameter definition Prms *)
   module O = Owl_opt.D.Adam.Make (Prms)

   (* define the objective function *)
   let f prms = Owl.Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) 

   (* define initial parameters *)
   let prms0 = {a = Owl.Algodiff.D.Mat.gaussian 5 5; b = Owl.Algodiff.D.gaussian 5 1} 

   (* define fixed learning rate *)
   let lr = Owl_opt.Lr.(Fix 1E-4) 

   (* initialise an optimisation session *)
   let s0 = O.init ~prms0 ~f () 

   (* define stopping criteria: stop when function value is smaller than 1E-4 *)
   let stop s = O.(fv s) < 1E-4

   (* minimise objective function f *)
   let s = O.min ~stop ~beta1:0.99 ~beta2:0.999 ~lr f

   (* final objective function value *)
   let c = O.fv s

   (* final prms *)
   let prms = O.prms s 
  ]}
*)

(** {1 Single-precision module} *)

module S = struct
  (** Single-precision vanilla gradient descent (see: {!module: Owl_opt.S.Gd.Make}). *)
  module Gd = Gd_s

  (** Single-precision Adam (see: {!module:Owl_opt.S.Adam.Make}). *)
  module Adam = Adam_s

  (** Single-precision Rmsprop (see: {!module:Owl_opt.S.Rmsprop.Make}). *)
  module Rmsprop = Rmsprop_s
end

(** {1 Double-precision module} *)

module D = struct
  (** Double-precision vanilla gradient descent (see: {!module:Owl_opt.D.Gd.Make}). *)
  module Gd = Gd_d

  (** Double-precision Adam (see: {!module:Owl_opt.D.Adam.Make}). *)
  module Adam = Adam_d

  (** Double-precision Rmsprop (see: {!module:Owl_opt.D.Rmsprop.Make}). *)
  module Rmsprop = Rmsprop_d
end

(** {1 Prm module type} *)

module Prms = Prms

(** {1 Learning rate module} *)

module Lr = Lr 
