open Owl

module Make (P : Owl_opt.Prms.PT) : sig
  (** objective function value type *)
  type fv = Algodiff.D.t

  (** paramter type *)
  type prm = Algodiff.D.t

  (** user-defined paramter record type *)
  type prms = prm P.t

  (** objective function type *)
  type f = int -> prms -> fv

  (** internal state *)
  type state

  (** stopping criterion function type *)
  type stop = float -> state -> bool

  (** [iter s] returns the number of iterations for optimisation state [s] *)
  val iter : state -> int

  (** [prms s] returns the optimisation parameters of state [s] *)
  val prms : state -> prms

  (** [prev_fv s] returns the last objective function value of state [s] *)
  val prev_fv : state -> float

  (** [fv_hist s] returns the history of the objective function values of state [s] up to the last objective function value (i.e., [prev_f s] is the same as [List.hd (fv_hist s)]) *)
  val fv_hist : state -> float list

  (** [init ~prms0 ()] returns an initialises optimisation state for initial parmaters [prms0] *)
  val init : prms0:prms -> unit -> state

  val min
    :  ?stop:stop
    -> ?pgtol:float
    -> ?factr:float
    -> ?corrections:int
    -> f:f
    -> state
    -> float

  val max
    :  ?stop:stop
    -> ?pgtol:float
    -> ?factr:float
    -> ?corrections:int
    -> f:f
    -> state
    -> float
end
