open Owl

module Make (P : Owl_opt.Prms.PT) : sig
  type fv = Algodiff.D.t
  type prm = Algodiff.D.t
  type prms = prm P.t
  type f = prms -> fv
  type state
  type stop = state -> bool

  val iter : state -> int
  val fv : state -> float
  val prms : state -> prms
  val f : state -> f
  val init : prms0:prms -> f:f -> unit -> state

  val min
    :  ?stop:stop
    -> ?pgtol:float
    -> ?factr:float
    -> ?corrections:int
    -> state
    -> state

  val max
    :  ?stop:stop
    -> ?pgtol:float
    -> ?factr:float
    -> ?corrections:int
    -> state
    -> state
end
