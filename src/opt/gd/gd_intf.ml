module type Sig = sig
  type 'a t
  type prm
  type fv
  type prms = prm t
  type f = prms -> fv
  type state
  type stop = state -> bool

  val iter : state -> int
  val prms : state -> prms
  val f : state -> f
  val fv : state -> float
  val init : prms0:prms -> f:f -> unit -> state
  val min : ?stop:stop -> lr:Lr.t -> state -> state
  val max : ?stop:stop -> lr:Lr.t -> state -> state
end
