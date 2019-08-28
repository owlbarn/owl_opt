module type Sig = sig
  type 'a t
  type fv
  type prm
  type prms = prm t
  type f = prms -> fv
  type state
  type stop = state -> bool

  val iter : state -> int
  val prms : state -> prms
  val f : state -> f
  val fv : state -> float
  val init : prms0:prms -> f:f -> unit -> state
  val stop : state -> bool
  val min : ?stop:stop -> ?beta:float -> ?eps:float -> lr:Lr.t -> state -> state
  val max : ?stop:stop -> ?beta:float -> ?eps:float -> lr:Lr.t -> state -> state
end
