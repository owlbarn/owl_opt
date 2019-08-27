module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) : sig
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t

  type lr =
    | Fix of float
    | Ada of (int -> float)

  type f = prms -> fv
  type state
  type stop = state -> bool

  val lr : state -> lr
  val iter : state -> int
  val prms : state -> prms
  val f : state -> f
  val fv : state -> float
  val init : ?beta:float -> ?eps:float -> prms0:prms -> f:f -> lr:lr -> unit -> state
  val stop : state -> bool
  val min : ?stop:stop -> state -> state
  val max : ?stop:stop -> state -> state
end
