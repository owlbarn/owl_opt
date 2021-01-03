module type Sig = sig
  (** user-defined record type *)
  type 'a t

  (** objective function value type *)
  type fv

  (** paramter type *)
  type prm

  (** user-defined paramter record type *)
  type prms = prm t

  (** objective function type *)
  type f = int -> prms -> fv

  (** internal state *)
  type state

  (** status *)
  type status =
    | Stop of float
    | Continue of float

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

  (** [init ?corrections ~prms0 ()] returns an initialises optimisation state for initial parmaters [prms0] *)
  val init : ?corrections:int -> prms0:prms -> unit -> state

  (** [min ~f state] minimises [f] with respect to the state [s] 

    @param factr tolerance in the termination test for the algorithm.
    The iteration will stop when
    [(f^k - f^{k+1})/max{ |f^k|, |f^{k+1}|, 1} <= factr*epsilon_float].
    Set e.g. [factr] to [1e12] for low accuracy, [1e7] for moderate
    accuracy and [1e1] for extremely high accuracy.  Setting [factr] to
    [0.] suppresses this termination test.  Default: [1e7].

    @param pgtol The iteration will stop when
    [max{ |proj g_i| : i = 0,..., n-1} <= pgtol]
    where [proj g_i] is the ith component of the projected gradient.
    Setting [pgtol] to [0.] suppresses this termination test.
    Default: [1e-5].

    @param stop An user-provided stopping criterion that terminates the optimization
    where [stop fv s] is [true].
  *)
  val min : ?pgtol:float -> ?factr:float -> ?stop:stop -> f:f -> state -> float

  (** [max ~f state] is the same as [min], but maximises [f] *)
  val max : ?pgtol:float -> ?factr:float -> ?stop:stop -> f:f -> state -> float
end
