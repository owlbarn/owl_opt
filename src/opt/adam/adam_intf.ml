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

  (** optimization status *)
  type status = Status.t =
    | Continue of float
    | Stop of float

  (** stopping criterion function type *)
  type stop = float -> state -> bool

  (** [iter s] returns the current iteration number of state [s] *)
  val iter : state -> int

  (** [prms s] returns the optimisation parameters of state [s] *)
  val prms : state -> prms

  (** [prev_fv s] returns the last objective function value of state [s] *)
  val prev_fv : state -> float

  (** [fv_hist s] returns the history of the objective function values of state [s] up to the last objective function value (i.e., [prev_f s] is the same as [List.hd (fv_hist s)]) *)
  val fv_hist : state -> float list

  (** [init ?(beta1=0.99) ?(beta2=0.999) ?(eps=1E-8) ~lr ~prms0 ()] initialises and returns optimisation state [s] with initial parmaters [prms0]. The hyperparamters [beta1], [beta2], and [eps] are defined {{:https://arxiv.org/pdf/1412.6980.pdf}here}. *)
  val init
    :  ?beta1:float
    -> ?beta2:float
    -> ?eps:float
    -> lr:Lr.t
    -> prms0:prms
    -> unit
    -> state

  (** [stop fv s] is the default stopping criterion, which prints out the iteration and objective function value at each optimisation iteration and terminates when the objective function value goes below 1E-4 *)
  val stop : float -> state -> bool

  (** [min_step ?(stop=stop) ~f s] computes the function value [fv] of optimization state [s] with parameters (i.e., [f (iter s) (prms s)] returns [fv]). If the stopping criterion is reached (i.e. [stop fv s] is [true]), then return [Stop fv] and no optimization is performed. Otherwise, minizie [f] by updating the parameters of [s] one step (in place) according to Adam and returns [Continue fv]. Here, [stop fv s] is a callback function that can be used to specify the termination criterion and print out intermediate function values. *)
  val min_step : ?stop:stop -> f:f -> state -> status

  (** [max_step ?(stop=stop) ~f ~lr] is similar to [min_step] but maximises f. *)
  val max_step : ?stop:stop -> f:f -> state -> status

  (** [min ?(stop=stop) ~f status] iteratively minimises [f] using Adam until the stopping criterion is reached (i.e., [stop fv s] returns [true]), then returns the final function value [fv]. See [min_step] for details on [stop]. *)
  val min : ?stop:stop -> f:f -> state -> float

  (** [max ?(stop=stop) ~f ~lr s] is similar to [min] but maximises f. *)
  val max : ?stop:stop -> f:f -> state -> float
end
