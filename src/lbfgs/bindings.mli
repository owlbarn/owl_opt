(** Binding to
    {{:http://users.eecs.northwestern.edu/~nocedal/lbfgsb.html}L-BFGS-B}.
    These is a limited-memory quasi-Newton code for unconstrained and
    for bound-constrained optimization.

    This is a modified version of the OCaml bindings written by

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

    Please refer to https://github.com/Chris00/L-BFGS-ocaml for details.

    The authors of the original FORTRAN code expect that if you use
    their software in a publication, you quote one of these references:

    - R. H. Byrd, P. Lu and J. Nocedal. A Limited Memory Algorithm for
    Bound Constrained Optimization, (1995), SIAM Journal on
    Scientific and Statistical Computing , 16, 5, pp. 1190-1208.
    - C. Zhu, R. H. Byrd and J. Nocedal. L-BFGS-B: Algorithm 778:
    L-BFGS-B, FORTRAN routines for large scale bound constrained
    optimization (1997), ACM Transactions on Mathematical Software,
    Vol 23, Num. 4, pp. 550-560.
    - J.L. Morales and J. Nocedal. L-BFGS-B: Remark on Algorithm 778:
    L-BFGS-B, FORTRAN routines for large scale bound constrained
    optimization (2011), to appear in ACM Transactions on
    Mathematical Software.

    @version %%VERSION%%
*)

open Bigarray

(** Represent the memory space needed to solve a minimization problem.
    It is usually allocated automatically but it is possible to
    do it manually to, say, allocate it once only before a loop. *)
type work

(** [Abnormal(f, msg)] is raised if the routine terminated abnormally
    without being able to satisfy the termination conditions.  In such
    an event, the variable [x] (see {!F.min}) will contain the current
    best approximation found and [f] is the value of the target
    function at [x].  [msg] is a message containing additional
    information (returned by the original FORTRAN code).

    If the error message is not precise enough, it is recommended to
    turn printing on to understand what is the problem. *)
exception Abnormal of float * string

(** Holds informations on the current state of the computation that
    can help to decide whether to stop. *)
type state

(** Vectors. *)
type vec = (float, float64_elt, c_layout) Array1.t

(** Lbfgs Status *)
type lbfgs_status =
  | Stop of float
  | Continue of float

(** [min f_df x df] minimises the function [f] given by
    [f_df].  [x] is an initial estimate of the solution vector. On
    termination, [x] will contain the next approximation found and 
    [df] will contain the latest gradient. [min f_df x df] should 
    be called iteratively to find the minimum of [f].
    [f_df x df] is a function that computes f(x) and its gradiant f'(x),
    returns f(x) and stores f'(x) in [df].  The [x] passed to
    [f_df x df] is physically equal to the [x] given in [min f_df x].  Can
    raise {!Abnormal}.

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

    *)
val min
  :  ?factr:float
  -> ?pgtol:float
  -> work:work
  -> (vec -> vec -> float)
  -> vec
  -> vec
  -> lbfgs_status

(** [max f_df x] maximises the function [f] given by [f_df].  
    [x] is an initial estimate of the solution vector.  This
    function is provided for convenience and calls {!F.min} to which
    the reader is referred for further explanations. *)
val max
  :  ?factr:float
  -> ?pgtol:float
  -> work:work
  -> (vec -> vec -> float)
  -> vec
  -> vec
  -> lbfgs_status

(** [start n] allocate the work space for a problem of size at most [n].

    @param l lower bound for each component of the vector [x].  Set
    [l.(i)] to [neg_infinity] to indicate that no lower bound is desired.
    Default: no lower bounds.

    @param u upper bound for each component of the vector [x].  Set
    [u.(i)] to [infinity] to indicate that no upper bound is desired.
    Default: no upper bounds.

    @param corrections maximum number of variable metric corrections
    used to define the limited memory matrix.  Values < 3 are not
    recommended, and large values of [corrections] can result in
    excessive computing time.  The range 3 <= [corrections] <= 20 is
    recommended.  Default: [10].  This value in called [M] in L-BFGS-B
    debugging output. *)
val start : ?corrections:int -> ?l:vec -> ?u:vec -> int -> work

(** [restart state] restarts the L-BFGS-B optimization. *)
val restart : work -> unit
