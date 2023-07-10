(* This is a modified version of the OCaml bindings to L-BFGS-B written by:

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/ 

    Please refer to https://github.com/Chris00/L-BFGS-ocaml for details.
*)

open Bigarray
open Printf

type vec = (float, float64_elt, c_layout) Array1.t
type wvec = (float, float64_elt, fortran_layout) Array1.t (* working vectors *)

(* FORTRAN 77 "integer" is mandated to be half the size of DOUBLE PRECISION  *)
type 'l int_vec = (int32, int32_elt, 'l) Array1.t
type wint_vec = fortran_layout int_vec (* working int vectors *)

type lbfgs_status =
  | Stop of float
  | Continue of float

external setulb
  :  n:int
  -> m:int
  -> (* BEWARE: C-style offsets *)
     c_ofsx:int
  -> x:vec
  -> c_ofsl:int
  -> l:vec
  -> c_ofsu:int
  -> u:vec
  -> nbd:'l int_vec
  -> f:float
  -> g:vec
  -> factr:float
  -> pgtol:float
  -> wa:wvec
  -> (* dim: depends on FORTRAN version; see [wa_min_size] *)
     iwa:wint_vec
  -> (* dim: 3n *)
     task:Bytes.t
  -> (* length: 60 *)
     iprint:int
  -> csave:Bytes.t
  -> (* length: 60 (working string) *)
     lsave:wint_vec
  -> (* logical working array of dimension 4 *)
     isave:wint_vec
  -> (* dim: 44 *)
     dsave:wvec
  -> (* dim: 29 *)
     float
  = "ocaml_lbfgs_setulb_bc" "ocaml_lbfgs_setulb"

(* Return the value of the function 'f'. *)

let max i j = if (i : int) > j then i else j (* specialized version *)

let min i j = if (i : int) < j then i else j

type work =
  { n : int
  ; l : vec
  ; u : vec
  ; nbd : c_layout int_vec
  ; corrections : int
  ; (* dimension of the problem used to create this work *)
    wa : wvec
  ; iwa : wint_vec
  ; task : Bytes.t
  ; csave : Bytes.t
  ; lsave : wint_vec
  ; isave : wint_vec
  ; dsave : wvec
  ; mutable fv : float
  }

let wvec ty n = Array1.create ty fortran_layout n

(* The work space [wa] changes with the L-BFGS-B version. *)
let coef_n1 m = (2 * m) + 5
let coef_n0 m = m * ((11 * m) + 8)
let wa_min_size n m = (coef_n1 m * n) + coef_n0 m
let wa_n_of_size s m = max 0 ((s - coef_n0 m) / coef_n1 m)

let unsafe_work ~l ~u ~nbd n corrections =
  { n
  ; corrections
  ; wa = wvec float64 (wa_min_size n corrections)
  ; iwa = wvec int32 (3 * n)
  ; (* FORTRAN requires the strings to be initialized with spaces: *)
    task = Bytes.make 60 ' '
  ; csave = Bytes.make 60 ' '
  ; lsave = wvec int32 4
  ; isave = wvec int32 44
  ; dsave = wvec float64 29
  ; l
  ; u
  ; nbd
  ; fv = nan
  }


let set_start s =
  (* No final '\000' for FORTRAN *)
  Bytes.set s 0 'S';
  Bytes.set s 1 'T';
  Bytes.set s 2 'A';
  Bytes.set s 3 'R';
  Bytes.set s 4 'T'


let restart w = set_start w.task

(* Check that the work is large enough for the current problem. *)
let check_work n work =
  let corrections = work.corrections in
  if Array1.dim work.wa < wa_min_size n corrections
     || Array1.dim work.iwa < 3 * n
  then (
    let n_min =
      min (wa_n_of_size (Array1.dim work.wa) corrections) (Array1.dim work.iwa / 3)
    in
    failwith
      (sprintf
         "Lbfgs.min: dim of work too small for problem size n = %i, valid n <= %i"
         n
         n_min))


exception Abnormal of float * string

let is_space c = c = ' ' || c = '\t' || c = '\n'

let rec strip_final_spaces s i =
  if i <= 0
  then ""
  else if is_space (Bytes.get s i)
  then strip_final_spaces s (i - 1)
  else Bytes.sub_string s 0 i


let extract_c_string s =
  try strip_final_spaces s (Bytes.index s '\000') with
  | Not_found -> strip_final_spaces s (Bytes.length s - 1)


(* Distinguish it from the first to avoid questionning a workspace not
   being used.  This information is only available after calling [f_df]
   when task=FG. *)
type state = work

let is_nan x = (x : float) <> x
let empty_vec = Array1.create float64 c_layout 0

let nbd_of_lu n (lopt : vec option) (uopt : vec option) =
  let nbd = Array1.create int32 c_layout n in
  match lopt, uopt with
  | None, None ->
    Array1.fill nbd 0l;
    empty_vec, empty_vec, nbd
  | Some l, None ->
    for i = 0 to n - 1 do
      if is_nan l.{i} || l.{i} = infinity
      then invalid_arg (sprintf "Lbfgs.min: l.{%i} = %f => empty domain" i l.{i});
      nbd.{i} <- (if l.{i} = neg_infinity then 0l else 1l)
    done;
    l, empty_vec, nbd
  | None, Some u ->
    for i = 0 to n - 1 do
      if is_nan u.{i} || u.{i} = neg_infinity
      then invalid_arg (sprintf "Lbfgs.min: u.{%i} = %f => empty domain" i u.{i});
      nbd.{i} <- (if u.{i} = infinity then 0l else 3l)
    done;
    empty_vec, u, nbd
  | Some l, Some u ->
    for i = 0 to n - 1 do
      if is_nan l.{i} || l.{i} = infinity
      then invalid_arg (sprintf "Lbfgs.min: l.{%i} = %f => empty domain" i l.{i});
      if is_nan u.{i} || u.{i} = neg_infinity
      then invalid_arg (sprintf "Lbfgs.min: u.{%i} = %f => empty domain" i u.{i});
      nbd.{i}
        <- (if l.{i} = neg_infinity
           then if u.{i} = infinity then 0l else 3l
           else if u.{i} = infinity
           then 1l
           else 2l)
    done;
    l, u, nbd


let start ?(corrections = 10) ?l ?u n =
  if corrections <= 0 then failwith "Lbfgs.work: corrections must be > 0";
  if n <= 0 then failwith "Lbfgs.work: n must be > 0";
  let l, u, nbd = nbd_of_lu n l u in
  let work = unsafe_work ~l ~u ~nbd n corrections in
  set_start work.task;
  work


let step ?(factr = 1E7) ?(pgtol = 1E-5) ~work ~l ~u ~nbd prev_f x g =
  setulb
    ~n:work.n
    ~m:work.corrections
    ~c_ofsx:0
    ~x
    ~c_ofsl:0
    ~l
    ~c_ofsu:0
    ~u
    ~nbd
    ~f:prev_f
    ~g
    ~factr
    ~pgtol
    ~wa:work.wa
    ~iwa:work.iwa
    ~task:work.task
    ~iprint:(-1)
    ~csave:work.csave
    ~lsave:work.lsave
    ~isave:work.isave
    ~dsave:work.dsave


let min ?(factr = 1e7) ?(pgtol = 1e-5) ~work f_df (x : vec) (g : vec) =
  let n = Array1.dim x in
  check_work n work;
  let rec run f =
    let f = step ~factr ~pgtol ~work ~l:work.l ~u:work.u ~nbd:work.nbd f x g in
    match Bytes.get work.task 0 with
    | 'F' (* FG *) ->
      let f = f_df x g in
      run f
    | 'C' (* CONV *) ->
      (* the termination test in L-BFGS-B has been satisfied. *)
      Stop f
    | 'A' (* ABNO *) -> raise (Abnormal (f, extract_c_string work.task))
    | 'E' (* ERROR *) -> invalid_arg (extract_c_string work.task)
    | 'N' (* NEW_X *) ->
      work.fv <- f;
      Continue f
    | _ -> assert false
  in
  run work.fv


let max ?factr ?pgtol ~work f_df (x : vec) (g : vec) =
  (* Play with -f *)
  let neg_f_df (x : vec) (g : vec) =
    let v = f_df x g in
    for i = 0 to Array1.dim g - 1 do
      g.{i} <- -.g.{i}
    done;
    -.v
  in
  match min ?factr ?pgtol ~work neg_f_df x g with
  | Continue v -> Continue (-.v)
  | Stop v -> Stop (-.v)
