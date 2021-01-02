open Base

module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type prm = AD.t
  type fv = AD.t
  type prms = prm P.t
  type f = int -> prms -> fv

  type state =
    { mutable prms : prms
    ; mutable fv_hist : float list
    ; mutable k : int
    ; lr : Lr.t
    }

  type status = Status.t =
    | Continue of float
    | Stop of float

  type stop = float -> state -> bool

  let iter s = s.k
  let prms s = s.prms

  let prev_fv s =
    match List.hd s.fv_hist with
    | Some fv -> fv
    | None -> failwith "there has not been any function evaluations"


  let fv_hist s = List.rev s.fv_hist
  let init ~prms0 ~lr () = { prms = prms0; fv_hist = []; k = 0; lr }
  let min_update lr x g = AD.Maths.(x - (lr * g))
  let max_update lr x g = AD.Maths.(x + (lr * g))

  let stop fv s =
    if s.k % 10 = 0 then Stdio.printf "\rstep: %i | loss: %4.9f%!" s.k fv;
    Float.(fv < 1E-3)


  let step update ?(stop = stop) ~f s =
    let k = s.k in
    let t = AD.tag () in
    let prms = P.map ~f:(fun x -> AD.make_reverse x t) s.prms in
    let l = f k prms in
    let fv = AD.unpack_flt l in
    if stop fv s
    then Stop fv
    else (
      AD.(reverse_prop (F 1.) l);
      let prms =
        P.map
          ~f:(fun prm ->
            let x = AD.primal prm in
            let g = AD.adjval prm in
            match s.lr with
            | Lr.Fix lr -> update (AD.pack_flt lr) x g |> AD.primal
            | Lr.Ada h -> update (AD.pack_flt (h s.k)) x g |> AD.primal)
          prms
      in
      s.fv_hist <- fv :: s.fv_hist;
      s.k <- Int.succ k;
      s.prms <- prms;
      Continue fv)


  let rec optimise update ?(stop = stop) ~f s =
    match step update ~stop ~f s with
    | Stop fv -> fv
    | Continue _ -> optimise update ~stop ~f s


  let min_step = step min_update
  let max_step = step max_update
  let min = optimise min_update
  let max = optimise max_update
end
