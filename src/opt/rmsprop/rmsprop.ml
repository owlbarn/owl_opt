open Base

module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t

  type x =
    { p : AD.t
    ; v : AD.t
    }

  type xs = x P.t
  type f = int -> prms -> fv

  type state =
    { mutable xs : xs
    ; mutable fv_hist : float list
    ; mutable k : int
    ; beta : float
    ; lr : Lr.t
    }

  type stop = float -> state -> bool

  type status = Status.t =
    | Continue of float
    | Stop of float

  let iter s = s.k
  let prms s = P.map ~f:(fun x -> x.p) s.xs

  let prev_fv s =
    match List.hd s.fv_hist with
    | Some fv -> fv
    | None -> failwith "there has not been any function evaluations"


  let fv_hist s = List.rev s.fv_hist

  let rec copy x =
    match AD.primal x with
    | AD.F x -> AD.F x
    | AD.Arr y -> AD.Arr AD.A.(copy y)
    | _ -> copy x


  let init ?(beta = 0.9) ~lr ~prms0 () =
    let xs =
      P.map
        ~f:(fun p ->
          let v = p |> copy |> AD.reset_zero in
          { p; v })
        prms0
    in
    { xs; fv_hist = []; k = 0; beta; lr }


  let min_update lr x g v = AD.Maths.(x - (lr * g / sqrt v))
  let max_update lr x g v = AD.Maths.(x + (lr * g / sqrt v))

  let stop fv s =
    if s.k % 10 = 0 then Stdio.printf "\rstep: %i | loss: %4.9f%!" s.k fv;
    Float.(fv < 1E-3)


  let step update ?(stop = stop) ~f s =
    let k = s.k in
    let t = AD.tag () in
    let xs =
      P.map
        ~f:(fun x ->
          let p = AD.make_reverse x.p t in
          { x with p })
        s.xs
    in
    let l = f k (P.map ~f:(fun x -> x.p) xs) in
    let fv = AD.unpack_flt l in
    if stop fv s
    then Stop fv
    else (
      AD.(reverse_prop (F 1.) l);
      let xs =
        P.map
          ~f:(fun x ->
            let p = AD.primal x.p in
            let g = AD.adjval x.p in
            let beta_ = AD.F (1. -. s.beta) in
            let v = AD.Maths.((F s.beta * x.v) + (beta_ * sqr g)) in
            let p =
              match s.lr with
              | Lr.Fix lr -> update (AD.pack_flt lr) p g v |> AD.primal
              | Lr.Ada h -> update (AD.pack_flt (h s.k)) p g v |> AD.primal
            in
            { p; v })
          xs
      in
      s.xs <- xs;
      s.k <- Int.succ k;
      s.fv_hist <- fv :: s.fv_hist;
      Continue fv)


  let rec optimise update ?(stop = stop) ~f s =
    match step ~stop update ~f s with
    | Stop fv -> fv
    | Continue _ -> optimise update ~stop ~f s


  let min_step = step min_update
  let max_step = step max_update
  let min = optimise min_update
  let max = optimise max_update
end
