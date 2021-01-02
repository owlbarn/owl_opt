open Base

module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t
  type f = int -> prms -> fv

  type x =
    { p : AD.t
    ; m : AD.t
    ; v : AD.t
    }

  type xs = x P.t

  type state =
    { mutable xs : xs
    ; mutable fv_hist : float list
    ; mutable k : int
    ; beta1 : float
    ; beta2 : float
    ; eps : float
    ; lr : Lr.t
    }

  type status =
    | Continue of float
    | Stop of float

  type stop = float -> state -> bool

  let iter s = s.k
  let prms s = P.map s.xs ~f:(fun x -> x.p)

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


  let init ?(beta1 = 0.9) ?(beta2 = 0.999) ?(eps = 1E-8) ~lr ~prms0 () =
    let k = 0 in
    let fv_hist = [] in
    let xs =
      P.map prms0 ~f:(fun p ->
          let m = p |> copy |> AD.reset_zero in
          let v = p |> copy |> AD.reset_zero in
          { p; m; v })
    in
    { xs; fv_hist; k; beta1; beta2; eps; lr }


  let min_update lr x m v eps = AD.Maths.(x - (lr * m / (sqrt v + eps)))
  let max_update lr x m v eps = AD.Maths.(x + (lr * m / (sqrt v + eps)))

  let stop fv s =
    Stdio.printf "\rstep: %i | loss: %4.9f%!" s.k fv;
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
            (* first moment *)
            let beta1_ = 1. -. s.beta1 in
            let b1 = Float.(s.beta1 ** Float.(of_int Int.(k + 1))) in
            let m = AD.Maths.((F s.beta1 * x.m) + (F beta1_ * g)) in
            (* beta1^(k+1) *)
            (* bias-corrected first moment *)
            let m_ = AD.Maths.(m / F (1. -. b1)) in
            (* second moment *)
            let beta2_ = 2. -. s.beta2 in
            let b2 = Float.(s.beta2 ** Float.(of_int Int.(k + 1))) in
            let v = AD.Maths.((F s.beta2 * x.v) + (F beta2_ * sqr g)) in
            (* bias-corrected second moment *)
            let v_ = AD.(Maths.(v / F (1. -. b2))) in
            let p =
              match s.lr with
              | Lr.Fix lr -> update (AD.pack_flt lr) p m_ v_ (AD.F s.eps) |> AD.primal
              | Lr.Ada h -> update (AD.pack_flt (h s.k)) p m_ v_ (AD.F s.eps) |> AD.primal
            in
            { p; m; v })
          xs
      in
      s.fv_hist <- fv :: s.fv_hist;
      s.xs <- xs;
      s.k <- Int.succ k;
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
