module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t
  type f = prms -> fv

  (* learning rate type *)
  include Lr

  type x =
    { p : AD.t
    ; m : AD.t
    ; v : AD.t
    }

  type xs = x P.t

  type state =
    { xs : xs
    ; f : f
    ; fv : float
    ; lr : lr
    ; k : int
    ; beta1 : float
    ; beta2 : float
    ; eps : float
    }

  type stop = state -> bool

  let lr s = s.lr
  let iter s = s.k
  let prms s = P.map s.xs ~f:(fun x -> x.p)
  let f s = s.f
  let fv s = s.fv

  let init ?(beta1 = 0.9) ?(beta2 = 0.999) ?(eps = 1E-8) ~prms0 ~f ~lr () =
    let fv = AD.unpack_flt (f prms0) in
    let xs =
      P.map prms0 ~f:(fun p ->
          let m = AD.copy_primal' p in
          AD.Mat.reset m;
          let v = AD.copy_primal' p in
          AD.Mat.reset v;
          { p; m; v })
    in
    { xs; fv; f; lr; k = 0; beta1; beta2; eps }


  let min_update lr x m v eps = AD.Maths.(x - (lr * m / (sqrt v + eps)))
  let max_update lr x m v eps = AD.Maths.(x + (lr * m / (sqrt v + eps)))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) s =
    let beta1 = AD.(F s.beta1) in
    let beta1_ = AD.(Maths.(F 1. - beta1)) in
    let beta2 = AD.(F s.beta2) in
    let beta2_ = AD.(Maths.(F 1. - beta2)) in
    let eps = AD.(F s.eps) in
    let rec run s =
      if stop s
      then s
      else (
        let t = AD.tag () in
        let xs =
          P.map
            ~f:(fun x ->
              let p = AD.make_reverse x.p t in
              { x with p })
            s.xs
        in
        let l = s.f (P.map ~f:(fun x -> x.p) xs) in
        AD.(reverse_prop (F 1.) l);
        let fv = AD.unpack_flt l in
        let xs =
          P.map
            ~f:(fun x ->
              let p = AD.primal x.p in
              let g = AD.adjval x.p in
              let m = AD.Maths.((beta1 * x.m) + (beta1_ * g)) in
              let v = AD.Maths.((beta2 * x.v) + (beta2_ * sqr g)) in
              let p =
                match s.lr with
                | Fix lr -> update (AD.pack_flt lr) p m v eps |> AD.primal
                | Ada h -> update (AD.pack_flt (h s.k)) p m v eps |> AD.primal
              in
              { p; m; v })
            xs
        in
        let s = { s with xs; k = succ s.k; fv } in
        run s)
    in
    run s


  let min = optimise min_update
  let max = optimise max_update
end
