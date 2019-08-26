open Owl

module Make (P : Prms.PT) = struct
  type fv = Algodiff.D.t
  type prm = Algodiff.D.t
  type prms = prm P.t

  type x =
    { p : Algodiff.D.t
    ; m : Algodiff.D.t
    ; v : Algodiff.D.t
    }

  type xs = x P.t

  type lr =
    | Fix of float
    | Ada of (int -> float)

  type f = prms -> fv

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
    let fv = Algodiff.D.unpack_flt (f prms0) in
    let xs =
      P.map prms0 ~f:(fun p ->
          let m = Algodiff.D.(zero (primal p)) in
          let v = Algodiff.D.(zero (primal p)) in
          { p; m; v })
    in
    { xs; fv; f; lr; k = 0; beta1; beta2; eps }


  let min_update lr x m v eps = Algodiff.D.Maths.(x - (lr * m / (sqrt v + eps)))
  let max_update lr x m v eps = Algodiff.D.Maths.(x + (lr * m / (sqrt v + eps)))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) s =
    let beta1 = Algodiff.D.(F s.beta1) in
    let beta1_ = Algodiff.D.(Maths.(F 1. - beta1)) in
    let beta2 = Algodiff.D.(F s.beta2) in
    let beta2_ = Algodiff.D.(Maths.(F 1. - beta2)) in
    let eps = Algodiff.D.(F s.eps) in
    let rec run s =
      if stop s
      then s
      else (
        let t = Algodiff.D.tag () in
        let xs =
          P.map
            ~f:(fun x ->
              let p = Algodiff.D.make_reverse x.p t in
              { x with p })
            s.xs
        in
        let l = s.f (P.map ~f:(fun x -> x.p) xs) in
        Algodiff.D.(reverse_prop (F 1.) l);
        let fv = Algodiff.D.unpack_flt l in
        let xs =
          P.map
            ~f:(fun x ->
              let p = Algodiff.D.primal x.p in
              let g = Algodiff.D.adjval x.p in
              let m = Algodiff.D.Maths.((beta1 * x.m) + (beta1_ * g)) in
              let v = Algodiff.D.Maths.((beta2 * x.v) + (beta2_ * sqr g)) in
              let p =
                match s.lr with
                | Fix lr ->
                  update (Algodiff.D.pack_flt lr) p m v eps |> Algodiff.D.primal
                | Ada h ->
                  update (Algodiff.D.pack_flt (h s.k)) p m v eps |> Algodiff.D.primal
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
