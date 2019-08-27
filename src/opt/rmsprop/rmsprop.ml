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
    ; beta : float
    ; eps : float
    }

  type stop = state -> bool

  let lr s = s.lr
  let iter s = s.k
  let prms s = P.map ~f:(fun x -> x.p) s.xs
  let f s = s.f
  let fv s = s.fv

  let init ?(beta = 0.9) ?(eps = 1E-8) ~prms0 ~f ~lr () =
    let fv = AD.unpack_flt (f prms0) in
    let xs =
      P.map
        ~f:(fun p ->
          let v = AD.copy_primal' p in
          AD.Mat.reset v;
          { p; v })
        prms0
    in
    { xs; fv; f; lr; k = 0; beta; eps }


  let min_update lr x g v eps = AD.Maths.(x - (lr * g / (sqrt v + eps)))
  let max_update lr x g v eps = AD.Maths.(x + (lr * g / (sqrt v + eps)))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) s =
    let beta = AD.(F s.beta) in
    let beta_ = AD.(Maths.(F 1. - beta)) in
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
              let v = AD.Maths.((beta * x.v) + (beta_ * sqr g)) in
              let p =
                match s.lr with
                | Fix lr -> update (AD.pack_flt lr) p g v eps |> AD.primal
                | Ada h -> update (AD.pack_flt (h s.k)) p g v eps |> AD.primal
              in
              { p; v })
            xs
        in
        let s = { s with xs; k = succ s.k; fv } in
        run s)
    in
    run s


  let min = optimise min_update
  let max = optimise max_update
end
