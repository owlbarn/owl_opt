open Owl

type prm = Algodiff.D.t
type fv = Algodiff.D.t

module Make (P : Prms.PT) = struct
  type prms = prm P.t

  type lr =
    | Fix of float
    | Ada of (int -> float)

  type f = prms -> fv

  type state =
    { prms : prms
    ; f : f
    ; fv : float
    ; lr : lr
    ; k : int
    }

  type stop = state -> bool

  let lr s = s.lr
  let iter s = s.k
  let prms s = s.prms
  let f s = s.f
  let fv s = s.fv

  let init ~prms0 ~f ~lr () =
    let fv = Algodiff.D.unpack_flt (f prms0) in
    { prms = prms0; fv; f; lr; k = 0 }


  let min_update lr x g = Algodiff.D.Maths.(x - (lr * g))
  let max_update lr x g = Algodiff.D.Maths.(x + (lr * g))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) s =
    let rec run s =
      if stop s
      then s
      else (
        let t = Algodiff.D.tag () in
        let prms = P.map ~f:(fun x -> Algodiff.D.make_reverse x t) s.prms in
        let l = s.f prms in
        Algodiff.D.(reverse_prop (F 1.) l);
        let fv = Algodiff.D.unpack_flt l in
        let prms =
          P.map
            ~f:(fun prm ->
              let x = Algodiff.D.primal prm in
              let g = Algodiff.D.adjval prm in
              match s.lr with
              | Fix lr -> update (Algodiff.D.pack_flt lr) x g |> Algodiff.D.primal
              | Ada h -> update (Algodiff.D.pack_flt (h s.k)) x g |> Algodiff.D.primal)
            prms
        in
        let s = { s with prms; k = succ s.k; fv } in
        run s)
    in
    run s


  let min = optimise min_update
  let max = optimise max_update
end
