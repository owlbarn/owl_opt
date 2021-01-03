open Owl
module AD = Algodiff.D

module P = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving prms]
end

open P
module O = Owl_opt_lbfgs.Make (P)

let () =
  let x = AD.Mat.gaussian 3 1 in
  let a = AD.Mat.gaussian 10 3 in
  let b = AD.Mat.gaussian 10 1 in
  let y = AD.Maths.((a *@ x) + b) in
  let prms0 = { a = AD.Mat.gaussian 10 3; b = AD.Mat.gaussian 10 1 } in
  let f _ prms = AD.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) in
  Printf.printf "initial: %1.10f\n%!" (f 0 prms0 |> AD.unpack_flt);
  let stop fv s =
    let i = O.iter s in
    Printf.printf "iter %i: %b %4.10f\n%!" i (i > 5) fv;
    i > 5
  in
  let s = O.init ~prms0 () in
  let fv = O.min ~stop ~f s in
  Printf.printf "final: %1.10f %1.10f\n%!" fv O.(fv_hist s |> List.rev |> List.hd)
