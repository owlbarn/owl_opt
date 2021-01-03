open Owl
module AD = Algodiff.D

module P = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving prms]
end

module O = Owl_opt.D.Gd.Make (P)
open P

let () =
  let x = AD.Mat.gaussian 3 1 in
  let a = AD.Mat.gaussian 5 3 in
  let b = AD.Mat.gaussian 5 1 in
  let y = AD.Maths.((a *@ x) + b) in
  let prms0 = { a = AD.Mat.gaussian 5 3; b = AD.Mat.gaussian 5 1 } in
  let f _ prms = AD.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) in
  let lr = Owl_opt.Lr.Fix 1E-4 in
  let init_fv = f 0 prms0 |> AD.unpack_flt in
  let s = O.init ~lr ~prms0 () in
  let stop fv s =
    let k = O.iter s in
    Printf.printf "\riter: %i | loss: %4.6f" k fv;
    fv < 1E-3
  in
  let fv = O.min ~f ~stop s in
  assert (fv = (f O.(iter s) O.(prms s) |> AD.unpack_flt));
  assert (init_fv = List.hd (O.fv_hist s));
  Printf.printf "\nfinal loss: %f\n%!" fv
