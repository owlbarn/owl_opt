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
module O = Owl_opt.D.Rmsprop.Make (P)

let () =
  let x = Algodiff.D.Mat.gaussian 3 1 in
  let a = Algodiff.D.Mat.gaussian 5 3 in
  let b = Algodiff.D.Mat.gaussian 5 1 in
  let y = Algodiff.D.Maths.((a *@ x) + b) in
  let prms0 = { a = Algodiff.D.Mat.gaussian 5 3; b = Algodiff.D.Mat.gaussian 5 1 } in
  let f _ prms = Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) in
  let init_fv = f 0 prms0 |> AD.unpack_flt in
  let lr = Owl_opt.Lr.Fix 1E-4 in
  let s = O.init ~lr ~beta:0.9 ~prms0 () in
  let fv = O.min ~f s in
  assert (fv = (f O.(iter s) O.(prms s) |> AD.unpack_flt));
  assert (init_fv = List.hd (O.fv_hist s));
  Printf.printf "\nfinal loss: %f\n%!" fv
