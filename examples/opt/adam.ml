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
module O = Owl_opt.D.Adam.Make (P)

let x = AD.Mat.gaussian 3 1
let a = AD.Mat.gaussian 5 3
let b = AD.Mat.gaussian 5 1
let y = AD.Maths.((a *@ x) + b)
let prms0 = { a = AD.Mat.gaussian 5 3; b = AD.Mat.gaussian 5 1 }
let loss prms = AD.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b)))
let f _ prms = loss prms
let lr = Owl_opt.Lr.Fix 1E-2

let () =
  let s = O.init ~prms0 ~lr () in
  let fv = O.min ~f s in
  Printf.printf "\nfinal loss: %f\n" fv


(* Alternativly, we can write our own optimization loop *)
let () =
  let init_fv = f 0 prms0 |> AD.unpack_flt in
  let s = O.init ~prms0 ~lr () in
  let rec opt s =
    match O.min_step ~f s with
    | Stop fv -> fv
    | Continue _ -> opt s
  in
  let fv = opt s in
  assert (fv = (f O.(iter s) O.(prms s) |> AD.unpack_flt));
  assert (init_fv = List.hd (O.fv_hist s));
  Printf.printf "\nfinal loss: %f\n" fv
