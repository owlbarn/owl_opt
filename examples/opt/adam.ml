open Owl

module P = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving prms]
end

open P
module O = Owl_opt.D.Adam.Make (P)

let x = Algodiff.D.Mat.gaussian 3 1
let a = Algodiff.D.Mat.gaussian 5 3
let b = Algodiff.D.Mat.gaussian 5 1
let y = Algodiff.D.Maths.((a *@ x) + b)
let prms0 = { a = Algodiff.D.Mat.gaussian 5 3; b = Algodiff.D.Mat.gaussian 5 1 }
let loss prms = Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b)))
let f _ prms = loss prms
let lr = Owl_opt.Lr.Fix 1E-2

let () =
  let s = O.init ~prms0 ~lr () in
  let fv = O.min ~f s in
  Printf.printf "\nfinal loss: %f\n" fv


(* Alternativly, we can write our own optimization loop *)
let () =
  let s = O.init ~prms0 ~lr () in
  let rec opt s =
    match O.min_step ~f s with
    | Stop fv -> fv
    | Continue _ -> opt s
  in
  let fv = opt s in
  Printf.printf "\nfinal loss: %f\n" fv
