open Owl

module P = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving prms]
end

open P
module O = Owl_opt.Adam.Make (P)

let () =
  let x = Algodiff.D.Mat.gaussian 3 1 in
  let a = Algodiff.D.Mat.gaussian 5 3 in
  let b = Algodiff.D.Mat.gaussian 5 1 in
  let y = Algodiff.D.Maths.((a *@ x) + b) in
  let prms0 = { a = Algodiff.D.Mat.gaussian 5 3; b = Algodiff.D.Mat.gaussian 5 1 } in
  let f prms = Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) in
  let lr = O.(Fix 1E-4) in
  let s0 = O.init ~f ~prms0 ~lr () in
  let s = O.min s0 in
  Printf.printf "\nfinal loss: %f\n" (O.fv s)
