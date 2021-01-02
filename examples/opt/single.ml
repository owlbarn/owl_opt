open Owl
module Prms = Owl_opt.Prms.Single
module O = Owl_opt.D.Adam.Make (Prms)

let () =
  let x = Algodiff.D.Mat.gaussian 3 1 in
  let a = Algodiff.D.Mat.gaussian 5 3 in
  let y = Algodiff.D.Maths.(a *@ x) in
  let prms0 = Prms.pack (Algodiff.D.Mat.gaussian 5 3) in
  let f _ prms = Algodiff.D.Maths.(l2norm' (y - (Prms.unpack prms *@ x))) in
  let lr = Owl_opt.Lr.Fix 1E-2 in
  let s = O.init ~prms0 ~lr () in
  let fv = O.min ~f s in
  Printf.printf "\nfinal loss: %f\n" fv
