module Make (P : Owl_opt.Prms.PT) :
  Lbfgs_intf.Sig
    with type 'a t := 'a P.t
     and type fv = Owl.Algodiff.D.t
     and type prm = Owl.Algodiff.D.t
