module Make (P : Owl_opt.Prms.PT) :
  Lbfgs_intf.Sig
    with type 'a t := 'a P.t
     and type fv = Owl.Algodiff.S.t
     and type prm = Owl.Algodiff.S.t
