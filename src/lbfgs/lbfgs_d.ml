module Make =
  Lbfgs.Make
    (Owl.Algodiff.D)
    (struct
      let to_64 x = x
      let from_64 x = x
    end)
