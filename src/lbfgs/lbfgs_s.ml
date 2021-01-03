module Make =
  Lbfgs.Make
    (Owl.Algodiff.S)
    (struct
      let to_64 = Owl_dense_ndarray_generic.cast_s2d
      let from_64 = Owl_dense_ndarray_generic.cast_d2s
    end)
