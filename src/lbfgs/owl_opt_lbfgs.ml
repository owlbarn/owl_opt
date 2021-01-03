open Base
open Owl
open Bigarray
module AD = Algodiff.D

module Make (P : Owl_opt.Prms.PT) = struct
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t
  type f = int -> prms -> fv

  type adt =
    | S
    | M

  type info = (int * int * int array * adt) P.t

  let build_info : prms -> int * info =
   fun prms ->
    let n_prms = ref 0 in
    let info =
      P.map
        ~f:(fun x ->
          let open AD in
          match x with
          | F _a ->
            let idx = !n_prms in
            n_prms := !n_prms + 1;
            idx, 1, [| 1 |], S
          | Arr _a ->
            let idx = !n_prms in
            let n = numel x in
            n_prms := !n_prms + n;
            idx, n, shape x, M
          | _ -> assert false)
        prms
    in
    !n_prms, info


  let extract info x =
    P.map
      ~f:(fun (idx, l, s, adt) ->
        match adt with
        | S -> AD.pack_flt x.{idx}
        | M ->
          Bigarray.Array1.sub x idx l
          |> Bigarray.genarray_of_array1
          |> (fun x -> Bigarray.reshape x s)
          |> AD.pack_arr)
      info


  (* blit a: Algodiff.t into b: Array1.t *)
  let blit f info src dst =
    let open Bigarray in
    let dst =
      P.map
        ~f:(fun (idx, l, s, _) ->
          Array1.sub dst idx l |> genarray_of_array1 |> fun x -> reshape x s)
        info
    in
    let open AD in
    P.iter2 src dst ~f:(fun a b ->
        match f a with
        | F x -> Genarray.set b [| 0 |] x
        | Arr x -> Genarray.blit x b
        | _ -> assert false)


  type state =
    { mutable fv_hist : float list
    ; mutable k : int
    ; work : Lbfgs.work
    ; ps : (float, float64_elt, c_layout) Array1.t
    ; n_prms : int
    ; info : info
    }

  type status =
    | Stop of float
    | Continue of float

  type stop = float -> state -> bool

  let iter s = s.k

  let prev_fv s =
    match List.hd s.fv_hist with
    | Some fv -> fv
    | None -> failwith "there has not been any function evaluations"


  let fv_hist s = List.rev s.fv_hist
  let prms s = extract s.info s.ps

  let init ?(corrections = 10) ~prms0 () =
    let n_prms, info = build_info prms0 in
    let ps = Array1.create float64 c_layout n_prms in
    blit AD.primal info prms0 ps;
    let work = Lbfgs.start ~corrections n_prms in
    { ps; n_prms; fv_hist = []; info; k = 0; work }


  let f_df ~f s x g =
    let x = extract s.info x in
    let t = AD.tag () in
    let x = P.map x ~f:(fun x -> AD.make_reverse x t) in
    let c = f s.k x in
    AD.reverse_prop (F 1.) c;
    blit AD.adjval s.info x g;
    AD.unpack_flt c


  let stop fv s =
    if s.k % 10 = 0 then Stdio.printf "\rstep: %i | loss: %4.9f%!" s.k fv;
    Float.(fv < 1E-3)


  let step update ?(pgtol = 1E-5) ?(factr = 1E7) ?(stop = stop) ~f s gs =
    let fv = f s.k (prms s) |> AD.unpack_flt in
    if stop fv s
    then Stop fv
    else (
      s.fv_hist <- fv :: s.fv_hist;
      match update ~pgtol ~factr ~work:s.work (f_df ~f s) s.ps gs with
      | Lbfgs.Stop fv -> Stop fv
      | Lbfgs.Continue fv ->
        s.k <- Int.succ s.k;
        Continue fv)


  let lmin ~pgtol ~factr ~work f_df x g = Lbfgs.min ~pgtol ~factr ~work f_df x g
  let lmax ~pgtol ~factr ~work f_df x g = Lbfgs.max ~pgtol ~factr ~work f_df x g

  let optimise update ?(pgtol = 1E-5) ?(factr = 1E7) ?(stop = stop) ~f s =
    Lbfgs.restart s.work;
    let gs = Array1.create float64 c_layout s.n_prms in
    let rec run () =
      match step ~pgtol ~factr ~stop update ~f s gs with
      | Stop fv -> fv
      | Continue _ -> run ()
    in
    run ()


  let min = optimise lmin
  let max = optimise lmax
end
