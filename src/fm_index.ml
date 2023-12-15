open Core
module CharWT = WaveletTree.Wavelet_tree.Make (Char)
module CMap = Map.Make (Char)

module FM_index = struct
  type t = { bwt : CharWT.t; c_arr : int CMap.t }

  let rank_exn fmi c pos =
    match CharWT.rank fmi c pos with
    | Some x -> x
    | None -> failwith "index out of bounds on rank op"

  let construct (text : string) : t =
    let bwt_wt = text |> String.to_list |> CharWT.build in

    let counts =
      text |> String.to_list
      |> List.fold ~init:CMap.empty ~f:(fun acc c ->
             Map.add_multi acc ~key:c ~data:true)
      |> Map.map ~f:List.length
    in
    let c_map, _ =
      counts |> Map.keys
      |> List.fold ~init:(CMap.empty, 0) ~f:(fun (acc, prior_before) ele ->
             let next = prior_before + Map.find_exn counts ele in
             (Map.add_exn acc ~key:ele ~data:prior_before, next))
    in
    printf "%s\n%!" (c_map |> CMap.sexp_of_t Int.sexp_of_t |> Sexp.to_string);
    { bwt = bwt_wt; c_arr = c_map }

  let of_file (fname : string) = fname |> In_channel.read_all |> construct

  let lf_range (fmi : t) (range : int * int) (c : char) : int * int =
    if not (Map.mem fmi.c_arr c) then (1, 0)
    else
      let top, bottom = range in
      let c_before_top = rank_exn fmi.bwt c top in
      let c_before_bottom = rank_exn fmi.bwt c (bottom + 1) in
      let f_col = Map.find_exn fmi.c_arr c in
      (c_before_top + f_col, c_before_bottom + f_col - 1)

  let count (fmi : t) (query : string) : int option =
    query |> String.to_list_rev
    |> List.fold_until
         ~init:(0, CharWT.cardinal fmi.bwt - 1)
         ~f:(fun range c ->
           let newstart, newend = lf_range fmi range c in
           if newend <= newstart then Stop None else Continue (newstart, newend))
         ~finish:(fun (s, e) -> Some (e - s + 1))

  let exists (fmi : t) (query : string) =
    match count fmi query with Some _ -> true | None -> false

  let lf (fmi : t) (i : int) : int =
    rank_exn fmi.bwt (CharWT.access fmi.bwt i) i
    + Map.find_exn fmi.c_arr (CharWT.access fmi.bwt i)

  let serialize (fmi : t) (filename : string) =
    let chars, ints = fmi.c_arr |> Map.to_alist |> List.unzip in
    let oc_carr = Out_channel.create (filename ^ ".occ") in
    Marshal.to_channel oc_carr (chars, ints) [];
    let oc = Out_channel.create (filename ^ ".wt") in
    Marshal.to_channel oc fmi.bwt []

  let deserialize (filename : string) : t =
    let (chars, ints) : char list * int list =
      Marshal.from_channel (In_channel.create (filename ^ ".occ"))
    in
    let c_map = CMap.of_alist_exn (List.zip_exn chars ints) in
    let wt : CharWT.t =
      Marshal.from_channel (In_channel.create (filename ^ ".wt"))
    in
    { bwt = wt; c_arr = c_map }
end
