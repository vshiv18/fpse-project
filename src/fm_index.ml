open Core

module CharWT = WaveletTree.Wavelet_tree.Make(Char)

module FM_index = struct
  type t = {bwt : CharWT.t; c_arr : (char, int) Hashtbl.t}
  
  let rank_exn fmi c pos = match CharWT.rank fmi c pos with | Some x -> x | None -> failwith "index out of bounds on rank op"

  let construct (text : string) : t =
    let bwt_wt = text |> String.to_list |> CharWT.build in
    let counts =
      text |> String.to_list
          |> Hashtbl.group
               (module Char)
               ~get_key:(fun x -> x)
               ~get_data:(fun _ -> 1)
               ~combine:(fun x y -> x + y)
    in
    let c_starts, _ = counts |> Hashtbl.keys
    |> List.sort ~compare:Char.compare
    |> List.fold ~init:([], 0) ~f:(fun (char_before, prior_before) ele ->
           let next = prior_before + Hashtbl.find_exn counts ele in
           ((ele, prior_before) :: char_before, next))
    in
    let c_map = c_starts |> List.rev |> Hashtbl.of_alist_exn (module Char) in
    {bwt=bwt_wt; c_arr=c_map}

  let of_file (fname : string) = 
    fname
    |> In_channel.read_all  
    |> construct

  let lf_range (fmi : t) (range : int * int) (c : char) : (int * int) = 
    if not (Hashtbl.mem fmi.c_arr c) then (1, 0)
    else
    let top, bottom = range in
    let c_before_top = rank_exn fmi.bwt c top in
    let c_before_bottom = rank_exn fmi.bwt c (bottom + 1) in
    let f_col = Hashtbl.find_exn fmi.c_arr c in
    (c_before_top + f_col, c_before_bottom + f_col - 1)

  let count (fmi : t) (query : string) : int option = 
    query
    |> String.to_list_rev
    |> List.fold_until ~init:((0, CharWT.cardinal fmi.bwt - 1)) ~f:(fun range c ->
      let newstart, newend = lf_range fmi range c in
      if newend <= newstart then Stop (None) else Continue (newstart, newend)
      )
      ~finish:(fun (s, e) -> Some (e - s + 1))

    let exists (fmi : t) (query : string) = 
      match count fmi query with 
      | Some _ -> true
      | None -> false

  let lf (fmi : t) (i : int) : int = 
    (rank_exn fmi.bwt (CharWT.access fmi.bwt i) i) + (Hashtbl.find_exn fmi.c_arr (CharWT.access fmi.bwt i))

  let serialize (fmi : t) (filename : string) =
    let chars, ints = fmi.c_arr |> Hashtbl.to_alist |> List.unzip in
    Serialize.StringSerializer.write (filename ^ ".f_char")  (String.of_list chars);
    Serialize.Int32Serializer.write_list (filename ^ ".occ") ints;
    let oc = Out_channel.create (filename ^ ".wt") in
    Marshal.to_channel oc fmi.bwt []

  let deserialize (filename : string) : t =
    let c_chars = Serialize.StringSerializer.read (filename ^ ".f_char") |> String.to_list in
    let c_ints = Serialize.Int32Serializer.read_list (filename ^ ".occ") in
    let c_map = (List.zip_exn c_chars c_ints) |> Hashtbl.of_alist_exn (module Char) in
    let ic = In_channel.create (filename ^ ".wt") in
    let wt : CharWT.t = Marshal.from_channel ic in
    {bwt=wt; c_arr=c_map}
  end