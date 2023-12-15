open Core

module CharWT = Wav

module FM_index = struct
  type t = {bwt : CharWT; c_arr : (char, int) Hashtbl.t}
  
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

  let exists _ _ = true

  let count _ _ = 0

  let lf (fmi : t) (i : int) : int = 
    (CharWT.rank fmi.bwt i) + (Hashtbl.find_exn fmi.c_arr i)

  let lf_range (fmi : t) (range : int * int) (c : char) : (int, int) = 
    if not Hashtbl.mem c then (1, 0)
    let c_before = CharWT.rank
end