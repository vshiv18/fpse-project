open Core
open Naive_bwt

module type PFP_S = sig
  type text = string
  type dict

  val parse : text -> int -> dict * int list
  val buildText : string -> text
  val dict_to_alist : dict -> (string * int) list
  val parse_to_BWT : dict * int list -> int -> string
  val getBWT : text -> int -> string
end

let repeat c k =
  String.of_char_list
    (List.fold (List.range 0 k) ~init:[] ~f:(fun acc _ -> c :: acc))

module PFP (Hash : sig
  val is_trigger : string -> bool
end) : PFP_S = struct
  type text = string

  module Dict = Map.Make (String)

  type dict = int Dict.t
  type dict_counter = bool list Dict.t

  module ParseMapper = Map.Make (String)

  type parse_mapper = int ParseMapper.t

  module ParseSorter = Map.Make (Int)
  module InvList = Map.Make (String)

  module IntBWT = Naive_bwt.Text (Naive_bwt.IntSequence)
  module StringBWT = Naive_bwt.Text (Naive_bwt.CharSequence)

  let parse (text : text) (w : int) : dict * int list =
    let rec helper (text : text) (dict : dict_counter) (parse : int list)
        (parsemap : parse_mapper) (current_phrase : string list) :
        dict_counter * int list * parse_mapper =
      if String.length text = 0 then (dict, parse, parsemap)
      else
        let cur_trigger = String.prefix text w in
        match Hash.is_trigger cur_trigger with
        | false ->
            helper
              (String.drop_prefix text 1)
              dict parse parsemap
              (String.prefix text 1 :: current_phrase)
        | true ->
            let final_phrase =
              String.rev
                (String.concat
                   [ String.rev cur_trigger; String.concat current_phrase ])
            in
            let new_parsemapper =
              if Map.mem dict final_phrase then parsemap
              else
                Map.add_exn parsemap ~key:final_phrase ~data:(Map.length dict)
            in
            let new_dict = Map.add_multi dict ~key:final_phrase ~data:true in
            helper
              (String.drop_prefix text w)
              new_dict
              (Map.find_exn new_parsemapper final_phrase :: parse)
              new_parsemapper
              [ String.rev cur_trigger ]
    in
    let dict, parse, parsemap =
      helper
        (String.concat [ "$"; text; repeat '$' w ])
        Dict.empty [] ParseMapper.empty []
    in
    let dict = Map.map dict ~f:(fun count -> List.length count) in
    let parse = List.rev parse in
    let reorder_parse (dict : dict) (parse : int list) (parsemap : parse_mapper)
        =
      let sorted_parsemap =
        Map.to_alist ~key_order:`Increasing parsemap
        |> List.mapi ~f:(fun idx (_, placeholder) -> (placeholder, idx))
        |> ParseSorter.of_alist_exn
      in
      let parse =
        List.map
          ~f:(fun placeholder -> Map.find_exn sorted_parsemap placeholder)
          parse
      in
      (dict, parse)
    in
    reorder_parse dict parse parsemap
  (* for testing *)

  let buildText (text : string) : text = text

  let dict_to_alist (dict : dict) : (string * int) list =
    Map.to_alist ~key_order:`Increasing dict

  let wrap_nth_exn list i = List.nth_exn list (i % List.length list)
  (* let wrap_get string i = String.get string (i % String.length string) *)

  let is_homogenous l =
    match
      List.all_equal ~equal:Char.( = ) (List.map ~f:(fun (x, _, _) -> x) l)
    with
    | Some _ -> true
    | None -> false

  (* TODO: *)
  let parse_to_BWT (parse : dict * int list) (w : int) : text=
    let dic, parse = parse in
    let phrases = Map.keys dic in
    let rank_to_phrase =
      List.mapi phrases ~f:(fun i p -> (i, p)) |> Map.of_alist_exn (module Int)
    in
    let parseBWT =
      parse |> IntSequence.of_list |> IntBWT.getBWT |> List.of_array
    in
    let inv_list =
      parseBWT
      |> List.filter ~f:(fun x -> x <> -1)
      |> List.foldi ~init:InvList.empty ~f:(fun i acc p ->
             Map.add_multi acc ~key:(Map.find_exn rank_to_phrase p) ~data:i)
    in
    let w_array =
      parse |> IntSequence.of_list |> IntBWT.getSA
      |> List.map ~f:(fun i ->
             let cur_phrase =
               List.nth_exn phrases (wrap_nth_exn parse (i - 2))
             in
             String.get cur_phrase (String.length cur_phrase - w - 1))
      |> String.of_char_list
    in
    (* add suffixes s which are proper suffixes of a phrase d in D *)
    let beta =
      List.foldi phrases ~init:ParseMapper.empty ~f:(fun i acc phrase ->
          let len = String.length phrase in
          List.fold
            (List.range w (len - 1))
            ~init:acc
            ~f:(fun countmap p ->
              Map.add_multi countmap
                ~key:(String.suffix phrase (p + 1))
                ~data:(String.get phrase (len - p - 2), i, Map.find_exn dic phrase)))
    in
    (* add suffixes s which are in D *)
    let beta =
      List.foldi phrases ~init:beta ~f:(fun i acc phrase ->
          let occs = Map.find_exn inv_list phrase in
          List.fold occs ~init:acc ~f:(fun countmap p ->
              Map.add_multi countmap ~key:phrase ~data:(String.get w_array p, i, 1)))
    in
    List.fold (Map.to_alist beta) ~init:[] ~f:(fun acc (_, prevs) ->
        if is_homogenous prevs then
          List.fold prevs ~init:acc ~f:(fun curbwt (c, _, freq) ->
              List.fold (List.range 0 freq) ~init:curbwt ~f:(fun seq _ ->
                  c :: seq))
        else
          let order = List.map prevs ~f:(fun (c, d, freq) ->
            List.fold (Map.find_exn inv_list (List.nth_exn phrases d)) ~init:[] ~f:(fun l p -> (c, p, freq) :: l)
            ) |> List.concat |> List.sort ~compare:(fun (_, p1, _) (_, p2, _) -> p1 - p2) in
          List.fold order ~init:acc ~f:(fun curbwt (c, _, freq) ->
            List.fold (List.range 0 freq) ~init:curbwt ~f:(fun seq _ ->
                c :: seq))
        )
    |> String.of_char_list |> String.rev
  
  let getBWT input_string w = let p = parse input_string w in parse_to_BWT p w
end
