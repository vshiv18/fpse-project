open Core

module type PFP_S = 
  sig
  type text = string
  type dict
  val parse: text -> int -> dict * (int list)
  val buildText: string -> text
  val dict_to_alist: dict -> (string * int) list
end

let repeat c k = String.of_char_list (List.fold (List.range 0 k) ~init:[] ~f:(fun acc _ -> c :: acc))
module PFP (Hash :sig val is_trigger: string -> bool end) : PFP_S =
struct
  type text = string
  module Dict = Map.Make(String)
  type dict = int Dict.t
  type dict_counter = bool list Dict.t

  module ParseMapper = Map.Make(String)
  type parse_mapper = int ParseMapper.t
  module ParseSorter = Map.Make(Int)
  let parse (text : text) (w : int) : dict * (int list) =
    let rec helper (text : text) (dict : dict_counter) (parse : int list) (parsemap : parse_mapper) (current_phrase : string list) : (dict_counter * (int list) * parse_mapper) =
      if (String.length text) = 0 then (dict, parse, parsemap) else
      let cur_trigger = String.prefix text w in
      match Hash.is_trigger cur_trigger with 
      | false -> helper (String.drop_prefix text 1) dict parse parsemap ((String.prefix text 1) :: current_phrase)
      | true -> 
        let final_phrase = String.rev (String.concat [String.rev cur_trigger; String.concat current_phrase]) in
        let new_parsemapper = if (Map.mem dict final_phrase) then parsemap else (Map.add_exn parsemap ~key:final_phrase ~data:(Map.length dict)) in
        let new_dict = (Map.add_multi dict ~key:final_phrase ~data:true) in
        helper (String.drop_prefix text w) new_dict ((Map.find_exn new_parsemapper final_phrase) :: parse) new_parsemapper [String.rev cur_trigger]
    in let dict, parse, parsemap = helper (String.concat ["$"; text; (repeat '$' w)]) Dict.empty [] ParseMapper.empty [] in
    let dict = Map.map dict ~f:(fun count -> List.length count) in
    let parse = List.rev parse in
    let reorder_parse (dict : dict) (parse : int list) (parsemap : parse_mapper) = 
      let sorted_parsemap = 
        (Map.to_alist ~key_order:`Increasing parsemap)
        |> List.mapi ~f:(fun idx (_, placeholder) -> (placeholder, idx))
        |> ParseSorter.of_alist_exn
      in let parse = List.map ~f:(fun placeholder -> Map.find_exn sorted_parsemap placeholder) parse 
      in (dict, parse) 
    in reorder_parse dict parse parsemap
    (* for testing *)
  let buildText (text : string) : text = text
  let dict_to_alist (dict : dict) : (string * int) list = Map.to_alist ~key_order:`Increasing dict
end