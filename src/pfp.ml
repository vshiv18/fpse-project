open Core

module type PFP_S = 
  sig
  type text = string
  type dict = string list
  val parse: text -> int -> dict * (int list)
  val buildText: string -> text
  val buildDict: dict -> string list
end

let repeat c k = String.of_char_list (List.fold (List.range 0 k) ~init:[] ~f:(fun acc _ -> c :: acc))
module PFP (Hash :sig val is_trigger: int -> string -> bool end) : PFP_S =
struct
  type text = string
  type dict = string list
  let parse (text : text) (w : int) : dict * (int list) =
    let rec helper (text : text) (dict : dict) (parse : int list) (current_phrase : string list) =
      if (String.length text) = 0 then (dict, parse) else
      let cur_trigger = String.prefix text w in
      match Hash.is_trigger 0 cur_trigger with 
      | false -> helper (String.drop_prefix text 1) dict parse ((String.prefix text 1) :: current_phrase)
      | true -> 
        let final_phrase = String.rev (String.concat [String.rev cur_trigger; String.concat current_phrase]) in
        let new_dict = if (List.mem dict final_phrase ~equal:String.(=)) then dict else  (final_phrase :: dict) in
        helper (String.drop_prefix text w) new_dict (((List.length new_dict) - 1) :: parse) [String.rev cur_trigger]
    in helper (String.concat ["$"; text; (repeat '$' w)]) [] [] []
  (* for testing *)
  let buildText (text : string) : text = text
  let buildDict (d : dict) : string list = d
end