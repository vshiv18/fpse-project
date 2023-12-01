open Core
open Naive_bwt

module type PFP_S = sig
  type text = string
  type dict
  type parse

  val parse : text -> int -> parse
  val buildText : string -> text
  val dict_to_alist : dict -> (string * int) list
  val parse_to_BWT : parse -> int -> string
  val getBWT : text -> int -> string
end

(* let repeat c k =
   String.of_char_list
     (List.fold (List.range 0 k) ~init:[] ~f:(fun acc _ -> c :: acc)) *)
let repeat c k = String.init k ~f:(fun _ -> c)

let fill s c len =
  String.concat
    [ s; repeat c (if len > String.length s then len - String.length s else 0) ]

let repeat_list c k =
  List.fold (List.range 0 k) ~init:[] ~f:(fun acc _ -> c :: acc)

module PFP (Hash : sig
  val is_trigger_string : string -> bool
end) : PFP_S = struct
  type text = string

  module Dict = Hashtbl.Make (String)

  type dict = int Dict.t
  type parse = string list * int list * int list

  module ParseMapper = Map.Make (String)
  module IntBWT = Naive_bwt.Text (Naive_bwt.IntSequence)
  module StringBWT = Naive_bwt.Text (Naive_bwt.CharSequence)

  (* rewrite to use indexes instead of passing in new text copies, esp no Stirng.drop_prefix *)
  (* now the helper takes in
     start pointer, current dict, parse, map of phrase to temp index, and current phrase, which is a tuple of (start, end) positions *)
  let parse (text : text) (w : int) : parse =
    let terminator = repeat '$' w in
    let dict_count = Hashtbl.create (module String) in
    (* let text = (String.concat [ "$"; text; repeat '$' w ]) in *)
    let rec helper (pos : int) (parse : int list)
        ((phrase_start, phrase_end) : int * int) : int list =
      let () =
        if pos % (String.length text / 100) = 0 then
          printf "%d/100 done\n%!" (pos / (String.length text / 100))
      in
      if pos > String.length text then parse
      else
        let cur_trigger =
          if pos + w > String.length text then
            fill (String.slice text pos 0) '$' w
          else String.slice text pos (pos + w)
        in
        match
          String.( = ) cur_trigger terminator
          || Hash.is_trigger_string cur_trigger
        with
        | false -> helper (pos + 1) parse (phrase_start, phrase_end + 1)
        | true ->
            let final_phrase =
              let final_phrase =
                if phrase_end + w > String.length text then
                  String.slice text phrase_start 0 ^ terminator
                else String.slice text phrase_start (phrase_end + w)
              in
              if phrase_start = 0 then "$" ^ final_phrase else final_phrase
            in
            let () = Hashtbl.incr dict_count final_phrase in
            helper (pos + 1) (Hashtbl.hash final_phrase :: parse) (pos, pos + 1)
    in
    let parse = List.rev (helper 0 [] (0, 0)) in
    let phrases =
      dict_count |> Hashtbl.keys |> List.sort ~compare:String.compare
    in
    let parse =
      let sorted_parsemap =
        phrases
        |> List.mapi ~f:(fun idx phrase -> (Hashtbl.hash phrase, idx))
        |> Hashtbl.of_alist_exn (module Int)
      in
      List.map
        ~f:(fun placeholder -> Hashtbl.find_exn sorted_parsemap placeholder)
        parse
    in
    let freqs =
      phrases |> List.map ~f:(fun p -> Hashtbl.find_exn dict_count p)
    in

    (phrases, freqs, parse)
  (* for testing *)

  let buildText (text : string) : text = text

  let dict_to_alist (dict : dict) : (string * int) list =
    dict |> Hashtbl.to_alist
    |> List.sort ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2)

  (* let wrap_nth_exn list i = List.nth_exn list (i % List.length list) *)
  let wrap_get list i = IntSequence.get list (i % IntSequence.length list)

  (* let wrap_get string i = String.get string (i % String.length string) *)

  let is_homogenous l =
    match
      List.all_equal ~equal:Char.( = ) (List.map ~f:(fun (x, _, _) -> x) l)
    with
    | Some _ -> true
    | None -> false

  (* TODO: *)
  let parse_to_BWT (parse : parse) (w : int) : text =
    let phrases, freqs, parse = parse in
    let parse = parse |> IntSequence.of_list in
    let parseSA = parse |> IntBWT.getSA in
    let inv_list = Hashtbl.create (module Int) in
    let () =
      List.length parseSA :: parseSA
      |> List.iteri ~f:(fun i p ->
             match p with
             | 0 -> ()
             | idx ->
                 let p = IntSequence.get parse (idx - 1) in
                 Hashtbl.add_multi inv_list ~key:p ~data:i)
    in
    let () = printf "Inverted list (BWT(P)) computed\n%!" in
    let w_array =
      parseSA
      |> List.map ~f:(fun i ->
             let cur_phrase = List.nth_exn phrases (wrap_get parse (i - 2)) in
             String.get cur_phrase (String.length cur_phrase - w - 1))
      |> String.of_char_list
    in
    let () = printf "W array computed\n%!" in
    (* add suffixes s which are proper suffixes of a phrase d in D *)
    let beta =
      List.zip_exn phrases freqs |>
      List.foldi ~init:ParseMapper.empty
        ~f:(fun i acc (phrase, freq) ->
          let len = String.length phrase in
          List.fold
            (List.range w (len - 1))
            ~init:acc
            ~f:(fun countmap p ->
              Map.add_multi countmap
                ~key:(String.suffix phrase (p + 1))
                ~data:(String.get phrase (len - p - 2), i, freq)))
    in
    (* add suffixes s which are in D *)
    (* let beta =
       List.foldi phrases ~init:beta ~f:(fun i acc phrase ->
           let occs = Map.find_exn inv_list phrase in
           List.fold occs ~init:acc ~f:(fun countmap p ->
               Map.add_multi countmap ~key:phrase ~data:(String.get w_array p, i, 1))) *)
    let beta =
      List.fold phrases ~init:beta ~f:(fun acc phrase ->
          Map.add_exn acc ~key:phrase ~data:[ (' ', -1, 0) ])
    in
    let () = printf "Beta mapping done\n%!" in
    (* beta contains a map of s (phrase suffix) -> list of (prev character, original phrase rank, frequency)
       If s is a phrase, then it stores a dummy (' ', -1, 0), which is used as an indicator that chars should be pulled from W *)
    List.fold (Map.to_alist beta) ~init:[] ~f:(fun bwt (phrase, prevs) ->
        if
          List.length prevs = 1
          &&
          let _, i, _ = List.hd_exn prevs in
          i = -1
        then
          (*case where s is a member of D*)
          let occs =
            List.sort (Hashtbl.find_exn inv_list phrase) ~compare:Int.compare
          in
          List.fold occs ~init:bwt ~f:(fun seq p -> String.get w_array p :: seq)
        else if is_homogenous prevs then
          (* case of no ambiguous preceding chars*)
          List.fold prevs ~init:bwt ~f:(fun curbwt (c, _, freq) ->
              List.concat [ repeat_list c freq; curbwt ])
        else
          (* Case where s appears as proper suffix of multiple phrases, which different preceding chars*)
          let order =
            List.map prevs ~f:(fun (c, d, _) ->
                List.map
                  (Hashtbl.find_exn inv_list (List.nth_exn phrases d))
                  ~f:(fun p -> (c, p)))
            |> List.concat
            |> List.sort ~compare:(fun (_, p1) (_, p2) -> p1 - p2)
          in
          List.fold order ~init:bwt ~f:(fun seq (c, _) -> c :: seq))
    |> String.of_char_list |> String.rev

  let getBWT input_string w =
    let p = parse input_string w in
    parse_to_BWT p w
end
