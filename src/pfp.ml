open Core
open Naive_bwt
open Serialize

module type PFP_S = sig
  type text = string
  type dict
  type parse = string list * int list * int list

  val parse : ?verbose:bool -> text -> int -> parse
  val buildText : string -> text
  val dict_to_alist : dict -> (string * int) list
  val parse_to_BWT : parse -> int -> string
  val getBWT : text -> int -> string
  val save_parse : parse -> string -> unit
  val load_parse : string -> parse
end

(* let repeat c k =
   String.of_char_list
     (List.fold (List.range 0 k) ~init:[] ~f:(fun acc _ -> c :: acc)) *)
let repeat c k = String.init k ~f:(fun _ -> c)

let fill s c len =
  String.concat
    [ s; repeat c (if len > String.length s then len - String.length s else 0) ]

(* let repeat_list c k = List.init k ~f:(fun _ -> c) *)

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
  let parse ?(verbose = false) (text : text) (w : int) : parse =
    let terminator = repeat '$' w in
    let dict_count = Hashtbl.create (module String) in
    (* let text = (String.concat [ "$"; text; repeat '$' w ]) in *)
    let rec helper (pos : int) (parse : int list)
        ((phrase_start, phrase_end) : int * int) : int list =
      let () =
        if verbose then
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

  let build_ilist (parse : int array) (parseSA : int list) :
      (int, int list) Hashtbl.t =
    let inv_list = Hashtbl.create (module Int) in
    List.length parseSA :: parseSA
    |> List.filter ~f:(fun x -> x <> 0)
    |> List.iteri ~f:(fun i p ->
           match p with
           | 0 -> ()
           | idx ->
               let p = IntSequence.get parse (idx - 1) in
               Hashtbl.add_multi inv_list ~key:p ~data:i);
    Hashtbl.map_inplace inv_list ~f:(List.sort ~compare:Int.compare);
    printf "Inverted list (BWT(P)) computed\n%!";
    inv_list

  let build_bwtlast (w : int) (phrases : string array) (parseSA : int list)
      (parse : int array) : string =
    let w_array =
      parseSA
      |> List.map ~f:(fun i ->
             let cur_phrase = Array.get phrases (wrap_get parse (i - 2)) in
             String.get cur_phrase (String.length cur_phrase - w - 1))
      |> String.of_char_list
    in
    let () = printf "W array computed\n%!" in
    w_array

  (* TODO: *)
  let parse_to_BWT (parse : parse) (w : int) : text =
    let phrases, _, parse = parse in
    (* let freqs = Array.of_list freqs in *)
    let phrases = Array.of_list phrases in
    let phrase_lengths = phrases |> Array.map ~f:String.length in
    let parse = parse |> IntSequence.of_list in
    let parseSA = parse |> IntBWT.getSA in
    let bwtlast = build_bwtlast w phrases parseSA parse in
    let inv_list = build_ilist parse parseSA in
    (* let phrase_starts = phrases
         |> Array.folding_map ~init:(-1) ~f:(fun acc phrase ->
           (acc + (String.length phrase) + 1, acc + 1)
         )
       in *)
    (* TODO: temp, throw "bad positions" in a set and filter that way *)
    (* alternatively, get bool array of good/bad positions to filter *)
    let dict_concat = phrases |> String.concat_array ~sep:"\x01" in
    let total_len = String.length dict_concat in
    let _, filter_pos =
      List.fold
        (List.range ~stride:(-1) (total_len - 1) (-1))
        ~init:(total_len, [])
        ~f:(fun (last_delim, lens) idx ->
          if Char.( = ) (String.get dict_concat idx) '\x01' then (idx, 0 :: lens)
          else (last_delim, (last_delim - idx) :: lens))
    in
    let filter_pos =
      List.folding_mapi filter_pos ~init:0 ~f:(fun i phrase_count length ->
          if Char.( = ) (String.get dict_concat i) '\x01' then
            (phrase_count + 1, (length, phrase_count))
          else (phrase_count, (length, phrase_count)))
      |> Array.of_list
    in
    let () = printf "computed filter positions\n%!" in

    (* replace below with SAIS method later *)

    (* this slots into the SA construction nicely, but SLOW *)
    (* let d_SA =
         dict_concat |> StringBWT.getSA
         |> List.filter_map ~f:(fun suffix_pos ->
                let len, phrase_id = Array.get filter_pos suffix_pos in
                if len <= w then None else Some (len, phrase_id))
       in *)

    (* this is the old method (using Map sort on suffix slices), much faster. but not scalable? *)
    (* let d_SA =
      Array.filter_map filter_pos ~f:(fun (len, phrase_id) ->
          if len <= w then None
          else
            Some
              ( String.slice
                  (Array.get phrases phrase_id)
                  (Array.get phrase_lengths phrase_id - len)
                  0,
                (len, phrase_id) ))
      |> List.of_array
      |> Map.of_alist_multi (module String)
      |> Map.data |> List.concat
    in *)
    let () = printf "number of phrases: %d\n%!" (Array.length phrases) in
    let () = Out_channel.write_all "/Users/vikram/Documents/jhu/third_year/fpse/fpse-project/dict_concat.txt" ~data:dict_concat in
    (* use the SAIS SA construction *)
    let d_SA =
      dict_concat |> Sais.SAIS.getSA
      |> List.of_array
      |> List.filter_map ~f:(fun suffix_pos ->
            if suffix_pos = (String.length dict_concat) then None else 
             let len, phrase_id = Array.get filter_pos suffix_pos in
             if len <= w then None else Some (len, phrase_id))
    in

    let () = printf "computed SA of dict\n%!" in
    (* fold, accumulator is (current output BWT, is buffer a dict phrase,
                            current representative phrase suffix, list of phrases that it occurs in) *)
    let process_phrase bwt phrase_id =
      let occs = Hashtbl.find_exn inv_list phrase_id in
      List.fold occs ~init:bwt ~f:(fun seq p -> String.get bwtlast p :: seq)
    in

    let process_alpha
        ((bwt, prev_alpha, prev_phrases) : char list * string * int list) :
        char list =
      if String.length prev_alpha = 0 then bwt
      else
        (* if this is a phrase, get prev chars from BWTlast, in order of ilist (already sorted) *)
        (* collect prev char of suffix from each phrase it appears in *)
        let prev_chars =
          let offset = String.length prev_alpha in
          List.map prev_phrases ~f:(fun phrase_id ->
              let phrase = Array.get phrases phrase_id in
              String.get phrase (String.length phrase - offset - 1))
        in
        (* merge ilists from each phrase (remembering prev char for each phrase) *)
        let merged_ilist =
          prev_phrases |> List.zip_exn prev_chars
          |> List.map ~f:(fun (c, x) ->
                 Hashtbl.find_exn inv_list x |> List.map ~f:(fun p -> (p, c)))
          |> List.fold ~init:[] ~f:(fun acc l ->
                 List.merge acc l ~compare:(fun (x1, _) (x2, _) ->
                     Int.compare x1 x2))
        in

        (* read off prev chars in order of ilist positions *)
        List.fold merged_ilist ~init:bwt ~f:(fun seq (_, c) -> c :: seq)
    in
    (* fold through SA of dict *)
    let bwt, prev_alpha, prev_phrases =
      List.fold d_SA ~init:([], "", [])
        ~f:(fun (bwt, prev_alpha, prev_phrases) (len, phrase_id) ->
          (* If suffix is a phrase from the dict, then process the buffer and set is_phrase = true to use BWTlast next iteration *)
          if len = Array.get phrase_lengths phrase_id then
            ( process_phrase
                (process_alpha (bwt, prev_alpha, prev_phrases))
                phrase_id,
              "",
              [] )
          else
            (* Check if we are in the same "alpha" range *)
            let cur_suffix =
              String.slice
                (Array.get phrases phrase_id)
                (Array.get phrase_lengths phrase_id - len)
                0
            in
            (* if so, add phrase id to buffer *)
            if
              String.length prev_alpha = 0 || String.( = ) cur_suffix prev_alpha
            then (bwt, cur_suffix, phrase_id :: prev_phrases)
            else
              (* otherwise process buffer and start new alpha range *)
              ( process_alpha (bwt, prev_alpha, prev_phrases),
                cur_suffix,
                [ phrase_id ] ))
    in
    process_alpha (bwt, prev_alpha, prev_phrases)
    (* we get the reverse BWT as a list of chars, post-processing *)
    |> String.of_char_list
    |> String.rev

  let getBWT input_string w =
    let p = parse input_string w in
    parse_to_BWT p w

  let save_parse (parse : parse) (out_dir : string) : unit =
    let dict, freq, parse = parse in
    StringSerializer.write_list (Filename.concat out_dir "dict") dict;
    Int32Serializer.write_list (Filename.concat out_dir "freq") freq;
    Int32Serializer.write_list (Filename.concat out_dir "parse") parse

  let load_parse (parse_dir : string) =
    let dict = StringSerializer.read_list (Filename.concat parse_dir "dict") in
    let freq = Int32Serializer.read_list (Filename.concat parse_dir "freq") in
    let parse = Int32Serializer.read_list (Filename.concat parse_dir "parse") in
    (dict, freq, parse)
end
