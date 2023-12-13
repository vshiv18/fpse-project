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
  val parse_to_BWT : parse -> int -> string * (int list)
  val getBWT : text -> int -> string
  val getBWT_SA : text -> int -> string * (int list)
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

let update_dict dict k = 
  let idx, _ = Hashtbl.update_and_return dict k ~f:(fun value ->
    match value with
    | Some (idx, freq) -> (idx, freq + 1)
    | None -> (Hashtbl.length dict, 1)
    )
  in idx

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
  let parse ?(verbose = false) (text : text) (w : int): parse =
    let terminator = repeat '\x01' w in
    let dict_count = Hashtbl.create (module String) in
    (* let text = (String.concat [ "$"; text; repeat '$' w ]) in *)
    let rec helper (pos : int) (parse : int list)
        ((phrase_start, phrase_end) : int * int) 
        (phrase_count : int) : int list =
      let () =
        if verbose then
          if pos % (String.length text / 100) = 0 then
            printf "%d/100 done\n%!" (pos / (String.length text / 100))
      in
      if pos > String.length text then parse
      else
        let cur_trigger =
          if pos + w > String.length text then
            fill (String.slice text pos 0) '\x01' w
          else String.slice text pos (pos + w)
        in
        match
          String.( = ) cur_trigger terminator
          || Hash.is_trigger_string cur_trigger
        with
        | false -> helper (pos + 1) parse (phrase_start, phrase_end + 1) phrase_count
        | true ->
            let final_phrase =
              let final_phrase =
                if phrase_end + w > String.length text then
                  String.slice text phrase_start 0 ^ terminator
                else String.slice text phrase_start (phrase_end + w)
              in
              if phrase_start = 0 then "\x01" ^ final_phrase else final_phrase
            in
            let idx = update_dict dict_count final_phrase in
            helper (pos + 1) (idx :: parse) (pos, pos + 1) phrase_count
    in
    let parse = List.rev (helper 0 [] (0, 0) 0) in
    let phrases =
      dict_count |> Hashtbl.keys |> List.sort ~compare:String.compare
    in
    let () = printf "number of phrases: %d\n%!" (List.length phrases) in
    let parse =
      let sorted_parsemap =
        phrases
        |> List.mapi ~f:(fun i phrase -> (let idx, _ = Hashtbl.find_exn dict_count phrase in idx, i))
        |> Hashtbl.of_alist_exn (module Int)
      in
      List.map
        ~f:(fun placeholder -> Hashtbl.find_exn sorted_parsemap placeholder)
        parse
    in
    let freqs =
      phrases |> List.map ~f:(fun p -> let _, freq = Hashtbl.find_exn dict_count p in freq)
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

  (* let build_ilist (parse : int array) (parseSA : int array):
      (int, int list) Hashtbl.t =
    let inv_list = Hashtbl.create (module Int) in
    parseSA
    |> Array.filter ~f:(fun x -> x <> 0)
    |> Array.iteri ~f:(fun i p ->
           match p with
           | 0 -> ()
           | idx ->
               let p = IntSequence.get parse (idx - 1) in
               Hashtbl.add_multi inv_list ~key:p ~data:i);
    Hashtbl.map_inplace inv_list ~f:(List.sort ~compare:Int.compare);
    printf "Inverted list (BWT(P)) computed\n%!";
    inv_list *)
  let build_ilist_sa_aux (parse : int array) (parseSA : int array) (phrase_lengths : int array) (w : int):
  (int, (int * int) list) Hashtbl.t =
    let offsets = parse 
    |> Array.map ~f:(fun i -> if i = 0 then (Array.get phrase_lengths i) else (Array.get phrase_lengths i) - w)
    |> Array.folding_map ~init:0 ~f:(fun acc len ->(acc + len, acc + len - 1)) in
    let inv_list = Hashtbl.create (module Int) in
    parseSA
    |> Array.filter ~f:(fun x -> x <> 0)
    |> Array.iteri ~f:(fun i p ->
          match p with
          | 0 -> ()
          | idx ->
              let p = IntSequence.get parse (idx - 1) in
              let s = Array.get offsets (idx - 1) in
              Hashtbl.add_multi inv_list ~key:p ~data:(i, s));
    Hashtbl.map_inplace inv_list ~f:(List.sort ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2));
    (* let sa_aux = Hashtbl.map inv_list ~f:(fun l -> List.map l ~f:(fun (_, offset) -> offset)) in
    let inv_list = Hashtbl.map inv_list ~f:(fun l -> List.map l ~f:(fun (bwtpos, _) -> bwtpos)) in *)
    printf "Inverted list (BWT(P)) computed\n%!";
    inv_list

  let build_bwtlast (w : int) (phrases : string array) (parseSA : int array)
      (parse : int array) : string =
    let w_array =
      parseSA
      |> Array.filter_mapi ~f:(fun idx i ->
            if idx = 0 then None else 
             let cur_phrase = Array.get phrases (wrap_get parse (i - 2)) in
            Some (String.get cur_phrase (String.length cur_phrase - w - 1)))
      |> String.of_array
    in
    let () = printf "W array computed\n%!" in
    w_array

  (* TODO: *)
  let parse_to_BWT (parse : parse) (w : int) : text * (int list) =
    let phrases, _, parse = parse in
    (* let freqs = Array.of_list freqs in *)
    let phrases = Array.of_list phrases in
    let phrase_lengths = phrases |> Array.map ~f:String.length in
    let parse = parse |> IntSequence.of_list in
    let parseSA = parse |> Gsacak.GSACAK.getSA_int in
    printf "Parse suffix array computed\n%!";
    let bwtlast = build_bwtlast w phrases parseSA parse in
    let inv_list = build_ilist_sa_aux parse parseSA phrase_lengths w in
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
    (* let () = printf "length of concatenated dict = %d\n%!" (String.length dict_concat) in
    let () = Out_channel.write_all "dict_concat.txt" ~data:dict_concat in *)
    (* use the SAIS SA construction *)
    let d_SA =
      dict_concat |> Gsacak.GSACAK.getSA
      |> List.of_array
      |> List.filter_map ~f:(fun suffix_pos ->
            if suffix_pos = (String.length dict_concat) then None else 
             let len, phrase_id = Array.get filter_pos suffix_pos in
             if len <= w then None else Some (len, phrase_id))
    in

    let () = printf "computed SA of dict\n%!" in
    (* fold, accumulator is (current output BWT, is buffer a dict phrase,
                            current representative phrase suffix, list of phrases that it occurs in) *)
    let process_phrase bwt sa phrase_id =
       (* if this is a phrase, get prev chars from BWTlast, in order of ilist (already sorted) *)
      let occs_sa = Hashtbl.find_exn inv_list phrase_id in
      let occs, offsets = List.unzip occs_sa in
      
      List.fold occs ~init:bwt ~f:(fun seq p -> String.get bwtlast p :: seq), 
      List.fold offsets ~init:sa ~f:(fun seq p -> (p - (Array.get phrase_lengths phrase_id)) :: seq)
      (* [] TODO *)
    in

    let process_alpha
        (bwt : char list) (sa : int list) (prev_alpha : string) (prev_phrases : int list) :
        char list * int list =
      let alpha_len = String.length prev_alpha in
      if alpha_len = 0 then bwt, sa (* TODO *)
      else
        (* collect prev char of suffix from each phrase it appears in *)
        let prev_chars =
          List.map prev_phrases ~f:(fun phrase_id ->
              let phrase = Array.get phrases phrase_id in
              String.get phrase (String.length phrase - alpha_len - 1))
        in
        (* merge ilists from each phrase (remembering prev char for each phrase) *)
        let offsets, merged_ilist_chars =
          prev_phrases |> List.zip_exn prev_chars
          |> List.map ~f:(fun (c, x) ->
                 Hashtbl.find_exn inv_list x |> List.map ~f:(fun p -> (p, c)))
          |> List.fold ~init:[] ~f:(fun acc l ->
                 List.merge acc l ~compare:(fun ((x1, _), _) ((x2, _), _) ->
                     Int.compare x1 x2))
          |> List.map ~f:(fun ((_, offset), c) -> (offset, c))
          |> List.unzip
        in
        (* read off prev chars in order of ilist positions *)
        List.fold merged_ilist_chars ~init:bwt ~f:(fun seq c -> c :: seq), 
        List.fold offsets ~init:sa ~f:(fun seq p -> (p - alpha_len) :: seq)
        (* [] TODO *)
    in
    (* fold through SA of dict *)
    let bwt, sa, prev_alpha, prev_phrases =
      List.fold d_SA ~init:([], [], "", [])
        ~f:(fun (bwt, sa, prev_alpha, prev_phrases) (len, phrase_id) ->
          (* If suffix is a phrase from the dict, then process the buffer and immediately process phrase before iterating *)
          if len = Array.get phrase_lengths phrase_id then
            let bwt, sa = process_alpha bwt sa prev_alpha prev_phrases in
            let bwt, sa = process_phrase bwt sa phrase_id in
            ( bwt, sa, "", [] )
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
            then (bwt, sa, cur_suffix, phrase_id :: prev_phrases)
            else
              (* otherwise process buffer and start new alpha range *)
              let bwt, sa = process_alpha bwt sa prev_alpha prev_phrases in
              ( bwt, sa, cur_suffix, [ phrase_id ] ))
    in
    let bwt, sa = process_alpha bwt sa prev_alpha prev_phrases in
    (bwt
    (* we get the reverse BWT as a list of chars, post-processing *)
    |> String.of_char_list
    |> String.rev),
    (List.length bwt) - 1 :: (sa |> List.tl_exn|> List.rev |> List.tl_exn)

  let getBWT input_string w =
    let p = parse input_string w in
    let bwt, _ = parse_to_BWT p w in
    bwt
  
  let getBWT_SA input_string w =
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
