open Core
open Naive_bwt
open Serialize
open Fasta

module type S = sig
  type text = string
  type dict
  type parse = string list * int list * int list

  val dict_to_alist : dict -> (string * int) list
  val initialize_streamer : ?chunk_size:int -> string -> Fasta.FASTAStreamer.t
  val trigger : string -> int -> FASTAStreamer.t -> bool -> text * text * bool
  val hash : string -> int -> int list * dict
  val parse : string -> int -> parse
  val buildText : string -> text
  val parse_to_BWT : parse -> int -> string
  val getBWT : text -> int -> string
  val save_parse : parse -> string -> unit
  val load_parse : string -> parse
end

module PFP (Hash : sig
  val is_trigger_string : string -> bool
end) : S = struct
  type text = string

  module Dict = Hashtbl.Make (String)

  type dict = int Dict.t
  type parse = string list * int list * int list

  module ParseMapper = Map.Make (String)
  module IntBWT = Naive_bwt.Text (Naive_bwt.IntSequence)
  module StringBWT = Naive_bwt.Text (Naive_bwt.CharSequence)

  let dict_to_alist (dict : dict) : (string * int) list =
    Hashtbl.to_alist dict
    |> List.sort ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2)

  let initialize_streamer ?(chunk_size = 100) (filename : string) :
      FASTAStreamer.t =
    FASTAStreamer.create ~chunk_size filename

  let finished (phrase : string) (chunk : string) (is_last_chunk : bool) : bool
      =
    String.is_empty phrase && String.is_empty chunk && is_last_chunk

  let sorted_phrases (dict_count : dict) : text list =
    dict_count |> Hashtbl.keys |> List.sort ~compare:String.compare

  let sorted_hashmap (phrases : text list) : (int, int) Dict.hashtbl =
    phrases
    |> List.mapi ~f:(fun idx phrase -> (Hashtbl.hash phrase, idx))
    |> Hashtbl.of_alist_exn (module Int)

  let sorted_parse (phrases : text list) (hash : int list) : int list =
    let sorted_hashmap = sorted_hashmap phrases in
    List.map ~f:(fun h -> Hashtbl.find_exn sorted_hashmap h) hash

  let sorted_freqs (phrases : text list) (dict_count : dict) : int list =
    phrases |> List.map ~f:(fun p -> Hashtbl.find_exn dict_count p)

  let trigger (chunk : string) (window : int) (streamer : FASTAStreamer.t)
      (is_last_chunk : bool) : text * text * bool =
    if String.length chunk < window then
      if is_last_chunk then
        (String.pad_right ~char:'$' chunk ~len:window, chunk, is_last_chunk)
      else
        match FASTAStreamer.next streamer with
        | Continue next_chunk ->
            let chunk = chunk ^ next_chunk in
            (String.prefix chunk window, chunk, false)
        | Stop next_chunk ->
            let chunk = chunk ^ next_chunk in
            (String.prefix chunk window, chunk, true)
    else (String.prefix chunk window, chunk, is_last_chunk)

  let hash (filename : string) (window : int) : int list * dict =
    let streamer = initialize_streamer filename in
    let terminator = String.pad_right ~char:'$' "" ~len:window in
    let dict_count = Hashtbl.create (module String) in
    let rec f (phrase : string) (chunk : string) (is_last_chunk : bool)
        (parse : int list) : int list =
      if finished phrase chunk is_last_chunk then parse
      else
        let trigger, chunk, is_last_chunk =
          trigger chunk window streamer is_last_chunk
        in
        match
          String.( = ) trigger terminator || Hash.is_trigger_string trigger
        with
        | false ->
            f
              (phrase ^ String.prefix chunk 1)
              (String.drop_prefix chunk 1)
              is_last_chunk parse
        | true ->
            let final_phrase = phrase ^ trigger in
            Hashtbl.incr dict_count final_phrase;
            f (String.prefix chunk 1)
              (String.drop_prefix chunk 1)
              is_last_chunk
              (Hashtbl.hash final_phrase :: parse)
    in
    ( List.rev
        (match FASTAStreamer.next streamer with
        | Continue first_chunk ->
            f
              (String.prefix first_chunk 1)
              (String.drop_prefix first_chunk 1)
              false []
        | Stop first_chunk ->
            f
              (String.prefix first_chunk 1)
              (String.drop_prefix first_chunk 1)
              true []),
      dict_count )

  let parse (filename : string) (window : int) : parse =
    let hash, dict_count = hash filename window in
    let phrases = sorted_phrases dict_count in
    let parse = sorted_parse phrases hash in
    let freqs = sorted_freqs phrases dict_count in
    (phrases, freqs, parse)

  let buildText (text : string) : text = text

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
    let d_SA =
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
