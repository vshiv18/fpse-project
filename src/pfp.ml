open Core
open Naive_bwt
open Serialize
open Fasta

module type S = sig
  type text = string
  type dict
  type parse = string list * int list * int list

  val dict_to_alist : dict -> (string * int) list
  val initialize_streamer : string -> chunk_size:int -> Fasta.FASTAStreamer.t

  val trigger :
    chunk:string ->
    phrase_start:int ->
    phrase_end:int ->
    window:int ->
    FASTAStreamer.t ->
    bool ->
    text * int * int * text * bool

  val sorted_phrases : dict -> text list
  val hash : ?chunk_size:int -> string -> window:int -> int list * dict
  val parse : string -> int -> parse
  val buildText : string -> text
  val parse_to_BWT : Out_channel.t -> parse -> int -> unit
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

  let default_chunk_size = 4096

  let dict_to_alist (dict : dict) : (string * int) list =
    Hashtbl.to_alist dict
    |> List.sort ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2)

  let initialize_streamer (filename : string) ~(chunk_size : int) :
      FASTAStreamer.t =
    FASTAStreamer.create ~chunk_size filename

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

  let first_phrase_invariant (final_phrase : string) (parse : int list) : bool =
    if List.length parse = 0 then
      String.equal (String.slice final_phrase 0 1) "$"
    else true

  let trigger ~(chunk : string) ~(phrase_start : int) ~(phrase_end : int)
      ~(window : int) (streamer : FASTAStreamer.t) (is_last_chunk : bool) :
      text * int * int * text * bool =
    if phrase_end + window > String.length chunk then
      if is_last_chunk then
        ( String.pad_right ~char:'$'
            (String.slice chunk phrase_end 0)
            ~len:window,
          phrase_start,
          phrase_end,
          chunk,
          is_last_chunk )
      else
        let chunk, is_last_chunk =
          match FASTAStreamer.next streamer with
          | Continue next_chunk ->
              (String.slice chunk phrase_start 0 ^ next_chunk, false)
          | Stop next_chunk ->
              (String.slice chunk phrase_start 0 ^ next_chunk, true)
        in
        let phrase_start, phrase_end = (0, phrase_end - phrase_start) in
        ( String.slice chunk phrase_end (phrase_end + window),
          phrase_start,
          phrase_end,
          chunk,
          is_last_chunk )
    else
      ( String.slice chunk phrase_end (phrase_end + window),
        phrase_start,
        phrase_end,
        chunk,
        is_last_chunk )

  let hash ?(chunk_size = default_chunk_size) (filename : string)
      ~(window : int) : int list * dict =
    let streamer = initialize_streamer filename ~chunk_size in
    let terminator = String.pad_right ~char:'$' "" ~len:window in
    let dict_count = Hashtbl.create (module String) in
    let rec f (phrase : int * int) (chunk : text * bool) (parse : int list) :
        int list =
      let phrase_start, phrase_end = phrase in
      let chunk, is_last_chunk = chunk in
      if phrase_end > String.length chunk then parse
      else
        let trigger, phrase_start, phrase_end, chunk, is_last_chunk =
          trigger ~chunk ~phrase_start ~phrase_end ~window streamer
            is_last_chunk
        in
        match
          String.( = ) trigger terminator || Hash.is_trigger_string trigger
        with
        | false -> f (phrase_start, phrase_end + 1) (chunk, is_last_chunk) parse
        | true ->
            let final_phrase =
              String.slice chunk phrase_start phrase_end ^ trigger
            in
            let final_phrase =
              if List.length parse = 0 then String.of_char '$' ^ final_phrase
              else final_phrase
            in
            assert (first_phrase_invariant final_phrase parse);
            Hashtbl.incr dict_count final_phrase;
            f
              (phrase_end, phrase_end + 1)
              (chunk, is_last_chunk)
              (Hashtbl.hash final_phrase :: parse)
    in
    let first_chunk, is_last_chunk =
      match FASTAStreamer.next streamer with
      | Continue first_chunk -> (first_chunk, false)
      | Stop first_chunk -> (first_chunk, true)
    in
    ( List.rev (f (0, 0) (first_chunk, is_last_chunk) []),
      dict_count )

  let parse (filename : string) (window : int) : parse =
    let hash, dict_count = hash filename ~window in
    let phrases = sorted_phrases dict_count in
    let parse = sorted_parse phrases hash in
    let freqs = sorted_freqs phrases dict_count in
    (phrases, freqs, parse)

  let buildText (text : string) : text = text

  (* let wrap_nth_exn list i = List.nth_exn list (i % List.length list) *)
  let wrap_get list i = IntSequence.get list (i % IntSequence.length list)

  (* let wrap_get string i = String.get string (i % String.length string) *)

  let build_ilist (parse : int array) (parseSA : int array) :
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
    inv_list

  let build_bwtlast (w : int) (phrases : string array) (parseSA : int array)
      (parse : int array) : string =
    let w_array =
      parseSA
      |> Array.filter_mapi ~f:(fun idx i ->
             if idx = 0 then None
             else
               let cur_phrase = Array.get phrases (wrap_get parse (i - 2)) in
               Some (String.get cur_phrase (String.length cur_phrase - w - 1)))
      |> String.of_array
    in
    let () = printf "W array computed\n%!" in
    w_array

  (* TODO: *)
  let parse_to_BWT (file_handle : Out_channel.t) (parse : parse) (w : int) :
      unit =
    let phrases, _, parse = parse in
    (* let freqs = Array.of_list freqs in *)
    let phrases = Array.of_list phrases in
    let phrase_lengths = phrases |> Array.map ~f:String.length in
    let parse = parse |> IntSequence.of_list in
    let parseSA =
      parse |> Array.map ~f:(fun x -> x + 1) |> Gsacak.GSACAK.getSA_int
    in
    printf "Parse suffix array computed\n%!";
    (* printf "length of parseSA: %d\n%!" (Array.length parseSA);
       printf "parseSA:\n%s\n%!" (parseSA |> List.of_array |> List.to_string ~f:Int.to_string);
       printf "length of parse: %d\n%!" (Array.length parse); *)
    let bwtlast = build_bwtlast w phrases parseSA parse in
    let inv_list = build_ilist parse parseSA in
    (* let phrase_starts = phrases
         |> Array.folding_map ~init:(-1) ~f:(fun acc phrase ->
           (acc + (String.length phrase) + 1, acc + 1)
         )
       in *)
    (* TODO: temp, throw "bad positions" in a set and filter that way *)
    (* alternatively, get bool array of good/bad positions to filter *)
    let dict_sep = '\x01' in
    let dict_concat =
      phrases |> String.concat_array ~sep:(String.of_char dict_sep)
    in
    let total_len = String.length dict_concat in
    let _, filter_pos =
      List.fold
        (List.range ~stride:(-1) (total_len - 1) (-1))
        ~init:(total_len, [])
        ~f:(fun (last_delim, lens) idx ->
          if Char.( = ) (String.get dict_concat idx) dict_sep then
            (idx, 0 :: lens)
          else (last_delim, (last_delim - idx) :: lens))
    in
    let filter_pos =
      List.folding_mapi filter_pos ~init:0 ~f:(fun i phrase_count length ->
          if Char.( = ) (String.get dict_concat i) dict_sep then
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
      |> Array.filter_map ~f:(fun suffix_pos ->
             if suffix_pos >= Array.length filter_pos then None
             else
               let len, phrase_id = Array.get filter_pos suffix_pos in
               if len <= w then None else Some (len, phrase_id))
    in

    let () = printf "computed SA of dict\n%!" in

    (* fold, accumulator is (current output BWT, is buffer a dict phrase,
                            current representative phrase suffix, list of phrases that it occurs in) *)

    (* THIS IS JUST FOR DEBUGGING PURPOSES, REMOVE LATER
       Load in ground truth for char by char comparison. Figure out on which proper phrase suffix this breaks... *)
    (* let true_bwt = In_channel.with_file "/Users/vikram/Documents/jhu/third_year/fpse/fpse-project/chr19.bwt" ~f:(fun channel -> In_channel.input_all channel) in *)
    (* let () = printf "loaded true bwt for debugging: %s\n%!" (String.slice true_bwt 0 100) in *)
    (* REMOVE ABOVE *)
    let process_phrase phrase_id =
      (* let cur_len = List.length bwt in *)
      let occs = Hashtbl.find_exn inv_list phrase_id in
      (* printf "PHRASE PROCESSED %d:\n%!" phrase_id;
         printf "%s\n%!" (Array.get phrases phrase_id);
         printf "%s\n%!" (List.to_string ~f:(fun x -> "(" ^ (Int.to_string x) ^ ", " ^ (String.of_char (String.get bwtlast x)) ^ ")") occs); *)
      List.iter occs ~f:(fun p ->
          Out_channel.output_char file_handle (String.get bwtlast p))
      (* let added = (List.take bwt (List.length occs)) |> List.rev |> String.of_char_list in
         let truth = String.slice true_bwt cur_len (cur_len + (String.length added)) in
         if String.(truth <> added) then
           let () = printf "MISMATCH FOUND AFTER %d CHARS:\n" cur_len in
           let () = printf "Added: %s\n Expected: %s\n%!" added truth in
           exit 0; else *)
    in

    let process_alpha (prev_alpha : string) (prev_phrases : int list) : unit =
      if String.length prev_alpha = 0 then ()
      else
        (* let cur_len = List.length bwt in *)
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
        (* printf "ALPHA PROCESSED:\n%!";
           printf "%s\n%!" prev_alpha;
           printf "%s\n%!" (List.to_string ~f:(fun (x, c) -> "(" ^ (Int.to_string x) ^ ", " ^ (String.of_char c) ^ ")") merged_ilist); *)
        (* read off prev chars in order of ilist positions *)
        List.iter merged_ilist ~f:(fun (_, c) ->
            Out_channel.output_char file_handle c)
      (* let added = (List.take bwt (List.length merged_ilist)) |> List.rev |> String.of_char_list in
         let truth = String.slice true_bwt cur_len (cur_len + (String.length added)) in
         if String.(truth <> added) then
           let () = printf "MISMATCH FOUND:\n" in
           let () = printf "Added: %s\n Expected: %s\n%!" added truth in
           exit 0; else *)
    in

    (* fold through SA of dict *)
    let prev_alpha, prev_phrases =
      Array.fold d_SA ~init:("", [])
        ~f:(fun (prev_alpha, prev_phrases) (len, phrase_id) ->
          (* If suffix is a phrase from the dict, then process the buffer and set is_phrase = true to use BWTlast next iteration *)
          (* if (List.length bwt) % 1000 = 0 then printf "completed %d chars\n%!" (List.length bwt);  *)
          if len = Array.get phrase_lengths phrase_id then
            let () = process_alpha prev_alpha prev_phrases in
            let () = process_phrase phrase_id in
            ("", [])
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
            then (cur_suffix, phrase_id :: prev_phrases)
            else
              (* otherwise process buffer and start new alpha range *)
              let () = process_alpha prev_alpha prev_phrases in
              (cur_suffix, [ phrase_id ]))
    in
    process_alpha prev_alpha prev_phrases
  (* we get the reverse BWT as a list of chars, post-processing *)

  let getBWT input_string w =
    let p = parse input_string w in
    parse_to_BWT (Out_channel.create "temp") p w;
    let bwt = In_channel.read_all "temp" in
    Core_unix.remove "temp";
    bwt

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
