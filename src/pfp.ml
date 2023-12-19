open Core
open Naive_bwt
open Serialize
open Fasta

module type S = sig
  type text = string
  type dict
  type parse = { phrases : string list; freqs : int list; parse : int list }

  val dict_to_alist : dict -> (string * int) list
  val initialize_streamer : chunk_size:int -> string -> Fasta.FASTAStreamer.t
  val parse : ?chunk_size:int -> string -> window:int -> parse
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
  type phrase = { first : bool; start_pos : int; end_pos : int }
  type parse = { phrases : string list; freqs : int list; parse : int list }

  module Dict = Hashtbl.Make (String)

  type dict = int Dict.t

  module ParseMapper = Map.Make (String)
  module IntBWT = Naive_bwt.Text (Naive_bwt.IntSequence)
  module StringBWT = Naive_bwt.Text (Naive_bwt.CharSequence)

  let dict_to_alist (dict : dict) : (string * int) list =
    Hashtbl.to_alist dict
    |> List.sort ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2)

  let initialize_streamer ~(chunk_size : int) (filename : string) :
      FASTAStreamer.t =
    FASTAStreamer.create ~chunk_size filename

  let repeat c k = String.init k ~f:(fun _ -> c)

  let fill s c len =
    String.concat
      [
        s; repeat c (if len > String.length s then len - String.length s else 0);
      ]

  let update_dict dict k =
    let idx, _ =
      Hashtbl.update_and_return dict k ~f:(fun value ->
          match value with
          | Some (idx, freq) -> (idx, freq + 1)
          | None -> (Hashtbl.length dict, 1))
    in
    idx

  let parse ?(chunk_size = -1) (filename : string) ~(window : int) : parse =
    let sep = '$' in
    let terminator = repeat sep window in
    let dict_count = Hashtbl.create (module String) in
    let streamer = initialize_streamer filename ~chunk_size in
    let rec f (phrase : phrase) (chunk : string chunk)
        (next_chunk : string chunk option) (parse : int list) : int list =
      if phrase.end_pos > String.length chunk.contents && chunk.is_last then
        parse
      else
        let chunk, next_chunk, is_trigger_across_chunks =
          if chunk.is_last then (chunk, None, false)
          else
            let trigger_end = phrase.end_pos + window in
            if trigger_end > String.length chunk.contents then
              match next_chunk with
              | None -> (chunk, Some (FASTAStreamer.next streamer), true)
              | _ -> (chunk, next_chunk, true)
            else (chunk, next_chunk, false)
        in
        let trigger =
          let trigger_end = phrase.end_pos + window in
          if trigger_end > String.length chunk.contents then
            if chunk.is_last then
              fill (String.slice chunk.contents phrase.end_pos 0) sep window
            else
              let next_chunk = Option.value_exn next_chunk in
              let end_first = String.slice chunk.contents phrase.end_pos 0 in
              let start_second =
                String.slice next_chunk.contents 0
                  (window - String.length end_first)
              in
              String.concat [ end_first; start_second ]
          else String.slice chunk.contents phrase.end_pos trigger_end
        in
        match
          String.( = ) trigger terminator || Hash.is_trigger_string trigger
        with
        | false ->
            if is_trigger_across_chunks then
              let next_chunk = Option.value_exn next_chunk in
              f
                {
                  first = phrase.first;
                  start_pos = 0;
                  end_pos = phrase.end_pos - phrase.start_pos;
                }
                {
                  contents =
                    String.concat
                      [
                        String.slice chunk.contents phrase.start_pos 0;
                        next_chunk.contents;
                      ];
                  is_last = next_chunk.is_last;
                }
                None parse
            else
              f
                {
                  first = phrase.first;
                  start_pos = phrase.start_pos;
                  end_pos = phrase.end_pos + 1;
                }
                chunk next_chunk parse
        | true ->
            let final_phrase =
              String.concat
                [
                  String.slice chunk.contents phrase.start_pos phrase.end_pos;
                  trigger;
                ]
            in
            let final_phrase =
              if phrase.first then
                String.concat [ String.of_char sep; final_phrase ]
              else final_phrase
            in
            let idx = update_dict dict_count final_phrase in
            if is_trigger_across_chunks then
              let next_chunk = Option.value_exn next_chunk in
              f
                { first = false; start_pos = 0; end_pos = 1 }
                {
                  contents =
                    String.concat
                      [
                        String.slice chunk.contents phrase.end_pos 0;
                        next_chunk.contents;
                      ];
                  is_last = next_chunk.is_last;
                }
                None (idx :: parse)
            else
              f
                {
                  first = false;
                  start_pos = phrase.end_pos;
                  end_pos = phrase.end_pos + 1;
                }
                chunk next_chunk (idx :: parse)
    in
    let first_chunk = FASTAStreamer.next streamer in
    let parse =
      List.rev
        (f { first = true; start_pos = 0; end_pos = 0 } first_chunk None [])
    in
    let phrases =
      dict_count |> Hashtbl.keys |> List.sort ~compare:String.compare
    in
    let () = printf "Number of phrases: %d\n%!" (List.length phrases) in
    let parse =
      let sorted_parsemap =
        phrases
        |> List.mapi ~f:(fun i phrase ->
               let idx, _ = Hashtbl.find_exn dict_count phrase in
               (idx, i))
        |> Hashtbl.of_alist_exn (module Int)
      in
      List.map
        ~f:(fun placeholder -> Hashtbl.find_exn sorted_parsemap placeholder)
        parse
    in
    let freqs =
      phrases
      |> List.map ~f:(fun p ->
             let _, freq = Hashtbl.find_exn dict_count p in
             freq)
    in
    { phrases; freqs; parse }

  let buildText (text : string) : text = text
  let wrap_get list i = IntSequence.get list (i % IntSequence.length list)

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

  let parse_to_BWT (file_handle : Out_channel.t) (parse : parse) (w : int) :
      unit =
    let phrases = Array.of_list parse.phrases in
    let phrase_lengths = phrases |> Array.map ~f:String.length in
    let parse = parse.parse |> IntSequence.of_list in
    let parseSA =
      parse |> Array.map ~f:(fun x -> x + 1) |> Gsacak.GSACAK.getSA_int
    in
    printf "Parse suffix array computed\n%!";
    let bwtlast = build_bwtlast w phrases parseSA parse in
    let inv_list = build_ilist parse parseSA in
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

    (* use the gSACAk SA construction *)
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
    let process_phrase phrase_id =
      (* let cur_len = List.length bwt in *)
      let occs = Hashtbl.find_exn inv_list phrase_id in
      List.iter occs ~f:(fun p ->
          Out_channel.output_char file_handle (String.get bwtlast p))
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
        (* read off prev chars in order of ilist positions *)
        List.iter merged_ilist ~f:(fun (_, c) ->
            Out_channel.output_char file_handle c)
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
    process_alpha prev_alpha prev_phrases;
    Out_channel.close file_handle
  (* we get the reverse BWT as a list of chars, post-processing *)

  let getBWT input_string w =
    let p = parse input_string ~window:w in
    parse_to_BWT (Out_channel.create "temp") p w;
    let bwt = In_channel.read_all "temp" in
    Core_unix.remove "temp";
    bwt

  let save_parse (parse : parse) (out_dir : string) : unit =
    StringSerializer.write_list (Filename.concat out_dir "dict") parse.phrases;
    Int32Serializer.write_list (Filename.concat out_dir "freq") parse.freqs;
    Int32Serializer.write_list (Filename.concat out_dir "parse") parse.parse

  let load_parse (parse_dir : string) : parse =
    let phrases =
      StringSerializer.read_list (Filename.concat parse_dir "dict")
    in
    let freqs = Int32Serializer.read_list (Filename.concat parse_dir "freq") in
    let parse = Int32Serializer.read_list (Filename.concat parse_dir "parse") in
    { phrases; freqs; parse }
end
