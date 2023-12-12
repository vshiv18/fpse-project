open Core

module SAIS = struct
  module SAIS_Step (Key_type : Base.Hashtbl.Key.S) = struct
    module Buckets = struct
      type t = {
        arr : int Array.t;
        counts : (Key_type.t, int) Hashtbl.t;
        mutable slots : (Key_type.t, int * int) Hashtbl.t;
      }

      let construct (text : 'a Array.t) : t =
        let arr = Array.create ~len:(Array.length text + 1) (-1) in
        (* Add the empty suffix bucket to be the length of the string, lexiocographically first *)
        let () = Array.set arr 0 @@ Array.length text in
        let counts =
          text |> Array.to_list
          |> Hashtbl.group
               (module Key_type)
               ~get_key:(fun x -> x)
               ~get_data:(fun _ -> 1)
               ~combine:(fun x y -> x + y)
        in
        let empty_slots_ls, _ =
          Hashtbl.keys counts
          |> List.sort ~compare:Key_type.compare
          |> List.fold ~init:([], 1) ~f:(fun (slots, prior) ele ->
                 let curr = prior + Hashtbl.find_exn counts ele in
                 ((ele, (prior, curr - 1)) :: slots, curr))
        in
        let slots = empty_slots_ls |> Hashtbl.of_alist_exn (module Key_type) in
        { arr; counts; slots }

      let reset_slots (buckets : t) =
        let empty_slots_ls, _ =
          Hashtbl.keys buckets.counts
          |> List.sort ~compare:Key_type.compare
          |> List.fold ~init:([], 1) ~f:(fun (slots, prior) ele ->
                 let curr = prior + Hashtbl.find_exn buckets.counts ele in
                 ((ele, (prior, curr - 1)) :: slots, curr))
        in
        buckets.slots <-
          empty_slots_ls |> Hashtbl.of_alist_exn (module Key_type)

      let reset_buckets (buckets : t) =
        let () =
          Array.fill buckets.arr ~pos:0 ~len:(Array.length buckets.arr) (-1)
        in
        Array.set buckets.arr 0 @@ (Array.length buckets.arr - 1)

      let set_bucket (buckets : t) (idx : int) (v : 'a) (typ : char) =
        let l, r = Hashtbl.find_exn buckets.slots v in
        let pos =
          match typ with
          | 'L' ->
              let () =
                Hashtbl.update buckets.slots v ~f:(fun x ->
                    match x with Some (l, r) -> (l + 1, r) | None -> (0, 0))
              in
              l
          | 'S' ->
              let () =
                Hashtbl.update buckets.slots v ~f:(fun x ->
                    match x with Some (l, r) -> (l, r - 1) | None -> (0, 0))
              in
              r
          | _ -> raise @@ Invalid_argument "Incorrect type: L/S expected"
        in
        Array.set buckets.arr pos idx
    end

    type t = { sl : string; buckets : Buckets.t }

    let getSL (text : 'a Array.t) : string =
      let rec helper (sl : char list) (pos : int) : char list =
        if pos < 0 then sl
        else
          match
            Key_type.compare (Array.get text pos) (Array.get text (pos + 1))
          with
          | x when x > 0 -> helper ('L' :: sl) (pos - 1)
          | x when x < 0 -> helper ('S' :: sl) (pos - 1)
          | _ -> helper (List.hd_exn sl :: sl) (pos - 1)
      in
      helper [ 'L'; 'S' ] (Array.length text - 2) |> String.of_char_list

    let isLMS (sl : string) (pos : int) : bool =
      if pos = 0 then false else Char.( > ) sl.[pos] sl.[pos - 1]

    let guess_LMS (text : 'a Array.t) (sl : string) (buckets : Buckets.t) =
      Array.iteri text ~f:(fun idx v ->
          if isLMS sl idx then Buckets.set_bucket buckets idx v 'S')

    let accurate_LMS (text : 'a Array.t) (buckets : Buckets.t)
        (summary_string : int Array.t) (summary_offets : int Array.t) =
      Array.fold_right summary_string ~init:() ~f:(fun summary_sa () ->
          if summary_sa < Array.length summary_offets then
            let text_pos = Array.get summary_offets summary_sa in
            if text_pos < Array.length text then
              let text_ele = Array.get text text_pos in
              Buckets.set_bucket buckets text_pos text_ele 'S')

    let induce_L (text : 'a Array.t) (sl : string) (buckets : Buckets.t) =
      Array.iter buckets.arr ~f:(fun ele ->
          match ele with
          | i when i - 1 >= 0 && Char.equal (String.get sl (i - 1)) 'L' ->
              Buckets.set_bucket buckets (i - 1) (Array.get text (i - 1)) 'L'
          | _ -> ())

    let induce_S (text : 'a Array.t) (sl : string) (buckets : Buckets.t) =
      (* this is reverse iteration, the accumulator is a unit *)
      Array.fold_right buckets.arr ~init:() ~f:(fun ele () ->
          match ele with
          | i when i - 1 >= 0 && Char.equal (String.get sl (i - 1)) 'S' ->
              Buckets.set_bucket buckets (i - 1) (Array.get text (i - 1)) 'S'
          | _ -> ())

    let substrings_are_equal (text : 'a Array.t) (sl : string) (offset1 : int)
        (offset2 : int) : bool =
      if offset1 = Array.length text || offset2 = Array.length text then false
      else
        List.fold_until
          (List.range 0 @@ Array.length text)
          ~init:true
          ~f:(fun _ i ->
            match (isLMS sl @@ (offset1 + i), isLMS sl @@ (offset2 + i)) with
            | true, true when i > 0 -> Stop true
            | x, y when Bool.( <> ) x y -> Stop false
            | _, _
              when Key_type.compare
                     (Array.get text @@ (offset1 + i))
                     (Array.get text @@ (offset2 + i))
                   <> 0 ->
                Stop false
            | _, _ -> Continue true)
          ~finish:Fn.id

    let lms_summary (text : 'a Array.t) (sl : string) (buckets : Buckets.t) =
      let lmsTextStarts = Array.create ~len:(Array.length text + 1) (-1) in
      let counter = ref 0 in
      let last_lms_pos = ref 0 in
      let update_counter text_pos =
        if not (substrings_are_equal text sl text_pos !last_lms_pos) then
          counter := !counter + 1
      in
      let insert_lms_start text_offset =
        last_lms_pos := text_offset;
        Array.set lmsTextStarts text_offset !counter
      in
      let () = insert_lms_start @@ Array.get buckets.arr 0 in
      let () =
        buckets.arr
        |> Array.iteri ~f:(fun i text_pos ->
               if i > 0 && isLMS sl text_pos then
                 let () = update_counter text_pos in
                 insert_lms_start text_pos)
      in
      let summary_text_pos, summary_string =
        lmsTextStarts
        |> Array.mapi ~f:(fun idx name -> (idx, name))
        |> Array.filter ~f:(fun (_, name) -> name <> -1)
        |> Array.unzip
      in
      (summary_text_pos, summary_string, !counter + 1)

    let init (text : 'a Array.t) : t =
      { sl = getSL text; buckets = Buckets.construct text }

    let step (s : t) (text : 'a Array.t) =
      guess_LMS text s.sl s.buckets;
      induce_L text s.sl s.buckets;
      Buckets.reset_slots s.buckets;
      induce_S text s.sl s.buckets;
      lms_summary text s.sl s.buckets

    let merge (s : t) (text : 'a Array.t) (sorted_summary : int Array.t)
        (sorted_offsets : int Array.t) =
      Buckets.reset_buckets s.buckets;
      Buckets.reset_slots s.buckets;
      accurate_LMS text s.buckets sorted_summary sorted_offsets;
      induce_L text s.sl s.buckets;
      Buckets.reset_slots s.buckets;
      induce_S text s.sl s.buckets;
      s.buckets.arr
  end

  module CharSAIS = SAIS_Step (Char)
  module IntSAIS = SAIS_Step (Int)

  let sort_easy summary_string =
    let sa = Array.create ~len:(Array.length summary_string + 1) (-1) in
    let () = Array.set sa 0 @@ Array.length summary_string in
    let () =
      summary_string
      |> Array.iteri ~f:(fun idx ele -> Array.set sa (ele + 1) idx)
    in
    sa

  let rec getSA_int (text : int Array.t) =
    let sais_t = IntSAIS.init text in
    let summary_idx, summary_string, summary_sigma =
      text |> IntSAIS.step sais_t
    in
    let sorted_summary =
      if summary_sigma = Array.length summary_string then
        sort_easy summary_string
      else getSA_int summary_string
    in
    IntSAIS.merge sais_t text sorted_summary summary_idx

  let getSA (text : string) =
    let text_arr = text |> String.to_array in
    let sais_t = CharSAIS.init text_arr in
    let summary_idx, summary_string, summary_sigma =
      CharSAIS.step sais_t text_arr
    in
    let sorted_summary =
      if summary_sigma = Array.length summary_string then
        sort_easy summary_string
      else getSA_int summary_string
    in
    CharSAIS.merge sais_t text_arr sorted_summary summary_idx

  let getBWT (text : string) : string =
    text |> getSA
    |> Array.map ~f:(fun idx ->
           if idx = 0 then '$' else String.get text (idx - 1))
    |> String.of_array
end
