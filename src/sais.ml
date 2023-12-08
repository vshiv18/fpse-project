open Core

module SAIS (Key_type : Base.Hashtbl.Key.S) = struct
  module Buckets = struct
    type t = { arr : int Array.t; slots : (Key_type.t, int * int) Hashtbl.t }

    let to_list b =
      Hashtbl.to_alist b.slots

    let get_slots counts =
      let empty_slots_ls, _ =
        Hashtbl.keys counts
        |> List.sort ~compare:Key_type.compare
        |> List.fold ~init:([], 1) ~f:(fun (slots, prior) ele ->
               let curr = prior + Hashtbl.find_exn counts ele in
               ((ele, (prior, curr - 1)) :: slots, curr))
      in
      empty_slots_ls |> Hashtbl.of_alist_exn (module Key_type)

    let construct (text : 'a Array.t) : t =
      let arr = Array.create ~len:(Array.length text + 1) (-1) in
      (* Add the empty suffix bucket, length of string *)
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
      { arr; slots }

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

  let slot_LMS (text : 'a Array.t) (sl : string) (buckets : Buckets.t) =
    Array.iteri text ~f:(fun idx v ->
        if isLMS sl idx then Buckets.set_bucket buckets idx v 'S')

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

  (* let LMS_summary (text : 'a Array.t) (sl : string) (buckets : buckets) =
     let lmsTextStarts = Array.create ~len:(Array.length text) (-1) in
     buckets |> Array.foldi ~f:(fun idx ele -> if isLMS sl ele then *)

  (* let sais (text : 'a Array.t) : Buckets =
     let buckets = Buckets.construct text in
     let sl = getSL text in
     slot_LMS text sl buckets;
     induce_L text sl buckets;
     induce_S text sl buckets;
     buckets *)
end
