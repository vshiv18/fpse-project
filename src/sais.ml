open Core

module SAIS (Key_type : Base.Hashtbl.Key.S)= struct
  type t = int Array.t

  let make_buckets (text : 'a Array.t) :
      t * ('a, int) Hashtbl.t * ('a, int) Hashtbl.t * ('a, int * int) Hashtbl.t
      =
    let array = Array.create ~len:(Array.length text + 1) (-1) in
    let tbl =
      text |> Array.to_list
      |> Hashtbl.group (module Key_type)
           ~get_key:(fun x -> x)
           ~get_data:(fun _ -> 1)
           ~combine:(fun x y -> x + y)
    in
    (array, tbl)

  let getSL (text : 'a Array.t) : string =
    let rec helper (sl : char list) (pos : int) : char list =
      match Char.compare (Array.get text pos) (Array.get text (pos + 1)) with
      | x when x > 0 -> helper ('L' :: sl) (pos - 1)
      | x when x < 0 -> helper ('S' :: sl) (pos - 1)
      | _ -> helper (List.hd_exn sl :: sl) (pos - 1)
    in
    helper [ 'L'; 'S' ] (Array.length text - 2) |> String.of_char_list

  let isLMS (sl : string) (pos : int) : bool =
    if pos = 0 then false else Char.( > ) sl.[pos] sl.[pos - 1]

  let add_LMS (text : 'a Array.t) (sl : string) (buckets : t)
      (empty_slots : ('a, int * int) Hashtbl.t) =
    Array.iteri text ~f:(fun idx v ->
        if isLMS sl idx then
          let l, _ = Hashtbl.find_exn empty_slots v in
          let () = Array.set buckets l idx in
          Hashtbl.update empty_slots v ~f:(fun x ->
              match x with Some (l, r) -> (l + 1, r) | None -> (0, 0)))

  let sais (text : 'a Array.t) : t =
    let buckets, starts, ends, empty_slots = make_buckets text in
    let sl = getSL text in

    buckets (* placeholder to compile*)
end
