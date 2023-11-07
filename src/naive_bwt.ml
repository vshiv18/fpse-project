open Core

module Text = struct
  (* type t = int [@@deriving sexp] *)
  type text = string
  let setText (intext : string) : text = intext
  let compare (t : text) (a : int) (b : int) : int =
    let len = String.length t in
    let endidx = if a > b then a else b in 
      List.fold_until (List.range 0 (len - endidx))
        ~init:0
        ~f:(fun _ offset -> let comp = Char.compare t.[a + offset] t.[b + offset] in
            if comp = 0 then Continue 0 else Stop comp)
        ~finish:(fun _ -> if a > b then -1 else 1)
  let getSA (t : text) =
    List.range 0 (String.length t)
    |> List.sort ~compare:(compare t)
  let getSuffix (t : text) (idx : int) : string = String.drop_prefix t idx
  let getBWT (t : text) : string = 
    (String.length t) :: (getSA t)
    (* |> List.map ~f:(fun idx -> if idx = 0 then t.[(String.length t) - 1] else t.[idx - 1]) *)
    |> List.map ~f:(fun idx -> if idx = 0 then '$' else t.[idx - 1])
    |> String.of_char_list 
  end
  