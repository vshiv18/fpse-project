open Core

module type Sequence = sig
  module Item : sig
    type t [@@deriving compare, sexp, hash]
  end

  type t

  val null : Item.t
  val get : t -> int -> Item.t
  val length : t -> int
  val of_list : Item.t list -> t
  val to_list : t -> Item.t list
  val of_seq : t -> t
  val fold : t -> init:'acc -> f:('acc -> Item.t -> 'acc) -> 'acc
end

module CharSequence : Sequence with type t = string and type Item.t = char =
struct
  module Item = Char

  type t = string

  let null = '$'
  let get s idx = s.[idx]
  let length = String.length
  let of_list = String.of_char_list
  let to_list = String.to_list
  let of_seq (s : string) : t = s
  let fold = String.fold
end

module IntSequence :
  Sequence with type t = Int.t Array.t and type Item.t = int = struct
  module Item = Int

  type t = Int.t Array.t

  let null = -1

  (* let compare_suffixes idx1 idx2 *)
  let get (a : t) (n : int) = Array.get a n
  let length = Array.length
  let of_list = Array.of_list
  let to_list = Array.to_list
  let of_seq (s : int array) : t = s
  let fold = Array.fold
end

module Text (Sequence : Sequence) = struct
  type text = Sequence.t

  let buildText (intext : Sequence.t) : text = intext

  let compare (t : text) (a : int) (b : int) : int =
    let len = Sequence.length t in
    let endidx = if a > b then a else b in
    List.fold_until
      (List.range 0 (len - endidx))
      ~init:0
      ~f:(fun _ offset ->
        let comp =
          Sequence.Item.compare
            (Sequence.get t (a + offset))
            (Sequence.get t (b + offset))
        in
        if comp = 0 then Continue 0 else Stop comp)
      ~finish:(fun _ -> if a > b then -1 else 1)

  let getSA (t : text) =
    List.range 0 (Sequence.length t) |> List.sort ~compare:(compare t)

  (* let getSuffix (t : text) (idx : int) : text = String.drop_prefix t idx *)

  let getBWT (seq : text) : text =
    let text = Sequence.of_seq seq in
    Sequence.length text :: getSA text
    |> List.map ~f:(fun idx ->
           if idx = 0 then Sequence.null else Sequence.get text (idx - 1))
    |> Sequence.of_list

  (* let bwt_from_SA (seq : text) (sa : int list) =
    let text = Sequence.of_seq seq in
    List.length sa :: sa
    |> List.map ~f:(fun idx ->
           if idx = 0 then Sequence.null else Sequence.get text (idx - 1))
    |> Sequence.of_list *)

  let rle_BWT string =
    Sequence.fold string ~init:[] ~f:(fun acc ele ->
        match acc with
        | [] -> [ (ele, 1) ]
        | (prev, count) :: tl when Int.( = ) 0 @@ Sequence.Item.compare prev ele
          ->
            (prev, count + 1) :: tl
        | _ -> (ele, 1) :: acc)
    |> List.rev
end
