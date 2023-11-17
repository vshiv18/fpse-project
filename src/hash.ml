open Core

module type S = sig
  val hash : string -> int
  val next_hash : string -> int -> int
  val prev_hash : string -> int -> int
  val is_trigger_string : string -> bool
end

module type Params = sig
  val b : int
  val p : int
end

module Default : Params = struct
  let b = 256
  let p = 27162335252586509
end

module MakeRollHash (P : Params) : S = struct
  let int_pow n m = int_of_float (float_of_int n ** float_of_int m) mod P.p
  let add n m = (n + m) mod P.p
  let mul n m = n * m mod P.p

  let rec xgcd a b =
    if a = 0 then (0, 1, b)
    else
      let x1, y1, d = xgcd (b mod a) a in
      let x = y1 - (b / a * x1) in
      let y = x1 in
      (x, y, d)

  let b_inv =
    let x, _, _ = xgcd P.b P.p in
    add x P.p

  let hash (str : string) =
    str
    |> String.foldi ~init:0 ~f:(fun i acc ele ->
           add acc
             (mul (Char.to_int ele) (int_pow P.b (String.length str - i - 1))))

  let next_hash (str : string) (last : int) =
    mul last P.b |> add (Char.to_int (String.get str (String.length str - 1)))

  let prev_hash (str : string) (last : int) =
    mul last b_inv |> add (Char.to_int (String.get str 0))

  let is_trigger_string (str : string) = Int.( = ) 0 @@ hash str
end
