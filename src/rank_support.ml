type t = {
    r_s : Bitv.t;
    r_b : Bitv.t;
    r_p : Bitv.t
  }

open Bitv
let bv_get_chunk arr idx len =
  to_int_us @@ (* should be constant-ish time *)
    let arr' = (*Printf.printf "bvlen=%d, idx=%d, size=%d\n" (length arr) idx len;*) create Sys.int_size false in
    blit arr idx arr' 0 len; arr'
    (*sub arr idx Sys.int_size in
    fill arr' len (Sys.int_size - len) false; arr'*)

let bv_get_int arr idx size =
  (*Printf.printf "get_int idx=%d size=%d\n" idx size;*)
  bv_get_chunk arr (idx * size) size

let bv_get_bit arr idx = if get arr idx then 1 else 0

let create bv =
  let log2 a = log (float_of_int a) /. log 2.0 in
  let log2_n = log2 @@ length bv in
  let x = max 2 @@ int_of_float log2_n in
  (* do logs early so that s/b = int(log2 n) or int(log2 n) - 1) is nice *)
  let s = x * x / 2 and
      b = x / 2 (*int_of_float @@ ceil @@ log2_n /. 2.0*) and
      bv_set_int v arr idx size = blit (of_int_us v) 0 arr (idx * size) size in
  let r_s_el_size = int_of_float @@ ceil @@ log2_n and
      r_b_el_size = int_of_float @@ ceil @@ log2 @@ s and
      r_p_el_size = int_of_float @@ ceil @@ log2 @@ b + 1 and
      dim_r_b = (b - 1 + length bv) / b in
  let r_s =
    let dim = (s - 1 + length bv) / s in
    let arr = create (dim * r_s_el_size + max 0 (Sys.int_size - r_s_el_size + 2)) false in
    for j = 1 to dim - 1 do
       let cum_rank =
         let rec rank k =
           bv_get_bit bv k +
             if k = (j-1)*s then
               bv_get_int arr (j-1) r_s_el_size else
               rank (k-1)
         in rank @@ j*s - 1 in
       bv_set_int cum_rank arr j r_s_el_size
    done; arr and
      r_b =
        let arr = create (dim_r_b * r_b_el_size + max 0 (Sys.int_size - r_b_el_size + 1)) false in
        for j = 1 to dim_r_b - 1 do
          let local_rank =
            let rec rank k =
              if k mod s = 0 then 0 else
                if k = (j-1) * b then bv_get_int arr (j-1) r_b_el_size
                else rank (k-1) + (bv_get_bit bv @@ k-1) in
            rank @@ j*b in
          bv_set_int local_rank arr j r_b_el_size
        done; arr and
      (* note r_p is done in little endian order as opposed to the examples *)
     r_p =
        let dim1 = 1 lsl b and
            dim2 = b in
        let arr = create (dim1 * dim2 * r_p_el_size + max 0 (Sys.int_size - r_p_el_size)) false in
        (*Printf.printf "dim1=%d, dim2=%d length=%d\n" dim1 dim2 @@ length arr;*)
        for i = 1 to dim1 - 1 do
          bv_set_int (i mod 2) arr (i * dim2) r_p_el_size;
          for j = 1 to dim2 - 1 do
            bv_set_int ((i lsr j mod 2) + bv_get_int arr (i * dim2 + j - 1) r_p_el_size) arr (i * dim2 + j) r_p_el_size
          done
       done; arr in
  {
    r_s = r_s;
    r_b = r_b;
    r_p = r_p
  }

let rank1 r bv j =
  let log2 a = log (float_of_int a) /. log 2.0 in
  let log2_n = log2 @@ length bv in
  let x = max 2 @@ int_of_float log2_n in
  (* do logs early so that s/b = int(log2 n) or int(log2 n) - 1) is nice *)
  let s = x * x / 2 and
      b = x / 2 (*int_of_float @@ ceil @@ log2_n /. 2.0*) in
  let r_s_el_size = int_of_float @@ ceil @@ log2_n and
      r_b_el_size = int_of_float @@ ceil @@ log2 s and
      r_p_el_size = int_of_float @@ ceil @@ log2 @@ b + 1 and
      dim_r_b = (b - 1 + length bv) / b in
  (*    dim_r_s = (s - 1 + length r.bv') / s in
  let print_bv name bv size dim =
    Printf.printf "%s " name; for i = 0 to dim - 1 do
                                Printf.printf "%d " @@ bv_get_int bv i size
                              done; Printf.printf "\n" in
  print_bv "r_s" r.r_s r_s_el_size dim_r_s; print_bv "r_b" r.r_b r_b_el_size ((b - 1 + length r.bv') / b); print_bv "r_p" r.r_p r_p_el_size ((1 lsl b) * b);     *)
  let rank1_h i = 
    let block_idx = i / b in
    let r_s_comp = bv_get_int r.r_s (i / s) r_s_el_size and
        r_b_comp = bv_get_int r.r_b block_idx r_b_el_size and
        r_p_comp = bv_get_int r.r_p (i - block_idx * b + b * (bv_get_chunk bv (block_idx * b) (max 0 @@ i - block_idx * b))) r_p_el_size in
    (*Printf.printf "rank1: r_s: s=%d, comp=%d; r_b: b=%d, comp=%d; r_p: l=%d, comp=%d block_idx=%d\n" s r_s_comp b r_b_comp (length r.r_p) r_p_comp block_idx;*)
      r_s_comp + r_b_comp + r_p_comp in
    if j = length bv && j / b = dim_r_b then bv_get_bit bv (j - 1) + rank1_h (j - 1) else rank1_h j

let rank0 r bv i = i - rank1 r bv i

let overhead r =
  let round_up_to_int_bits n = (n + Sys.int_size - 1) / Sys.int_size * Sys.int_size in
  round_up_to_int_bits (length r.r_s) + round_up_to_int_bits (length r.r_b) + round_up_to_int_bits (length r.r_p)
