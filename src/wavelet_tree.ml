module type S =
  sig
    type elt
    type t
    val build : elt list -> t
    val access : t -> int -> elt
    val rank : t -> elt -> int -> int option
    val select : t -> elt -> int -> int option
    val alphabet_size: t -> int
    val cardinal : t -> int
  end

module Make(Ord: Set.OrderedType) =
  struct
    type elt = Ord.t

    type btree = Leaf of int | Node of Bitv.t * Rank_support.t * Select_support.t *  btree * btree
    (** Waste a little extra space including select support, but it just holds pointers to the same arrays as rank support *)

    type t = {
        length: int;
        alphabet: elt array; (** sorted *)
        root: btree
      }

    let rec bsearch arr low high c =
      if low >= high then None
      else
        let mid = (low + high) / 2 in
        if Ord.compare arr.(mid) c = 0 then Some mid
        else if Ord.compare arr.(mid) c > 0 then bsearch arr low mid c
        else bsearch arr (mid + 1) high c

    (** This is the naive partition-based construction algorithm. *)
    let build s =
      let arr = List.sort_uniq Ord.compare s |> List.to_seq |> Array.of_seq in
      let list = List.rev @@ List.rev_map (fun c -> Option.get @@ bsearch arr 0 (Array.length arr) c) s in
      let rec make_tree list' bv start fin =
        if start + 1 >= fin then Leaf start else
          let len_left = ref 0 and idx = ref 0 in
          let mid = (start + fin + 1) / 2 in
          let left, right = List.partition (fun el ->
                                idx := !idx + 1;
                                if el < mid then begin
                                    len_left := !len_left + 1; true end
                                else begin
                                    Bitv.set bv (!idx - 1) true; false end) list' in
          let len_right = (Bitv.length bv) - !len_left and
              r = Rank_support.create bv in
          Node(bv, r, Select_support.create r,
               make_tree left (Bitv.create !len_left false) start mid,
               make_tree right (Bitv.create len_right false) mid fin) in
      let len = List.length list in
      {length = len;
       alphabet = arr;
       root = make_tree list (Bitv.create len false) 0 (Array.length arr)
      }

    let access wt i =
      let rec access_h tree i = match tree with
        | Leaf n -> wt.alphabet.(n)
        | Node (bv, r, _, left, right) ->
           if Bitv.get bv i then access_h left @@ Rank_support.rank0 r bv i
           else access_h right @@ Rank_support.rank1 r bv i in
      access_h wt.root i

    let rank wt c i =
      (*let rec print_preorder = function
    | Leaf n -> Printf.printf "n=%d, " n; Printf.printf "c=%c\n" wt.alphabet.(n)
    | Node (bv, r, s, left, right) ->
       Bitv.iter (fun b -> Printf.printf "%d" (if b then 1 else 0)) bv;
       Printf.printf "\n"; print_preorder left; print_preorder right in
  print_preorder wt.root;*)
      let c' = bsearch wt.alphabet 0 (Array.length wt.alphabet) c in
      Option.bind c'
        @@ fun c' ->
           (* start, fin are indices in the alphabet *)
           let rec rank_h tree start fin i = match tree with
             | Leaf _ -> None
             | Node (bv, r, _, left, right) ->
                let mid = (start + fin + 1) / 2 in
                let which_child = c' >= mid in
                let rank = if which_child then Rank_support.rank1 r bv i
                           else Rank_support.rank0 r bv i in
                let res = (if which_child then rank_h right mid fin else
                             rank_h left start mid) rank in
                if Option.is_none res then Some rank else res in
           match wt.root with
           | Leaf n -> if c' = n then Some i else None
           | Node _ -> rank_h wt.root 0 (Array.length wt.alphabet) i

    let select wt c i =
      let c' = bsearch wt.alphabet 0 (Array.length wt.alphabet) c in
      Option.bind c'
      @@ fun c' ->
         (*Printf.printf "c'=%d\n" c';*)
         let rec select_h tree start fin acc = match tree with
           | Leaf _ -> (*Printf.printf "select: %c\n" wt.alphabet.(n);*)
              List.fold_left (fun acc' (bv, s, b) ->
                  Option.bind acc' @@
                    (if b then Select_support.select1 else Select_support.select0) s bv)
                (Some i) acc
           | Node (bv, _, s, left, right) ->
              let mid = (start + fin + 1) / 2 in
              if c' < mid then
                select_h left start mid ((bv, s, false)::acc)
              else
                select_h right mid fin ((bv, s, true)::acc) in
         match wt.root with
         | Leaf _ -> if i > wt.length then None else Some i
         | Node _ -> select_h wt.root 0 (Array.length wt.alphabet) []

    let alphabet_size wt = Array.length wt.alphabet

    let cardinal wt = wt.length
  end
