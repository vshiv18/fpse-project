type t = Rank_support.t

let create r = r  

let select b s bv i =
  let rec bsearch low high =
    if low > high then None else
      let mid = (low + high) / 2 in
      let rank = if b then Rank_support.rank1 s bv mid else Rank_support.rank0 s bv mid in
      if rank = i && b = Bitv.get bv @@ mid - 1 then Some mid
      else if rank > i || (rank = i && (b <> Bitv.get bv @@ mid - 1)) then bsearch low @@ mid - 1
      else bsearch (mid + 1) high in
  bsearch 1 @@ Bitv.length bv

let select1 = select true

let select0 = select false

let overhead = Rank_support.overhead
