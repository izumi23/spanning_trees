type t = {
  heap : (int * float) Array.t ; 
  max_size : int ;
  mutable size : int ;
  index : int Array.t
}

let create n = 
  {heap = Array.make (n+1) (n, max_float) ; max_size = n ; size = 0 ;
  index = Array.make n n}

let swap h i j index =
  let d = index.(fst h.(i)) in
  index.(fst h.(i)) <- index.(fst h.(j)) ; index.(fst h.(j)) <- d ;
  let x = h.(i) in h.(i) <- h.(j) ; h.(j) <- x

let ascend h k index =
  let rec aux i =
    if i > 1 && snd h.(i) < snd h.(i/2) then (swap h i (i/2) index ; aux (i/2))
  in aux k

let descend h k m index =
  let rec aux i =
    let j =
      if 2*i+1 <= m && snd h.(2*i) >= snd h.(2*i+1) then 2*i+1
      else if 2*i = m then 2*i
      else i
    in
    if snd h.(i) > snd h.(j) then (swap h i j index ; aux j)
  in aux k

let top h =
  if h.size = 0 then failwith "empty heap" else fst h.heap.(1)

let pop h =
  let m = h.size in
  if m = 0 then failwith "empty heap"
  else
    let x, p = h.heap.(1) in
    swap h.heap 1 m h.index ;
    descend h.heap 1 (m-1) h.index ;
    h.size <- h.size - 1 ;
    x

let add h x p =
  let m = h.size in
  if m = h.max_size then failwith "full heap"
  else
    h.heap.(m+1) <- (x, p) ;
    h.index.(x) <- m+1 ;
    ascend h.heap (m+1) h.index ;
    h.size <- h.size + 1

let update h x p =
  let i = h.index.(x) in
  if p < snd h.heap.(i) then (h.heap.(i) <- (x, p) ; ascend h.heap i h.index)

let print h =
  let q = h.heap and n = h.size in
  Printf.printf "[|" ;
  for i = 1 to n-1 do Printf.printf "(%d, %f); " (fst q.(i)) (snd q.(i)) done ;
  Printf.printf "(%d, %f)|]\n" (fst q.(n)) (snd q.(n)) ;