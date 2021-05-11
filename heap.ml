type 'a t = {
  heap : ('a * int) Array.t ; 
  max_size : int ;
  mutable size : int ;
}

let create n x = 
  {heap = Array.make (n+1) (x, max_int) ; max_size = n ; size = 0}

let swap h i j =
  let x = h.(i) in h.(i) <- h.(j) ; h.(j) <- x

let ascend h k =
  let rec aux i =
    if i > 1 && snd h.(i) < snd h.(i/2) then (swap h i (i/2) ; aux (i/2))
  in aux k

let descend h k m =
  let rec aux i =
    let j =
      if 2*i+1 <= m && snd h.(2*i) >= snd h.(2*i+1) then 2*i+1
      else if 2*i = m then 2*i
      else i
    in
    if snd h.(i) > snd h.(j) then (swap h i j ; aux j)
  in aux k

let top h =
  if h.size = 0 then failwith "empty heap" else fst h.heap.(1)

let pop h =
  let m = h.size in
  if m = 0 then failwith "empty heap"
  else
    let x, p = h.heap.(1) in
    swap h.heap 1 m ;
    descend h.heap 1 (m-1) ;
    h.size <- h.size - 1 ;
    x

let add h x p =
  let m = h.size in
  if m = h.max_size then failwith "full heap"
  else
    h.heap.(m) <- (x, p) ;
    ascend h.heap (m-1) ;
    h.size <- h.size + 1


