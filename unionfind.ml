type t = 
  {parent : int Array.t ; height : int Array.t}

let create n =
  let p = Array.make n 0 in
  for i = 0 to n-1 do p.(i) <- i done ;
  {parent = p ; height = Array.make n 0}

let component u i =
  let rec find l i =
    if u.parent.(i) = i then l, i
    else find (i :: l) u.parent.(i)
  in
  let l, j = find [] i in
  List.iter (fun k -> u.parent.(k) <- j) l ;
  j

let merge u i j =
  if u.height.(i) = u.height.(j) then u.height.(j) <- u.height.(j) + 1 ;
  if u.height.(i) <= u.height.(j) then u.parent.(i) <- j else u.parent.(j) <- i
