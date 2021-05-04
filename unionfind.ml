type t = 
  {parent : int Array.t ; height : int Array.t}

let create n =
  let p = Array.make n 0 in
  for i = 0 to n-1 do p.(i) <- i done ;
  {parent = p ; height = Array.make n 0}

let rec component u i =
  if u.parent.(i) = i then i
  else component u u.parent.(i)

let merge u i j =
  if u.height.(i) = u.height.(j) then u.height.(j) <- u.height.(j) + 1 ;
  if u.height.(i) <= u.height.(j) then u.parent.(i) <- j else u.parent.(j) <- i
