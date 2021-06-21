type rooted_path = {
  side : int ;
  mutable root : int ;
  mutable leaf : int ;
  parent : int array ;
  child : int array
}

let new_rooted_path n = {
  side = n ;
  root = n*n-1 ;
  leaf = 0 ;
  parent = Array.make (n*n) (-1) ;
  child = Array.make (n*n) (-1)
} 

let add p x y = 
  p.parent.(x) <- y ;
  p.child.(y) <- x

let del p x y =
  if p.parent.(x) = y then p.parent.(x) <- -1 ;
  if p.child.(y) = x then p.child.(y) <- -1

let displacement dir x n =
  let i, j = x / n, x mod n in
  let next i j = function
    | 0 -> i-1, j
    | 1 -> i, j-1
    | 2 -> i, j+1
    | _ -> i+1, j
  in
  let i1, j1 = next i j dir in
  if (i1 >= 0 && i1 < n && j1 >= 0 && j1 < n) then n*i1 + j1
  else x

let revert p origin dest =
  let rec aux x y =
    let z = p.parent.(y) in
    add p y x ;
    if y != dest then aux y z
  in
  aux origin p.parent.(origin)

let default_path n =
  let next x =
    let i, j = x / n, x mod n in
    if j = n-1 && i mod 2 = 0 then n*(i+1) + j
    else if j = 0 && i mod 2 = 1 then n*(i+1) + j
    else if i mod 2 = 0 then n*i + j+1
    else n*i + j-1
  in
  let p = new_rooted_path n in
  for x = 0 to n*n-1 do
    if next x < n*n then add p x (next x)
  done ;
  if n mod 2 = 0 then p.root <- n*(n-1) ;
  p

let backbite p dir =
  let n = p.side in
  let x = p.root in
  let y = displacement dir x n in
  if x != y && p.parent.(y) != x then (
    let z = p.parent.(y) in
    revert p z x ;
    del p y z ;
    add p y x ;
    p.root <- z
  )

let backbite_leaf p dir =
  let n = p.side in
  let x = p.leaf in
  let y = displacement dir x n in
  if x != y && p.child.(y) != x then (
    let z = p.child.(y) in
    revert p x z ;
    del p z y ;
    add p x y ;
    p.leaf <- z
  )

let transition p nature dir =
  if nature = 0 then backbite p dir
  else backbite_leaf p dir

let same_ends p =
  let n = p.side in
  let root = if n mod 2 = 0 then n*(n-1) else n*n-1 in
  p.leaf = 0 && p.root = root