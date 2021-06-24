type rooted_path = {
  height : int ;
  width : int ;
  node : int array ;
  index : int array
}


let new_rooted_path m n  = {
  height = m ;
  width = n ;
  node = Array.make (m*n) (-1) ;
  index = Array.make (m*n) (-1)
} 


let default_path m n =

  let next x =
    let i, j = x / n, x mod n in
    if j = n-1 && i mod 2 = 0 then n*(i+1) + j
    else if j = 0 && i mod 2 = 1 then n*(i+1) + j
    else if i mod 2 = 0 then n*i + j+1
    else n*i + j-1
  in

  let p = new_rooted_path m n in
  let current_node = ref 0 in
  for i = 0 to m*n-1 do
    p.index.(!current_node) <- i ;
    p.node.(i) <- !current_node ;
    if i < m*n-1 then current_node := next !current_node
  done ;
  p   


let displacement dir x m n =
  let i, j = x / n, x mod n in
  let next i j = function
    | 0 -> i-1, j
    | 1 -> i, j-1
    | 2 -> i, j+1
    | _ -> i+1, j
  in
  let i1, j1 = next i j dir in
  if (i1 >= 0 && i1 < m && j1 >= 0 && j1 < n) then n*i1 + j1
  else x


let swap p i j =
  let u = p.node.(i) and v = p.node.(j) in
  p.node.(i) <- v ; p.node.(j) <- u ;
  p.index.(u) <- j ; p.index.(v) <- i


let revert p i j =
  let range = (j-i+1)/2 in
  for k = 0 to range-1 do swap p (i+k) (j-k) done


let backbite p dir =
  let y = displacement dir p.node.(0) p.height p.width in
  let j = p.index.(y) in
  revert p 0 (j-1)


let backbite_leaf p dir =
  let t = p.height * p.width in
  let y = displacement dir p.node.(t-1) p.height p.width in
  let j = p.index.(y) in
  revert p (j+1) (t-1)


let transition p nature dir =
  if nature = 0 then backbite p dir
  else backbite_leaf p dir


let same_ends p =
  let m = p.height and n = p.width in
  let root = if n mod 2 = 0 then n*(m-1) else n*m-1 in
  p.node.(0) = 0 && p.node.(n*m-1)= root