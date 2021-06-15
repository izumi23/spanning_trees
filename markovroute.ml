open Tree

let del i l =
  List.filter (fun u -> fst u != i) l

let add_edge (i, j, c) t =
  t.children.(j) <- (i, c) :: t.children.(j) ;
  t.parent.(i) <- j, c

let remove_edge (i, j) t =
  t.children.(j) <- del i t.children.(j) ;
  t.parent.(i) <- -1, 0.

let aldous_transform (y, c) t =
  let z = fst t.parent.(y) in
  add_edge (t.root, y, c) t ;
  remove_edge (y, z) t ;
  t.root <- y

let reverse_aldous_transform (x, c) t =
  let z = fst t.parent.(t.root) in
  add_edge (x, t.root, c) t ;
  remove_edge (t.root, z) t ;
  t.root <- x

let markov_transition g cost t =
  let x = t.root in
  let y, c = List.nth g.(x) (Random.int (List.length g.(x))) in
  aldous_transform (y, c) t ;
  let new_cost = Tree.routing_cost t in
  if Random.float 1. > (cost /. new_cost) ** 2. then (
    reverse_aldous_transform (x, c) t ;
    cost
  )
  else new_cost

