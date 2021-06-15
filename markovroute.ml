open Tree

let add_edge (i, j, c) t =
  t.children.(j) <- (i, c) :: t.children.(j) ;
  t.parent.(i) <- j, c

let remove_edge (i, j) t =
  let rec aux d l = function
    | [] -> d, l
    | (y, c) :: q when y = i -> aux c l q
    | e :: q -> aux d (e :: l) q
  in
  let d, l = aux 0. [] t.children.(j) in
  t.children.(j) <- l ;
  t.parent.(i) <- -1, 0. ;
  d

let aldous_transform (y, c) t =
  let z = fst t.parent.(y) in
  add_edge (t.root, y, c) t ;
  t.root <- y ;
  z, remove_edge (y, z) t 

let reverse_aldous_transform (z, d, x) t =
  add_edge (t.root, z, d) t ;
  let _ = remove_edge (x, t.root) t in
  t.root <- x

let markov_transition g cost t =
  let x = t.root in
  let y, c = List.nth g.(x) (Random.int (List.length g.(x))) in
  let z, d = aldous_transform (y, c) t in
  let new_cost = Tree.routing_cost t in
  if Random.float 1. < (new_cost -. cost) /. cost *. 1000. then (
    reverse_aldous_transform (z, d, x) t ;
    cost
  )
  else new_cost

