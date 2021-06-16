open Tree

let add_edge (i, j) t =
  t.children.(j) <- i :: t.children.(j) ;
  t.parent.(i) <- j

let remove_edge (i, j) t =
  t.children.(j) <- List.filter ((!=) i) t.children.(j) ;
  t.parent.(i) <- -1

let aldous_transform y t =
  let z = t.parent.(y) in
  add_edge (t.root, y) t ;
  t.root <- y ;
  remove_edge (y, z) t ;
  z

let reverse_aldous_transform z x t =
  add_edge (t.root, z) t ;
  remove_edge (x, t.root) t ;
  t.root <- x

let markov_transition g cost t =
  let x = t.root in
  let y, _ = List.nth g.(x) (Random.int (List.length g.(x))) in
  let z = aldous_transform y t in
  let new_cost = Tree.routing_cost g t in
  if Random.float 1. < (new_cost -. cost) /. cost *. 1000. then (
    reverse_aldous_transform z x t ;
    cost
  )
  else new_cost

