let del i l =
  List.filter (fun u -> fst u != i) l

let add_edge (i, j) parent (root, w_children) =
  w_children.(j) <- (i, c) :: w_children.(j) ;
  parent.(i) <- j

let remove_edge (i, j) parent (root, w_children) =
  w_children.(j) <- del y w_children.(j) ;
  parent.(i) <- -1

let aldous_transform g (y, c) parent (root, w_children) =
  (* let y, c = List.nth g.(root) (Random.int (List.length g.(root))) in *)
  let z = parent.(y) in
  w_children.(y) <- (root, c) :: w_children.(y) ;
  parent.(root) <- y ;
  w_children.(z) <- del y w_children.(z) ;
  parent.(y) <- -1

let reverse_aldous_transform g (z, d) (r, c) parent (root, w_children) =
  



let markov_transition g parent (root, w_children) =

  let pa