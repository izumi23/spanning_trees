type weighted_tree = {
  parent : (int * float) array ;
  mutable root : int ;
  children : (int * float) list array
}

let construct_tree parent =
  let n = Array.length parent in
  let root = ref 0 in
  let children = Array.make n [] in
  for i = 0 to n-1 do
    let j = parent.(i) in
    if j = -1 then root := i
    else children.(j) <- i :: children.(j)
  done ;
  !root, children

let construct_weighted_tree w_parent =
  let n = Array.length w_parent in
  let root = ref 0 in
  let w_children = Array.make n [] in
  for i = 0 to n-1 do
    let j, c = w_parent.(i) in
    if j = -1 then root := i
    else w_children.(j) <- (i, c) :: w_children.(j)
  done ;
  {parent = w_parent ; root = !root ; children = w_children}

let height (root, children) =
  let rec aux h = function
    | [] -> h+1
    | i :: l -> aux (max h (aux (-1) children.(i))) l
  in
  aux (-1) children.(root)

let routing_cost t =
  let n = float_of_int (Array.length t.children) in
  let rec aux total size x = function
    | [] -> total, size + 1
    | (y, c) :: l ->
        let w, s = aux 0. 0 y t.children.(y) in
        let s0 = float_of_int s in
        aux (total +. w +. s0 *. (n -. s0) *. c) (size + s) x l
  in
  fst (aux 0. 0 t.root t.children.(t.root))