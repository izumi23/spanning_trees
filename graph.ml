let matrix_to_array g =
  let n = Array.length g in
  let a = Array.make n [] in
  for i = 0 to n-1 do
    for j = i to n-1 do
      let w = g.(i).(j) in
      if w > 0. then (a.(i) <- (j, w) :: a.(i) ; a.(j) <- (i, w) :: a.(j))
    done
  done ;
  a

let matrix_to_list g =
  let n = Array.length g in
  let l = ref [] in
  for i = 0 to n-1 do
    for j = i to n-1 do
      let w = g.(i).(j) in
      if w > 0. then l := (i, j, w) :: !l
    done
  done ;
  List.rev !l

let array_to_list g =
  let n = Array.length g in
  let l = ref [] in
  for i = 0 to n-1 do
    let rec aux = function
      | [] -> ()
      | (j, p) :: q when j >= i -> l := (i, j, p) :: !l ; aux q
      | _ :: q -> aux q
    in
    aux g.(i)
  done ;
  List.rev !l


let g1_matrix = [|
  [|0.; 10.; 0.; 0.; 0.; 0.; 0.;|] ;
  [|10.; 0.; 4.; 0.; 0.; 5.; 1.;|] ;
  [|0.; 4.; 0.; 3.; 1.; 0.; 2.;|] ;
  [|0.; 0.; 3.; 0.; 4.; 5.; 0.;|] ;
  [|0.; 0.; 1.; 4.; 0.; 4.; 3.;|] ;
  [|0.; 5.; 0.; 5.; 4.; 0.; 0.;|] ;
  [|0.; 1.; 2.; 0.; 3.; 0.; 0.;|] ;
|]

let g1 = matrix_to_array g1_matrix

let hash x y =
  let pos = float_of_int (7 + x*x + y*y) in
  let z = pos *. 0.61803398874989484 in z -. (floor z)

let complete_array n =
  let g = Array.make n [] in
  let rec aux x y l =
    if y < 0 then l
    else if x = y then aux x (y-1) l
    else aux x (y-1) ((y, hash x y) :: l)
  in
  for x = 0 to n-1 do g.(x) <- aux x (n-1) [] done ;
  g

let complete_matrix n =
  let g = Array.make_matrix n n 0. in
  for x = 0 to n-1 do for y = 0 to n-1 do
    if x != y then g.(x).(y) <- hash x y
  done done ;
  g

let torus n =
  let g = Array.make (n*n) [] in
  let node origin (x, y) =
    let dest = n*((x+n) mod n) + ((y+n) mod n) in (dest, hash origin dest)
  in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      let origin = n*i + j in
      g.(origin) <- List.map (node origin) [i-1,j ; i,j-1 ; i,j+1; i+1,j]
    done
  done ;
  g

let merge_trees parent x y =
  let rec remonte l i =
    if parent.(i) = -1 then i :: l
    else remonte (i :: l) parent.(i)
  in
  let rec inverse = function
    | [] | [_] -> ()
    | i :: j :: l -> parent.(i) <- j ; inverse (j :: l)
  in
  inverse (remonte [] y) ;
  parent.(y) <- x

let unorient g =
  let n = Array.length g in
  let rec aux i = function
    | [] when i = n-1 -> ()
    | [] -> aux (i+1) g.(i+1)
    | (j, p) :: l -> if i < j then g.(j) <- (i, p) :: g.(j) ; aux i l
  in
  aux 0 g.(0)

let simul_complete n =
  let n0 = float_of_int n in
  let avgd = log n0 in
  let maxw = avgd /. n0 in
  let g = Array.make n [] in
  for i = 0 to n-1 do
    let lambda = (1. -. (float_of_int i) /. n0) *. avgd in
    let d = Distrib.simul_poisson lambda in
    for k = 0 to d-1 do
      let rec aux () =
        let j = i+1 + Random.int (n-i-1) in
        if List.exists (fun (y, p) -> y = j) g.(i) then aux ()
        else j
      in
      g.(i) <- (aux (), Random.float maxw) :: g.(i)
    done
  done ;
  unorient g ;
  g
