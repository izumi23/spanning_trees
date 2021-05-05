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