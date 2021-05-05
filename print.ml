let print_list_pairs l =
  let rec aux = function
    | [] -> print_string "]"
    | [(x, y)] -> Printf.printf "(%d, %d)]\n" x y
    | (x, y) :: m -> Printf.printf "(%d, %d); " x y ; aux m
  in
  print_string "[" ; aux l

let print_array a =
  let n = Array.length a in
  Printf.printf "[|" ;
  for i = 0 to n-2 do Printf.printf "%d; " a.(i) done ;
  Printf.printf "%d|]\n" a.(n-1)

let print_list_triplets l =
  let rec aux = function
    | [] -> print_string "]"
    | [(x, y, z)] -> Printf.printf "(%d, %d, %.0f)]\n" x y z
    | (x, y, z) :: m -> Printf.printf "(%d, %d, %.0f); " x y z ; aux m
  in
  print_string "[" ; aux l

let print_array_triplets a =
  let n = Array.length a in
  Printf.printf "[|" ;
  for i = 0 to n-2 do
    let x, y, p = a.(i) in Printf.printf "(%d, %d, %f); " x y p
  done ;
  let x, y, p = a.(n-1) in Printf.printf "(%d, %d, %f)|]\n" x y p

let print_matrix a =
  let n = Array.length a in
  let print_line i =
    if i > 0 then print_string "  " ;
    Printf.printf "[|" ;
    for j = 0 to n-2 do Printf.printf "%.3f; " a.(i).(j) done ;
    Printf.printf "%.3f|]" a.(i).(n-1)
  in
  print_string "[|" ;
  for i = 0 to n-2 do print_line i ; print_string ";\n" done ;
  print_line (n-1) ;
  print_string "|]\n"

let print_graph g =
  let n = Array.length g in
  let print_line i =
    let rec aux = function
      | [] -> print_string "]"
      | [(x, y)] -> Printf.printf "(%d, %f)]" x y
      | (x, y) :: m -> Printf.printf "(%d, %f); " x y ; aux m
    in
    print_string "[" ; aux g.(i)
  in
  print_string "[|" ;
  for i = 0 to n-2 do 
    if i > 0 then print_string "  " ;
    print_line i ; print_string ";\n" ;
  done ;
  print_string "  " ; print_line (n-1) ;
  print_string "|]\n"