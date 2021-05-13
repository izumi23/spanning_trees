let draw_graph g file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length g in
  let rec aux i = function
    | [] when i = n-1 -> ()
    | [] -> aux (i+1) g.(i+1)
    | (j, _) :: l -> p out "    %d -- %d;\n" i j ; aux i l
  in
  p out "graph T {\n" ;
  aux 0 g.(0) ;
  p out "}\n"

let draw_tree parent file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length parent in
  p out "graph T {\n" ;
  for i = 0 to n-1 do
    let j = parent.(i) in
    if j >= 0 then p out "    %d -- %d;\n" i j
  done ;
  p out "}\n"