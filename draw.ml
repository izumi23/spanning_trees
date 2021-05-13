let draw_graph_f g file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length g in
  let rec aux i = function
    | [] when i = n-1 -> ()
    | [] -> aux (i+1) g.(i+1)
    | (j, _) :: l -> p out "    %d -- %d;\n" i j ; aux i l
  in
  p out "strict graph T [bgcolor = \"gray30\"]{\n" ;
  aux 0 g.(0) ;
  p out "}\n" ;
  close_out_noerr out

let draw_tree_f parent file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length parent in
  p out "strict graph T [bgcolor = \"gray30\"]{\n" ;
  for i = 0 to n-1 do
    let j = parent.(i) in
    if j >= 0 then p out "    %d -- %d;\n" i j
  done ;
  p out "}\n" ;
  close_out_noerr out

let draw_graph g filename =
  Sys.chdir "graphviz" ;
  draw_graph_f g (filename ^ ".dot") ;
  let comd =
    Printf.sprintf "dot -Tpng %s.dot -o %s.png" filename filename
  in
  ignore (Sys.command comd) ;
  Printf.printf "Wrote graphviz/%s.dot and graphviz/%s.png\n\n" filename filename

let draw_tree g filename =
  Sys.chdir "graphviz" ;
  draw_tree_f g (filename ^ ".dot") ;
  let comd =
    Printf.sprintf "dot -Tpng %s.dot -o %s.png" filename filename
  in
  ignore (Sys.command comd) ;
  Printf.printf "Wrote graphviz/%s.dot and graphviz/%s.png\n\n" filename filename