let attributes = "    graph [bgcolor = gray10, overlap = false];
    node [style = filled, fillcolor = gray80, color = blue, shape = point];
    edge [color = blue];\n"

let attributes_small = "    graph [bgcolor = gray10, overlap = false];
    node [style = filled, fillcolor = gray80, color = blue, shape = circle, fontname = \"Arial\"];
    edge [color = blue];\n"          

let write_graph g file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length g in
  let rec aux i = function
    | [] when i = n-1 -> ()
    | [] -> aux (i+1) g.(i+1)
    | (j, _) :: l -> p out "    %d -- %d;\n" i j ; aux i l
  in
  p out "strict graph T {\n" ;
  p out "%s" (if n <= 100 then attributes_small else attributes) ;
  aux 0 g.(0) ;
  p out "}\n" ;
  close_out_noerr out

let write_tree parent file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length parent in
  p out "graph T {\n" ;
  p out "%s" (if n <= 100 then attributes_small else attributes) ;
  for i = 0 to n-1 do
    let j = parent.(i) in
    if j >= 0 then p out "    %d -- %d;\n" i j
  done ;
  p out "}\n" ;
  close_out_noerr out

let draw_graph g filename =
  Sys.chdir "graphviz" ;
  write_graph g (filename ^ ".dot") ;
  Printf.printf "Wrote graphviz/%s.dot\n" filename ; flush stdout ;
  let comd =
    Printf.sprintf "sfdp -Tpng %s.dot -o %s.png" filename filename
  in
  ignore (Sys.command comd) ;
  Printf.printf "Wrote graphviz/%s.png\n\n" filename

let draw_tree g filename =
  Sys.chdir "graphviz" ;
  write_tree g (filename ^ ".dot") ;
  Printf.printf "Wrote graphviz/%s.dot\n" filename ; flush stdout ;
  let comd =
    Printf.sprintf "sfdp -Tpng %s.dot -o %s.png" filename filename
  in
  ignore (Sys.command comd) ;
  Printf.printf "Wrote graphviz/%s.png\n\n" filename