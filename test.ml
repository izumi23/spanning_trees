let f () = flush stdout in
print_string "Compiled.\n" ; f () ;

(* let a = Graph.complete_matrix 100 in
Print.print_matrix a ;
let g = Graph.matrix_to_array a in *)
(* let g = Graph.complete_array 4000 in *)
let g = Graph.torus 3 in

(* Print.print_graph g ; print_newline () ; *)

(* let print_results (s, t) =
  Printf.printf "%f, " s ; Print.print_list_pairs t ; print_newline () ;
  flush stdout 
in *)
(* let print_results (s, t) = Printf.printf "%f\n\n" s ; flush stdout in *)

print_string "Graph constructed.\n\n" ; f () ;
(* print_string "prim:\n" ; f () ; print_results (Prim.prim g) ;
print_string "boruvka:\n" ; f () ; print_results (Boruvka.boruvka g) ;
print_string "kruskal: " ; f () ; print_results (Kruskal.kruskal g) ; *)

Print.print_list_pairs (List.rev (Aldousbroder.aldous_broder g))
