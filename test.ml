let g = Graph.complete_array 4 in
let print_results (s, t) = Printf.printf "%f, " s ; Print.print_list_pairs t in
List.iter print_results [Prim.prim g ; Kruskal.kruskal g ; Boruvka.boruvka g]