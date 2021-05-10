let usage_msg = "acm [-s][-r][-t][-seed] <graph_size>"

let results = ref false
let show_graph = ref false
let torus = ref false
let new_seed = ref false
let n = ref 4

(* let output_file = ref "" *)

let anon_fun graph_size =
  n := int_of_string graph_size

let speclist =
  [("-s", Arg.Set show_graph, "Show the graph"); 
   ("-r", Arg.Set results, "Give detailed results");
   ("-t", Arg.Set torus, "Make a torus graph instead of a complete one");
   ("-seed", Arg.Set new_seed, "Use a new random seed");
   (* ("-o", Arg.Set_string output_file, "Set output file name") *)
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !new_seed then Random.self_init () ;

  let f () = flush stdout in
  print_string "Compiled.\n" ; f () ;

  (* let g = Graph.complete_array 4000 in *)
  let g = if !torus then Graph.torus !n else Graph.complete_array !n in

  if !show_graph then (
    if !torus then Print.print_graph g
    else Print.print_matrix (Graph.complete_matrix !n) ;
  print_newline ()
  ) ;

  (* let print_results (s, t) =
    Printf.printf "%f, " s ; Print.print_list_pairs t ; print_newline () ;
    flush stdout 
  in *)

  let print_results (s, t, p) =
    if !results then (
      Printf.printf "%f, " s ; Print.print_list_pairs t ; 
      Print.print_array p ; print_newline ()
    )
    else Printf.printf "%f\n\n" s ;
    flush stdout 
  in
  (* let print_results (s, t) = Printf.printf "%f\n\n" s ; flush stdout in *)

  print_string "Graph constructed.\n\n" ; f () ;
  print_string "prim:\n" ; f () ; print_results (Prim.prim g) ;
  print_string "boruvka:\n" ; f () ; print_results (Boruvka.boruvka g) ;
  print_string "kruskal: " ; f () ; print_results (Kruskal.kruskal g) ;

  (* Print.print_list_pairs (List.rev (Aldousbroder.aldous_broder g)) *)
