let usage_msg = "test [options] <graph_size>"

let results = ref false
let show_graph = ref false
let torus = ref false
let new_seed = ref false
let sim = ref false
let n = ref 4
let output_file = ref ""
let iterations = ref 1000

let anon_fun graph_size =
  n := int_of_string graph_size

let speclist =
  [("-show", Arg.Set show_graph, "Show the graph"); 
   ("-r", Arg.Set results, "Give detailed results");
   ("-t", Arg.Set torus, "Make a torus graph instead of a complete one");
   ("-seed", Arg.Set new_seed, "Use a new random seed");
   ("-sim", Arg.Set sim, "Simulate a complete graph");
   ("-simul", Arg.Set sim, "Simulate a complete graph");
   ("-o", Arg.Set_string output_file, "Set output file name");
   ("-i", Arg.Set_int iterations, "Set a number of iterations")
  ]
let graph_ex = 9, [|
  [];
  [0,1; 4,1];
  [];
  [];
  [2,1 ;3,1];
  [1,2];
  [];
  [6,1];
  [7,1];
  [5,1; 8,1]
|]


let () =
  Arg.parse speclist anon_fun usage_msg ;

  if !new_seed then Random.self_init () ;

  let f () = flush stdout in
  print_string "Compiled.\n" ; f () ;

  let g =
    if !torus then Graph.torus !n
    else if !sim then Graph.complete_array 1
    else Graph.complete_array !n 
  in

  if !show_graph then (
    if !torus then Print.print_graph g
    else Print.print_matrix (Graph.complete_matrix !n) ;
  print_newline ()
  ) ;

  let print_results (t, w_parent) =
    Print.print_list_pairs t ; 
    Print.print_array (Array.map fst w_parent) ; print_newline () ;
    flush stdout 
  in

  print_string "Graph constructed.\n\n" ; f () ;
  let u, w_parent = Aldousbroder.aldous_broder_weighted g
  in
  print_string "Uniform spanning tree found.\n\n" ; flush stdout ;
  if !results then print_results (u, w_parent) ;

  let t = Tree.construct_weighted_tree w_parent in

  for i = 0 to !iterations - 1 do
    let res = Markovroute.markov_transition g max_float t in
    Printf.printf "%f\n" res
  done ;
  print_newline () ;

  if !output_file != "" then
    let p = Array.map fst t.parent in
    if !torus then Plot.plot_tree p !output_file
    else Draw.draw_tree p !output_file
