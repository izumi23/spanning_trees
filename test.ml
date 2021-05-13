(* open Owl

let () =
  Printf.printf "%f\n" (Maths.sin 1.) ;
  Print.print_array [|42; 43|] *)

let usage_msg = "acu [options] <graph_size>"

let show_graph = ref false
let torus = ref false
let new_seed = ref false
let sim = ref false
let uniform = ref false
let n = ref 4
let output_file = ref ""

let anon_fun graph_size =
  n := int_of_string graph_size

let speclist =
  [("-show", Arg.Set show_graph, "Show the graph"); 
    ("-t", Arg.Set torus, "Make a torus graph instead of a complete one");
    ("-seed", Arg.Set new_seed, "Use a new random seed");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !new_seed then Random.self_init () ;

  let g =
    if !torus then Graph.torus !n
    else Graph.complete_array !n 
  in
  
  Draw.draw_graph g "t4"