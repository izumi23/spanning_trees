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
  Arg.parse speclist anon_fun usage_msg;

  if !new_seed then Random.self_init () ;
  print_int (Graph.routing_cost graph_ex) ; print_newline ()

