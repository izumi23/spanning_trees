let usage_msg = "test [options] <graph_size>"

let torus = ref false
let new_seed = ref false
let n = ref 4
let output_file = ref ""
let iterations = ref 1000
let minitree = ref false

let anon_fun graph_size =
  n := int_of_string graph_size

let speclist = [
   ("-t", Arg.Set torus, "Make a torus graph instead of a complete one");
   ("-seed", Arg.Set new_seed, "Use a new random seed");
   ("-o", Arg.Set_string output_file, "Set output file name");
   ("-i", Arg.Set_int iterations, "Set a number of iterations");
   ("-m", Arg.Set minitree, "Start with the minimum spanning tree");
  ]


let () =
  Arg.parse speclist anon_fun usage_msg ;

  if !new_seed then Random.self_init () ;

  let f () = flush stdout in
  print_string "Compiled.\n" ; f () ;

  let g =
    if !torus then Graph.torus !n
    else Graph.complete_array !n 
  in

  print_string "Graph constructed.\n\n" ; f () ;

  let _, parent = 
    if !minitree then let _, e, p = Prim.prim g in e, p
    else Aldousbroder.aldous_broder g
  in

  if !minitree then print_string "Minimum spanning tree found.\n\n"
  else print_string "Uniform spanning tree found.\n\n" ;
  f () ;

  let t = Tree.construct_tree parent in

  Printf.printf "(0)   %.0f\n" (Tree.routing_cost g t) ; f () ;

  let step = !iterations / 100 in
  let weight = ref max_float in
  let sum = ref 0. in
  let mini = ref max_float in
  
  for i = 0 to !iterations - 1 do
    weight := Markovroute.markov_transition g !weight t ;
    if i >= !iterations / 2 then sum := !sum +. !weight ;
    mini := min !mini !weight ;
    if i mod step = step - 1 then (
      Printf.printf "(%d)   %.0f      min = %.0f\n" (i+1) !weight !mini ; f ()
    )
  done ;
  print_newline () ;

  let avg = !sum /. float_of_int (!iterations / 2) in
  Printf.printf "Second half average: %.0f\nMinimum: %.0f\n\n" avg !mini ;

  if !output_file != "" then
    if !torus then Plot.plot_tree parent !output_file
    else Draw.draw_tree parent !output_file

