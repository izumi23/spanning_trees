let usage_msg = "rhp [options] <graph_size>"

let torus = ref false
let new_seed = ref false
let n = ref 4
let output_file = ref ""
let iterations = ref 1000
let minitree = ref false
let sameends = ref false

let anon_fun graph_size =
  n := int_of_string graph_size

let speclist = [
   ("-seed", Arg.Set new_seed, "Use a new random seed");
   ("-o", Arg.Set_string output_file, "Set output file name");
   ("-i", Arg.Set_int iterations, "Set a number of iterations");
   ("-same", Arg.Set sameends, "Path must have same ends as original");
  ]


let () =
  Arg.parse speclist anon_fun usage_msg ;

  if !new_seed then Random.self_init () ;

  let f () = flush stdout in
  print_string "Compiled.\n" ; f () ;

  let p = Markovpath.default_path !n in

  let i = ref 0 in
  
  while !i < !iterations do
    let nature = Random.int 2 in
    let dir = Random.int 4 in
    Markovpath.transition p nature dir ;
    if not !sameends || Markovpath.same_ends p then incr i  
  done ;

  if !output_file != "" then
    Plot.plot_tree p.parent !output_file

