let usage_msg = "rhp [options] <graph_size>"

let torus = ref false
let new_seed = ref false
let m = ref 8
let n = ref 8
let iterations = ref 1000
let minitree = ref false
let sameends = ref false
let playing = ref false
let arg_position = ref 0
let delay = ref 0.

let anon_fun graph_size =
  let r = if !arg_position = 0 then m else n in
  r := int_of_string graph_size ;
  incr arg_position

let speclist = [
   ("-seed", Arg.Set new_seed, "Use a new random seed");
   ("-i", Arg.Set_int iterations, "Set a number of iterations");
   ("-same", Arg.Set sameends, "Path must have same ends as original");
   ("-game", Arg.Set playing, "Launch interactive game");
   ("-delay", Arg.Set_float delay, "Set delay for automatic moves");
  ]


let () =
  Arg.parse speclist anon_fun usage_msg ;

  if !new_seed then Random.self_init () ;

  let f () = flush stdout in
  print_string "Compiled.\n" ; f () ;

  let p = Markovpath.default_path !m !n in

  let i = ref 0 in
  
  while !i < !iterations do
    let nature = Random.int 2 in
    let dir = Random.int 4 in
    Markovpath.transition p nature dir ;
    if not !sameends || Markovpath.same_ends p then incr i  
  done ;

  if !playing then Game.play !m !n p !delay