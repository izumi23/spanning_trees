let usage_msg = "stats [options] <lower_bound> <upper_bound> <step>"

let show_graph = ref false
let torus = ref false
let new_seed = ref false
let sim = ref false
let uniform = ref false
let lower_bound = ref 10000
let upper_bound = ref 100000
let step = ref 10000
let nb_args = ref 0
let output_file = ref ""

let anon_fun n =
  let field =
    if !nb_args = 0 then lower_bound
    else if !nb_args = 1 then upper_bound
    else step
  in
  incr nb_args ;
  field := int_of_string n

let speclist =
  [("-show", Arg.Set show_graph, "Show the graph"); 
    ("-t", Arg.Set torus, "Make a torus graph instead of a complete one");
    ("-seed", Arg.Set new_seed, "Use a new random seed");
    ("-u", Arg.Set uniform, "Find a uniform spanning tree");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !new_seed then Random.self_init () ;

  let nb_tests = (!upper_bound - !lower_bound) / !step + 1 in
  let x_axis = Array.make nb_tests 0 in
  let y_axis = Array.make nb_tests 0 in

  for k = 0 to nb_tests - 1 do
    let size = !lower_bound + k* !step in
    Printf.printf "Size %d: " size ; flush stdout ;

    let parent =
      if !torus then let n0 = int_of_float (sqrt (float_of_int size)) in
        if !uniform then snd (Aldousbroder.aldous_broder (Graph.torus n0))
        else let _,_, p = Prim.prim (Graph.torus n0) in p
      else
        if !uniform then snd (Aldousbroder.aldous_broder_complete size)
        else let _,_, p = Prim.prim (Graph.simul_complete size) in p
    in

    let h = Tree.height (Tree.construct_tree parent) in
    Printf.printf "height %d\n" h ;
    x_axis.(k) <- size ;
    y_axis.(k) <- h
  done ;

  if !output_file != "" then (
    let p = Printf.fprintf in
    Sys.chdir "statresults" ;
    let out = open_out !output_file in
    for k = 0 to nb_tests - 1 do
      p out "%d %d\n" x_axis.(k) y_axis.(k)
    done ;
    close_out_noerr out ;
    Printf.printf "Wrote statresults/%s\n" !output_file
  )