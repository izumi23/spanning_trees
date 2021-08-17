let usage_msg = "stats [options] <values>"

let show_graph = ref false
let torus = ref false
let new_seed = ref false
let no_sim = ref false
let uniform = ref false
let nb_iterations = ref 1000
let values = ref []
let output_file = ref ""

let anon_fun n =
  values := int_of_string n :: !values

let speclist =
  [("-show", Arg.Set show_graph, "Show the graph"); 
    ("-t", Arg.Set torus, "Make a torus graph instead of a complete one");
    ("-seed", Arg.Set new_seed, "Use a new random seed");
    ("-u", Arg.Set uniform, "Find a uniform spanning tree");
    ("-o", Arg.Set_string output_file, "Set output file name");
    ("-i", Arg.Set_int nb_iterations, "Set the number of iterations");
    ("-nosim", Arg.Set no_sim, "Don't simulate the complete graph")
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !new_seed then Random.self_init () ;

  let x_axis = Array.of_list (List.rev !values) in
  let nb_values = Array.length x_axis in
  let h_axis = Array.make nb_values 0 in
  let h_axis_mean = Array.make nb_values 0. in
  let w_axis_mean = Array.make nb_values 0. in

  for k = 0 to nb_values - 1 do
    let size = x_axis.(k) in
    Printf.printf "Size %d: \t" size ; flush stdout ;

    for i = 0 to !nb_iterations - 1 do
      let weight, parent =
        if !torus then let n0 = int_of_float (sqrt (float_of_int size)) in
          if !uniform then 0., snd (Aldousbroder.aldous_broder (Graph.torus n0))
          else let w,_, p = Prim.prim (Graph.torus n0) in w, p
        else
          if !uniform then 0., snd (Aldousbroder.aldous_broder_complete size)
          else if !no_sim then let w,_, p = Prim.prim (Graph.complete size) in w, p
          else let w,_, p = Prim.prim (Graph.simul_complete size) in w, p
      in

      let h = Tree.height (Tree.construct_tree parent) in
      w_axis_mean.(k) <- w_axis_mean.(k) +. weight ;
      h_axis.(k) <- h_axis.(k) + h
    done ;

    w_axis_mean.(k) <- w_axis_mean.(k) /. float_of_int !nb_iterations ;
    h_axis_mean.(k) <- float_of_int h_axis.(k) /. float_of_int !nb_iterations ;
    if not !uniform then Printf.printf "weight %f \t" w_axis_mean.(k) ;
    Printf.printf "height %f\n" h_axis_mean.(k)  
  done ;

  if !output_file != "" then (
    let p = Printf.fprintf in
    Sys.chdir "statresults" ;
    let filename = !output_file ^ ".dat" in
    let out = open_out filename in
    for k = 0 to nb_values - 1 do
      if !uniform then p out "%d %f\n" x_axis.(k) h_axis_mean.(k)
      else p out "%d %f %f\n" x_axis.(k) w_axis_mean.(k) h_axis_mean.(k)
    done ;
    close_out_noerr out ;
    Printf.printf "Wrote statresults/%s\n" filename
  )