(* open Owl

let () =
  Printf.printf "%f\n" (Maths.sin 1.) ;
  Print.print_array [|42; 43|] *)

let () =
  let g = Graph.simul_complete 20 in
  Print.print_graph g