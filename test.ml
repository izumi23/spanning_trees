(* open Owl

let () =
  Printf.printf "%f\n" (Maths.sin 1.) ;
  Print.print_array [|42; 43|] *)

let () =
  Random.self_init () ;
  let s = ref 0 in
  let n = 1000 in
  for i = 0 to n-1 do
    let x = Distrib.simul_poisson 10. in
    Printf.printf "%d " x ;
    s := !s + x
  done ;
  Printf.printf "\n%d\n" (!s/n)