let prim g =

  let n = Array.length g in
  let integre = Array.make n false in
  let pred = Array.make n (-1, max_float) in
  let q = Prioqueue.create n in
  let t = ref [] in
  let s = ref 0. in
  let parent = Array.make n (-1) in
  Prioqueue.add q 0 0. ;

  let rec explore_voisins x = function
    | [] -> ()
    | (y, c) :: l when not integre.(y) ->
        let c1 = snd pred.(y) in
        if c < c1 then (
          pred.(y) <- (x, c) ;
          if c1 = max_float then Prioqueue.add q y c
          else Prioqueue.update q y c
        ) ;
        explore_voisins x l
    | _ :: l -> explore_voisins x l
  in

  for i = 0 to n-1 do
    let x = Prioqueue.pop q in
    integre.(x) <- true ;
    parent.(x) <- fst (pred.(x)) ;
    if x > 0 then (t := (fst pred.(x), x) :: !t ; s := snd pred.(x) +. !s) ;
    explore_voisins x g.(x) ; 
  done ;
  !s, List.rev !t, parent

;;


(* let s, t = prim Graph.g1 in
Printf.printf "%f, " s ; Print.print_list_pairs t *)